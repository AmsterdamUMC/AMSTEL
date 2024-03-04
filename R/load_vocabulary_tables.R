#' @title Load Vocabulary Tables From CSV Files.
#'
#' @description This function loads the vocabulary tables with csv files downloaded
#' from OHDSI Athena (https://athena.ohdsi.org/vocabulary/list).
#'
#' @details
#' This function requires a configured `config.yaml` file created using
#' `amstel::create_config()`. It will use the connection configuration specified
#' in the `cdm` section to create the Vocabulary tables in the CDM by reading the
#' vocabulary csv files downloaded from the Athena from the path as
#' specified in the `data` section.
#'
#' This function assumes `amstel::create_cdm_tables()` has already been run.
#'
#'
#'@export
load_vocabulary_tables <-
  function()
  {
    log_info("Loading vocabulary tables...")
    connection_details <- get_connection_details("cdm")

    cdm_schema <- amstel_env$config$databases$cdm$schema
    vocabulary_path <- amstel_env$config$data$vocabulary

    csv_list <- c(
      "concept.csv",
      "vocabulary.csv",
      "concept_ancestor.csv",
      "concept_relationship.csv",
      "relationship.csv",
      "concept_synonym.csv",
      "domain.csv",
      "concept_class.csv",
      "drug_strength.csv"
    )

    file_list <- list.files(vocabulary_path)

    # create a file list based on the order above using
    # case insensitive matching
    ordered_file_list = c()

    for (csv in csv_list) {
      matched <- match(csv, tolower(file_list))
      ordered_file_list <- c(ordered_file_list, file_list[matched])
    }

    # file_list <- file_list[which(tolower(file_list) %in% csv_list)]
    file_list <- ordered_file_list

    conn <- DatabaseConnector::connect(connection_details)
    on.exit(DatabaseConnector::disconnect(conn))

    if (connection_details$dbms == "postgresql") {
      # uses PostgreSQL specific driver to improve performance of
      # copying large data frame to a PostgreSQL database table
      conn_pg <- DBI::dbConnect(RPostgres::Postgres(),
                                dbname = strsplit(connection_details$server(), "/")[[1]][2],
                                host = strsplit(connection_details$server(), "/")[[1]][1],
                                port = connection_details$port(),
                                user = connection_details$user(),
                                password = connection_details$password())
    }

    for (csv in file_list) {
      # retrieves the table on the server that matches the current csv file with
      # case-insensitive matching
      server_table_name <- get_server_tablename(csv, conn)
      log_info(paste0("Working on file ", paste0(vocabulary_path, "/", csv, " for table ", server_table_name)))

      log_info(" - reading file ")
      vocabulary_table <-
        data.table::fread(
          file = paste0(vocabulary_path, "/", csv),
          quote="", # assumes no fields are quoted (e.g. SQL Server default)
          stringsAsFactors = FALSE,
          header = TRUE,
          sep = "\t", # tab delimited
          na.strings = ""
        )

      if (tolower(csv) == "concept.csv" ||
          tolower(csv) == "concept_relationship.csv" ||
          tolower(csv) == "drug_strength.csv") {
        log_info(" - handling dates")
        vocabulary_table$valid_start_date <-
          as.Date(as.character(vocabulary_table$valid_start_date), "%Y%m%d")
        vocabulary_table$valid_end_date   <-
          as.Date(as.character(vocabulary_table$valid_end_date), "%Y%m%d")
        vocabulary_table <- dplyr::tibble(vocabulary_table)
      }

      log_info(" - type converting")
      vocabulary_table <- readr::type_convert(df = vocabulary_table,
                                        col_types = readr::cols(),
                                        na = c("")) %>%
        dplyr::tibble()

      if (tolower(csv) == "drug_strength.csv") {
        vocabulary_table <- vocabulary_table %>%
          dplyr::mutate_at(
            dplyr::vars(
              "amount_value",
              "amount_unit_concept_id",
              "numerator_value",
              "numerator_unit_concept_id",
              "denominator_value",
              "denominator_unit_concept_id",
              "box_size"
            ),
            ~ replace(., is.na(.), 0)
          )
      }

      log_info(" - deleting all records from table")
      sql <- "DELETE FROM @table_name;"
      DatabaseConnector::renderTranslateExecuteSql(
        connection = conn,
        sql = sql,
        table_name = paste0(cdm_schema, ".", server_table_name)
      )

      chunk_size <- 1e7
      n_rows_table <- nrow(vocabulary_table)
      n_chunks <-
        ceiling(x = n_rows_table / chunk_size)

      log_info(
        paste0(
          " - uploading ",
          n_rows_table,
          " rows of data in ",
          n_chunks,
          " chunks."
        )
      )

      start_row <- 1
      for (j in (1:n_chunks)) {
        if (n_rows_table >= start_row) {
          max_rows <- min(n_rows_table,
                         start_row + chunk_size)
          chunk <- vocabulary_table[start_row:max_rows, ]
          log_info(
            paste0(
              " - chunk uploading started on ",
              Sys.time(),
              " for rows ",
              start_row,
              " to ",
              max_rows
            )
          )
          if (connection_details$dbms == "postgresql") {
            log_info(" * using PostgreSQL COPY FROM STDIN")
            RPostgres::dbWriteTable(conn_pg,
                                    name = DBI::Id(
                                      schema = cdm_schema,
                                      table = server_table_name
                                      ),
                                    value = chunk,
                                    copy = TRUE, # enables COPY FROM STDIN
                                    append = TRUE
                                    )
          }
          else {
            suppressWarnings({
              DatabaseConnector::insertTable(
                connection = conn,
                tableName = paste0(cdm_schema, ".", server_table_name),
                data = chunk,
                dropTableIfExists = FALSE,
                createTable = FALSE,
                bulkLoad = FALSE,
                progressBar = TRUE
              )
            })
          }
          start_row <- max_rows + 1
        }
      }
      log_info(" - Success")
    }
  }
