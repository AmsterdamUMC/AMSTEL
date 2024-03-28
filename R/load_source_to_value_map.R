#' @title Load source_to_value_map table from SQL file.
#'
#' @description Since Usagi removes mappings to invalid concepts on loading
#' (e.g. after importing a new vocabulary with deprecated concepts),
#' our previous method to store quantities for Drug and other domains will
#' fail since many numeric concepts have been deprecated.
#' This function will load migrated quantity data from the
#' `insert_source_to_value_map.sql` file to the new source_to_value table.
#'
#' @details
#' This function requires a configured `config.yaml` file created using
#' `amstel::create_config()`. It will use the connection configuration specified
#' in the `cdm` section to load the source_to_concept tables in the CDM by reading the
#' Usagi csv files in the `mappings` folder.
#'
#' This function assumes `amstel::create_cdm_tables()` and
#' `amstel::load_vocabulary_tables()` have already been run.
#'
#' @examples
#' load_source_to_value_map()
#'
#' @export
load_source_to_value_map <- function() {
  log_info("Loading source_to_value_map table...")
  connection_details <- get_connection_details("cdm")

  cdm_schema <- amstel_env$config$databases$cdm$schema
  conn <- DatabaseConnector::connect(connection_details)
  on.exit(DatabaseConnector::disconnect(conn))
  stvm_table_name <- get_server_tablename('source_to_value_map', conn)

  log_info(paste0("Deleting source_to_value_map table: ", stvm_table_name))
  sql <- "DROP TABLE IF EXISTS @table_name;"
  DatabaseConnector::renderTranslateExecuteSql(
    progressBar = interactive(),
    connection = conn,
    sql = sql,
    table_name = paste0(cdm_schema, ".", stvm_table_name)
  )

  log_info(paste0("Creating source_to_value_map table: ", stvm_table_name))
  sql <- load_sql('create_source_to_value_map_table.sql')
  DatabaseConnector::renderTranslateExecuteSql(
    progressBar = interactive(),
    connection = conn,
    sql = sql,
    cdm_schema = amstel_env$config$databases$cdm$schema
  )

  stvm_file <- file.path(amstel_env$config$data$mappings,
                         "source_to_value_map.csv")
  source_to_value_map <- readr::read_csv(file = stvm_file,
                                         show_col_types = FALSE)

  log_info(paste0("Loading mapping records into table: ", stvm_table_name))
  suppressWarnings({
    DatabaseConnector::insertTable(
      progressBar = interactive(),
      connection = conn,
      tableName = paste0(cdm_schema, ".", stvm_table_name),
      data = source_to_value_map,
      dropTableIfExists = FALSE,
      createTable = FALSE,
      bulkLoad = FALSE
    )
  })

  log_info("Success: source_to_value_map loaded.")

  return(source_to_value_map)

}
