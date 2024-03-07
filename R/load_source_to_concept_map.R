#' @title Load source_to_concept_map table from Usagi mapping Files.
#'
#' @description This function loads the source_to_concept_map table using the mapping
#' files created with the OHDSI Usagi (https://github.com/OHDSI/Usagi) application.
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
#' stcm <- load_source_to_concept_map()
#' stcm
#'
#' @export
load_source_to_concept_map <- function() {
  log_info("Loading source_to_concept_map table...")
  connection_details <- get_connection_details("cdm")

  cdm_schema <- amstel_env$config$databases$cdm$schema
  conn <- DatabaseConnector::connect(connection_details)
  on.exit(DatabaseConnector::disconnect(conn))
  stcm_table_name <- get_server_tablename('source_to_concept_map', conn)

  log_info(paste0("Deleting all records from table: ", stcm_table_name))
  sql <- "TRUNCATE @table_name;"
  DatabaseConnector::renderTranslateExecuteSql(
    progressBar = interactive(),
    connection = conn,
    sql = sql,
    table_name = paste0(cdm_schema, ".", stcm_table_name)
  )

  # add (redundant) target_vocabulary_id to data frame (non-null constraint)
  log_info("Reading vocabulary_id from concept vocabulary...")

  sql <- "SELECT concept_id, vocabulary_id FROM @table_name;"
  concept_table_name <- get_server_tablename('concept', conn)
  concept <- DatabaseConnector::renderTranslateQuerySql(
    progressBar = interactive(),
    connection = conn,
    sql = sql,
    table_name = paste0(cdm_schema, ".", concept_table_name),
  )
  # convert column names to lower case
  names(concept) <- tolower(names(concept))

  log_info("Processing Usagi mapping files...")

  # The list of local (AmsterdamUMCdb) vocabularies is stored in the
  # local_vocabularies.yaml file in the mappings folder. A vocabulary
  # can contain a sub-vocabulary based on the mapping type.
  local_vocabularies <- yaml::read_yaml(
    file=file.path(amstel_env$config$data$mappings,
                   paste0("local_vocabularies.yaml")))

  maps_list <- list()

  for (vocabulary in names(local_vocabularies)) {
    log_info(paste0("- ", vocabulary))

    if (vocabulary == 'admissions_destination') {
      concepts <- load_usagi_concepts("listitems_value")
      concepts <- concepts %>% dplyr::filter(.data$`ADD_INFO:itemid` == 10472)
    }
    else {
      concepts <- load_usagi_concepts(vocabulary)
    }

    # add empty column that contains original Dutch item name for most tables
    # but missing from admissions
    if(!'ADD_INFO:source_concept' %in% names(concepts)) {
      concepts <- concepts %>% tibble::add_column('ADD_INFO:source_concept' = NA)
    }

    # iterate mapping types for each vocabulary
    mapping_types <- local_vocabularies[[vocabulary]]
    for (mapping_type in names(mapping_types)) {

      voc_id <- mapping_types[[mapping_type]]
      log_info(paste0("  * '", voc_id, "' for ", mapping_type, " mapping"))

      concepts_by_mapping_type <- concepts %>%
        dplyr::filter(.data$mappingStatus == 'APPROVED',
                      .data$mappingType == mapping_type,
                      !is.na(.data$sourceCode))

      if (nrow(concepts_by_mapping_type) == 0) next

      source_to_concept_map <- concepts_by_mapping_type %>%
        dplyr::left_join(concept, by=c("conceptId" = "concept_id")) %>%
        dplyr::mutate(
          sourceCode = stringr::str_trunc(
            as.character(.data$sourceCode), 50, "right", ellipsis = ''), # max length = 50
          source_concept_id = 0,
          source_vocabulary_id = voc_id,
          source_code_description = dplyr::case_when(
            !is.na(.data$`ADD_INFO:source_concept`) ~ .data$`ADD_INFO:source_concept`,
            .default = .data$sourceCode),
          valid_start_date = as.Date('1970-01-01'),
          valid_end_date = as.Date('2099-12-31'),
          invalid_reason = NA
        ) %>%
        dplyr::select(
          source_code = .data$sourceCode,
          .data$source_concept_id,
          .data$source_vocabulary_id,
          .data$source_code_description,
          target_concept_id = .data$conceptId,
          target_vocabulary_id = .data$vocabulary_id,
          .data$valid_start_date,
          .data$valid_end_date,
          .data$invalid_reason
        )

      # wrap the data frame in a list to add as a single element
      maps_list <- append(maps_list, list(source_to_concept_map))
    }

  }

  source_to_concept_map <- dplyr::bind_rows(maps_list)

  # The SOURCE_TO_CONCEPT_MAP table currently does not support mapping a
  # single source_code to multiple target concepts while assigning
  # multiple dimensions to those target concepts
  # i.e. Drug -> Ingredient 1 > Quantity
  #           -> Ingredient 2 > Quantity
  #
  # To allow these multi-level mappings in the SOURCE_TO_CONCEPT_MAP table, each
  # source_code will be assigned to multiple source vocabularies when required.
  # To allow multiple Ingredients to be assigned separate Quantities, the order
  # of the mapping in the table will determine what quantity belongs to what
  # ingredient.
  # Since SQL does not have an inherent ordering and the SOURCE_TO_CONCEPT_MAP
  # does not have a unique incremental identifier, we increment the
  # `valid_start_date` to preserve order.

  source_to_concept_map$valid_start_date <-
    seq(as.Date('1970-01-01'),
        length.out = nrow(source_to_concept_map),
        by = 1)

  # save source_to_concept_map
  log_info("Saving source_to_concept_map.csv in mappings folder...")
  stcm_file <- file.path(amstel_env$config$data$mappings,
                         "source_to_concept_map.csv")
  source_to_concept_map %>% readr::write_csv(
    file=stcm_file,
    na = "",
    quote = "needed")

  log_info(paste0("Loading mapping records into table: ", stcm_table_name))
  suppressWarnings({
    DatabaseConnector::insertTable(
      progressBar = interactive(),
      connection = conn,
      tableName = paste0(cdm_schema, ".", stcm_table_name),
      data = source_to_concept_map,
      dropTableIfExists = FALSE,
      createTable = FALSE,
      bulkLoad = FALSE
    )
  })

  log_info("Success: source_to_concept_map loaded.")

  return(source_to_concept_map)

}
