#' Create OHDSI WebAPI tables
#'
#' @description
#' This function creates the required OHDSI WebAPI tables.
#'
#' @details
#' This function requires a configured `config.yaml` file created using
#' `amstel::create_config()`. It will use the connection configuration specified
#' in the `results` section to create WebAPI tables. WebAPI requires that
#' `amstel::etl()` (or the ETL background script) and `amstel::achilles()`
#' have already been run.
#'
#' @export
create_webapi_tables <- function() {
  connection_details <- get_connection_details("cdm")
  conn <- DatabaseConnector::connect(connection_details)
  on.exit(DatabaseConnector::disconnect(conn))

  log_info("Creating WebAPI tables...")
  sql <- load_sql('create_webapi_tables.sql')
  DatabaseConnector::renderTranslateExecuteSql(
    progressBar = interactive(),
    connection = conn,
    sql = sql,
    cdm_schema = amstel_env$config$databases$cdm$schema,
    cdm_results_schema = amstel_env$config$databases$results$schema
  )

  log_info("Creating Count tables...")
  sql <- load_sql('create_concept_count_tables.sql')
  DatabaseConnector::renderTranslateExecuteSql(
    progressBar = interactive(),
    connection = conn,
    sql = sql,
    cdm_schema = amstel_env$config$databases$cdm$schema,
    cdm_results_schema = amstel_env$config$databases$results$schema
  )

  log_info("Adding AmsterdamUMCdb CDM to WebAPI source...")
  sql <- load_sql('insert_webapi_source.sql')
  connection_string = paste0(
    'jdbc:',
    connection_details$dbms,
    '://',
    connection_details$server(),
    '?user=',
    connection_details$user(),
    '&password=',
    connection_details$password()
  )

  DatabaseConnector::renderTranslateExecuteSql(
    progressBar = interactive(),
    connection = conn,
    sql = sql,
    cdm_source_name = amstel_env$config$metadata$cdm_source_name,
    cdm_source_abbreviation = amstel_env$config$metadata$cdm_source_abbreviation,
    cdm_connection_string = connection_string,
    cdm_dbms = amstel_env$config$databases$cdm$dbms,
    cdm_schema = amstel_env$config$databases$cdm$schema,
    cdm_results_schema = amstel_env$config$databases$results$schema,
    cdm_temp_schema = amstel_env$config$databases$temp$schema
  )

  # drops the cache tables in WebAPI to allow updated Achilles results to show
  # up in ATLAS
  log_info("Dropping WebAPI cache tables...")
  sql <- load_sql('drop_webapi_cache.sql')
  DatabaseConnector::renderTranslateExecuteSql(
    progressBar = interactive(),
    connection = conn,
    sql = sql
  )
}
