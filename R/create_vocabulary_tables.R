#' Create the Common Data Model Vocabulary tables
#'
#' @description
#' This function creates the Common Data Model Standard Vocabularies and
#' Standard Concepts tables.
#'
#' @details
#' This function requires a configured `config.yaml` file created using
#' `amstel::create_config()`. It will use the connection configuration specified
#' in the `cdm` section to create WebAPI tables.
#'
#' @export
#'
#' @examplesIf has_example_environment()
#' create_vocabulary_tables()
create_vocabulary_tables <- function() {

  log_info("Creating CDM vocabulary tables...")

  connection_details <- get_connection_details("cdm")
  conn <- DatabaseConnector::connect(connection_details)
  on.exit(DatabaseConnector::disconnect(conn))

  sql <- load_sql('create_vocabulary_tables.sql')
  DatabaseConnector::renderTranslateExecuteSql(
    progressBar = interactive(),
    connection = conn,
    sql = sql,
    cdm_schema = amstel_env$config$databases$cdm$schema
  )
}
