#' @title Create Indices on Common Data Model clinical data tables.
#'
#' @description This function creates indices for the
#' clinical data tables in the CDM.
#'
#' @details
#' This function requires a configured `config.yaml` file created using
#' `amstel::create_config()`. It will use the connection configuration specified
#' in the `cdm` section to create the indices for the tables of the
#' Common Data Model.
#'
#' This function assumes `amstel::create_cdm_tables()` and
#' `amstel::load_cdm_clinical_data()` have been run successfully.
#'
#'@export
index_data_tables <- function() {
  log_info("Creating Indices on CDM clinical data tables...")

  connection_details <- get_connection_details("cdm")
  conn <- DatabaseConnector::connect(connection_details)
  on.exit(DatabaseConnector::disconnect(conn))

  sql <- load_sql('index_data_tables.sql')
  DatabaseConnector::renderTranslateExecuteSql(
    progressBar = interactive(),
    connection = conn,
    sql = sql,
    cdmDatabaseSchema = amstel_env$config$databases$cdm$schema
  )
  log_info("Indices creation for clinical data tables Complete.")
}
