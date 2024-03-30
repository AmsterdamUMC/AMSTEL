#' @title Apply corrections
#'
#' @description This function applies corrections to the clinical data
#' based on issues identified using DataQualityDashboard.
#'
#' @details
#' This function executes the [`apply_corrections.sql`](https://github.com/AmsterdamUMC/AMSTEL/blob/main/inst/sql/apply_corrections.sql)
#' file. For a list of applied corrections, see the comments in this file.
#'
#' In order for this function to work, the CDM instance already needs to be
#' contain clinical data (e.g. after `amstel::load_data_tables`).
#'
#'@export
apply_corrections <- function() {

  log_info("Applying data corrections to CDM clinical data")

  connection_details <- get_connection_details("cdm")
  conn <- DatabaseConnector::connect(connection_details)
  on.exit(DatabaseConnector::disconnect(conn))

  sql <- load_sql('apply_corrections.sql')
  DatabaseConnector::renderTranslateExecuteSql(
    progressBar = interactive(),
    connection = conn,
    sql = sql,
    cdm_schema = amstel_env$config$databases$cdm$schema
  )
  log_info("Corrections: complete.")
}
