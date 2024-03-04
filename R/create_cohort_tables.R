#' Create the Common Data Model Results Cohort Tables
#'
#' @description
#' This function creates the Common Data Model Cohort tables, than can be used
#' to define cohorts based on criteria using standard concepts. An example cohort
#' has been created to select patients receiving mechanical ventilation
#'
#' @details
#' This function requires a configured `config.yaml` file created using
#' `amstel::create_config()`. It will use the connection configuration specified
#' in the `cdm` section to create WebAPI tables.
#'
#' @export
#'
#' @examples
#' create_cohort_tables()
create_cohort_tables <- function() {
  connection_details <- get_connection_details("cdm")
  conn <- DatabaseConnector::connect(connection_details)
  on.exit(DatabaseConnector::disconnect(conn))

  sql <- load_sql('create_cohort_tables.sql')
  DatabaseConnector::renderTranslateExecuteSql(
    connection = conn,
    sql = sql,
    cdm_schema = amstel_env$config$databases$cdm$schema,
    cdm_results_schema = amstel_env$config$databases$results$schema
  )
}
