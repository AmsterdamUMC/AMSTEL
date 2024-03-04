#' Create the Common Data Model Clinical Data tables
#'
#' @description
#' This function creates the Common Data Model Clinical Data tables, e.g.
#' all tables with the exception of the tables related to the Standard
#' Vocabularies and Standard Concepts.
#'
#' @details
#' This function requires a configured `config.yaml` file created using
#' `amstel::create_config()`. It will use the connection configuration specified
#' in the `cdm` section to create WebAPI tables.
#'
#' @export
#'
#' @examples
#' create_data_tables()
create_data_tables <- function() {
  connection_details <- get_connection_details("cdm")
  conn <- DatabaseConnector::connect(connection_details)
  on.exit(DatabaseConnector::disconnect(conn))

  sql <- load_sql('create_data_tables.sql')
  DatabaseConnector::renderTranslateExecuteSql(
    connection = conn,
    sql = sql,
    cdm_schema = amstel_env$config$databases$cdm$schema
  )
}
