#' @title Alters PostgreSQL System settings.
#'
#' @description This function alters the PostgreSQL settings to reduce house
#' keeping while bulk inserting records.
#'
#' @details
#' This function requires a configured `config.yaml` file created using
#' `amstel::create_config()`.
#'
#' These settings require a server restart.
#'
#'@export

alter_system_settings <- function(defaults = FALSE) {
  log_info("Altering PostgreSQL system settings...")
  connection_details <- get_connection_details("cdm")
  conn <- DatabaseConnector::connect(connection_details)
  on.exit(DatabaseConnector::disconnect(conn))

  if (defaults == FALSE) {
    sql_file = 'alter_system_optimized.sql'
  }
  else {
    sql_file = 'alter_system_defaults.sql'
  }

  sql <- load_sql(sql_file)
  DatabaseConnector::renderTranslateExecuteSql(
    connection = conn,
    sql = sql
  )

  log_info("Please restart the server for the settings to take effect.")

}
