#' Connect to database
#'
#' @description
#' Connects to the AmsterdamUMCdb or CDM database based on `config.yaml` in
#' the user config folder.
#'
#' @param database Database to use, either "amsterdamumcdb",  "cdm", "results",
#' or "temp".
#' Default: "cdm"
#'
#' @param default_schema Default schema to use as the search path. By default,
#' this will be set to the value of `schema` in the configuration section
#' of `database`
#'
#' @return DatabaseConnector Connection object
#' @export
#'
#' @examples
#' conn <- connect()
#' conn <- connect("amsterdamumcdb")
connect <- function(database = "cdm", default_schema = "") {
  connection_details <- get_connection_details(database)
  conn <- DatabaseConnector::connect(connection_details)

  # set the default schema
  sql <- "SET SCHEMA '@schema';"
  schema <- if (default_schema != "")
    default_schema else amstel_env$config$database[[database]]$schema

  DatabaseConnector::renderTranslateExecuteSql(
    progressBar = interactive(),
    connection = conn,
    sql = sql,
    schema = schema,
  )
  return(conn)
}

#' Get case-sensitive table name on server
#'
#' @description
#' Since some DBMS are case-sensitive use the names of the tables currently
#' stored in the database to prevent creating new ones with a different case
#'
#' @param tablename Name of the table to check
#' @param conn DatabaseConnector Connection object
#'
#' @return String server table name
#' @export
#'
#' @examplesIf has_cdm_environment()
#' conn <- connect("cdm")
#' table <- get_server_tablename("PROCEDURE_OCCURRENCE", conn)
#' table
get_server_tablename <- function(tablename, conn) {
  server_tables <- DBI::dbListTables(conn)
  server_tablename <- server_tables[
    match(strsplit(tolower(tablename), "[.]")[[1]][1], tolower(server_tables))
  ]
  return(server_tablename)
}
