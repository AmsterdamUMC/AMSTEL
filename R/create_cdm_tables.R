#' Create Common Data Model tables
#'
#' @description
#' This function creates the required CDM tables using the `executeDdl` function
#' from the `OHDSI/CommonDataModel` package.
#'
#' @details
#' This function requires a configured `config.yaml` file created using
#' `amstel::create_config()`. It will use the connection configuration specified
#' in the `cdm` section to create the Common Data Model tables using the
#' `executeDdl` function from the `OHDSI/CommonDataModel` package.
#'
#' @export
#'
#' @examples
#' create_cdm_tables()
create_cdm_tables <- function() {
  # install CommonDataModel package from GitHub
  devtools::install_github("OHDSI/CommonDataModel")

  connection_details <- get_connection_details("cdm")

  # creates the schema if it does not exist
  conn <- DatabaseConnector::connect(connection_details)
  on.exit(DatabaseConnector::disconnect(conn))
  sql <- "CREATE SCHEMA IF NOT EXISTS @cdm_schema"
  sql <- SqlRender::render(sql, cdm_schema = amstel_env$config$databases$cdm$schema)
  sql <- SqlRender::translate(sql, targetDialect = connection_details$dbms)
  DatabaseConnector::executeSql(conn, sql)

  CommonDataModel::executeDdl(
    connectionDetails = connection_details,
    cdmVersion        = "5.4",
    cdmDatabaseSchema = amstel_env$config$databases$cdm$schema,
    executeDdl        = TRUE,
    executePrimaryKey = TRUE,
    executeForeignKey = FALSE # prevent constraints being applied while loading tables
  )
}
