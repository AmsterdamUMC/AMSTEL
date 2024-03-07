#' Create Common Data Model tables
#'
#' @param use_cdm_package
#' Specifies whether the OHDSI CommonDataModel::executeDdl function will be used
#' to execute the DDL (Data Definition Language) or the internal functions that
#' use SQL code that that allow conditional creation of the tables.
#' Default: FALSE
#'
#' @description
#' This function creates the required CDM tables using the `executeDdl` function
#' from the `OHDSI/CommonDataModel` package.
#'
#' @details
#' This function requires a configured `config.yaml` file created using
#' `amstel::create_config()`. It will use the connection configuration specified
#' in the `cdm` section to create the Common Data Model tables using either the
#' `amstel::create_data_tables` and `amstel::create_vocabulary_tables` functions
#' or the `executeDdl` function from the `OHDSI/CommonDataModel` package.
#'
#' @export
#'
#' @examplesIf has_example_environment()
#' create_cdm_tables()
create_cdm_tables <- function(use_cdm_package = FALSE) {
  # install CommonDataModel package from GitHub
  devtools::install_github("OHDSI/CommonDataModel")

  connection_details <- get_connection_details("cdm")

  # creates the schema if it does not exist
  conn <- DatabaseConnector::connect(connection_details)
  on.exit(DatabaseConnector::disconnect(conn))
  sql <- "CREATE SCHEMA IF NOT EXISTS @cdm_schema"
  sql <- SqlRender::render(sql, cdm_schema = amstel_env$config$databases$cdm$schema)
  sql <- SqlRender::translate(sql, targetDialect = connection_details$dbms)
  DatabaseConnector::executeSql(
    progressBar = interactive(),
    connection = conn,
    sql = sql
    )

  # The SQL code is generated using the CommonDataModel package. However,
  # since this does not allow conditional creation
  # (CREATE ... IF NOT EXISTS) the generated SQL code has been modified for
  # this requirement.
  if(use_cdm_package == TRUE) {
    CommonDataModel::executeDdl(
      connectionDetails = connection_details,
      cdmVersion        = "5.4",
      cdmDatabaseSchema = amstel_env$config$databases$cdm$schema,
      executeDdl        = TRUE,
      executePrimaryKey = TRUE,
      executeForeignKey = FALSE # prevent constraints being applied while loading tables
    )
  }
  else {
    create_data_tables()
    create_vocabulary_tables()
  }
}
