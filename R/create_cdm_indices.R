#' @title Create Indices on Common Data Model Tables.
#'
#' @description This function creates indices for the tables in the CDM.
#'
#' @details
#' This function requires a configured `config.yaml` file created using
#' `amstel::create_config()`. It will use the connection configuration specified
#' in the `cdm` section to create the indices for the tables of the Common Data Model.
#'
#' This function assumes `amstel::create_cdm_tables()` has been run successfully.
#'
#'@export

create_cdm_indices <- function() {

    log_info("Creating Indices on CDM Tables....")

    connection_details <- get_connection_details("cdm")
    conn <- DatabaseConnector::connect(connection_details)
    on.exit(DatabaseConnector::disconnect(conn))

    index_sql_file <- CommonDataModel::writeIndex(
        targetDialect     = connection_details$dbms,
        cdmVersion        = " 5.4",
        cdmDatabaseSchema = amstel_env$config$databases$cdm$schema,
        outputfolder      = tempdir()
    )

    index_ddl <- SqlRender::readSql(paste0(tempdir(), "/", index_sql_file))
    conn <- DatabaseConnector::connect(connection_details)
    DatabaseConnector::executeSql(
      progressBar = interactive(),
      connection = conn,
      sql = index_ddl
      )
    log_info("Index Creation Complete.")

}
