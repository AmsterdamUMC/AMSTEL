#' @title Create Indices on Common Data Model vocabulary tables.
#'
#' @description This function creates primary keys and indices for the
#' vocabulary tables in the CDM.
#'
#' @details
#' This function requires a configured `config.yaml` file created using
#' `amstel::create_config()`. It will use the connection configuration specified
#' in the `cdm` section to create the indices for the tables of the
#' Common Data Model. Since the `concept` will be used often during the ETL of
#' the AmsterdamUMCdb tables, it helps when these (static) tables have already
#' been indexed.
#'
#' This function assumes `amstel::create_cdm_tables()` and
#' `amstel::load_vocabulary_tables()` has been run successfully.
#'
#'@export

index_vocabulary_tables <- function() {

    log_info("Creating Primary Keys and Indices on CDM vocabulary tables...")

    connection_details <- get_connection_details("cdm")
    conn <- DatabaseConnector::connect(connection_details)
    on.exit(DatabaseConnector::disconnect(conn))

    sql <- load_sql('index_vocabulary_tables.sql')
    DatabaseConnector::renderTranslateExecuteSql(
        connection = conn,
        sql = sql,
        cdmDatabaseSchema = amstel_env$config$databases$cdm$schema
    )
    log_info("Primary Keys and Indices creation for vocabulary tables Complete.")

}
