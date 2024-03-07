
#' Checks whether the AmsterdamUMCdb environment is available for running
#' example code
#'
#' @return Bool
#' @export
#'
has_adb_environment <- function() {
  result <- tryCatch({
    # (re)loads configuration that could be overwritten by example environment
    amstel_env$config <- get_config()
    connect("amsterdamumcdb")
  },
  error = function(err) {
    NULL
  }
  )
  if(is.null(result)) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}

#' Checks whether the CDM environment is available for running example code
#'
#' @return Bool
#' @export
#'
has_cdm_environment <- function() {
  result <- tryCatch({
    # (re)loads configuration that could be overwritten by example environment
    amstel_env$config <- get_config()
    connect("cdm")
  },
  error = function(err) {
    NULL
  }
  )
  if(is.null(result)) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}

#' Checks whether an example environment can be used
#'
#' @return Bool
#' @export
#'
has_example_environment <- function() {
  result <- tryCatch({
    create_environment(name = "example", clean = TRUE)
  },
  error = function(err) {
    NULL
  }
  )
  if(is.null(result)) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}

#' Creates a new database environment (database schemes) for AmsterdamUMCdb
#' and the CDM data and results schemes for testing and/or
#' example purposes.
#'
#' @param name
#' Name of the environment to create.
#' Default: "test"
#'
#' @param clean
#' Specifies whether to start with completely empty schemes by dropping the
#' existing ones. WARNING: this will completely remove both schemes if the
#' specified user is the owner of the schemes.
#' Default: FALSE
#'
#' @return list of connections to both schemes
#' @export
create_environment <- function(name = "test", clean = FALSE) {
  # Set the environment to test schemes to allow functions that reference
  # the environment to access the correct schema
  amsterdaumcdb_env_schema <- paste0("amsterdamumcdb", "_", name)
  cdm_env_schema <- paste0("cdm", "_", name)
  results_env_schema <- paste0("results", "_", name)
  amstel_env$config$databases$amsterdamumcdb$schema <- amsterdaumcdb_env_schema
  amstel_env$config$databases$cdm$schema <- cdm_env_schema
  amstel_env$config$databases$results$schema <- results_env_schema

  # Create connections to both databases
  adb_connection_details <- get_connection_details("amsterdamumcdb")
  adb_conn <- DatabaseConnector::connect(adb_connection_details)

  cdm_connection_details <- get_connection_details("cdm")
  cdm_conn <- DatabaseConnector::connect(cdm_connection_details)

  results_connection_details <- get_connection_details("results")
  results_conn <- DatabaseConnector::connect(results_connection_details)

  if (clean == TRUE) {
    log_info(paste0("Dropping AmsterdamUMCdb ", name, " schema..."))
    sql <- "DROP SCHEMA IF EXISTS @amsterdamumcdb_schema CASCADE;"
    DatabaseConnector::renderTranslateExecuteSql(
      progressBar = interactive(),
      connection = adb_conn,
      sql = sql,
      amsterdamumcdb_schema = amsterdaumcdb_env_schema
    )

    log_info(paste0("Dropping CDM ", name, " schema..."))
    sql <- "DROP SCHEMA IF EXISTS @cdm_schema CASCADE;"
    DatabaseConnector::renderTranslateExecuteSql(
      progressBar = interactive(),
      connection = cdm_conn,
      sql = sql,
      cdm_schema = cdm_env_schema
    )

    log_info(paste0("Dropping Results ", name, " schema..."))
    sql <- "DROP SCHEMA IF EXISTS @results_schema CASCADE;"
    DatabaseConnector::renderTranslateExecuteSql(
      progressBar = interactive(),
      connection = cdm_conn,
      sql = sql,
      results_schema = results_env_schema
    )
  }

  # Create a clean AmsterdamUMCdb test schema with empty test tables
  log_info(paste0("Creating AmsterdamUMCdb ", name, " schema and tables..."))
  sql <- "CREATE SCHEMA IF NOT EXISTS @amsterdamumcdb_schema;"
  DatabaseConnector::renderTranslateExecuteSql(
    progressBar = interactive(),
    connection = adb_conn,
    sql = sql,
    amsterdamumcdb_schema = amsterdaumcdb_env_schema
  )
  sql <- load_sql('create_amsterdamumcdb_tables.sql')
  DatabaseConnector::renderTranslateExecuteSql(
    progressBar = interactive(),
    connection = adb_conn,
    sql = sql,
    amsterdamumcdb_schema = amsterdaumcdb_env_schema
  )

  # Create a clean CDM test schema with empty test tables
  log_info(paste0("Creating CDM ", name, " schema and tables..."))
  sql <- "CREATE SCHEMA IF NOT EXISTS @cdm_schema;"
  DatabaseConnector::renderTranslateExecuteSql(
    progressBar = interactive(),
    connection = adb_conn,
    sql = sql,
    cdm_schema = cdm_env_schema
  )
  sql <- load_sql('create_data_tables.sql')
  DatabaseConnector::renderTranslateExecuteSql(
    progressBar = interactive(),
    connection = adb_conn,
    sql = sql,
    cdm_schema = cdm_env_schema
  )
  sql <- load_sql('create_vocabulary_tables.sql')
  DatabaseConnector::renderTranslateExecuteSql(
    progressBar = interactive(),
    connection = adb_conn,
    sql = sql,
    cdm_schema = cdm_env_schema
  )

  # Create a clean results schema
  log_info(paste0("Creating CDM ", name, " schema and tables..."))
  sql <- "CREATE SCHEMA IF NOT EXISTS @results_schema;"
  DatabaseConnector::renderTranslateExecuteSql(
    progressBar = interactive(),
    connection = adb_conn,
    sql = sql,
    results_schema = results_env_schema
  )

  return(list("adb_conn" = adb_conn,
              "cdm_conn" = cdm_conn,
              "results_conn" = results_conn
              )
         )
}
