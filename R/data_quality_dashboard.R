#' @title Evaluate Data Quality of AmsterdamUMCdb OMOP CDM after ETL.
#'
#' @description This function runs the tests of the OHDSI Data Quality
#' Dashboard.
#' Based on: https://ohdsi.github.io/DataQualityDashboard/articles/DataQualityDashboard.html
#'
#'@export
execute_dqd_checks <- function() {

    log_info("Running Data Quality Dashboard quality checks...")
    connection_details <- get_connection_details("cdm")
    conn <- DatabaseConnector::connect(connection_details)
    on.exit(DatabaseConnector::disconnect(conn))

    # creates test schema if it does not exist
    sql <- "CREATE SCHEMA IF NOT EXISTS @cdm_results"
    sql <- SqlRender::render(sql, cdm_results = amstel_env$config$databases$results$schema)
    sql <- SqlRender::translate(sql, targetDialect = connection_details$dbms)
    DatabaseConnector::executeSql(conn, sql)

    # Data Quality Dashboard Settings
    # the CDM version you are targetting. Currently supports 5.2, 5.3, and 5.4
    cdm_version <- as.character(amstel_env$config$metadata$cdm_version)

    # fully qualified database schema name of the CDM
    cdm_database_schema <- amstel_env$config$databases$cdm$schema

    # fully qualified database schema name of the results schema (that you can write to)
    results_database_schema <- amstel_env$config$databases$results$schema

    # human readable name for your CDM source
    cdm_source_name <- amstel_env$config$metadata$cdm_source_name

    # determine how many threads (concurrent SQL sessions) to use
    num_threads <- 1

    # output folder (set in config.yaml)
    output_folder <- amstel_env$config$data$dqd

    # logging type
    # set to TRUE if you want to see activity written to the console
    verbose_mode <- FALSE

    # write results to table
    # set to FALSE if you want to skip writing to a SQL table in the results schema
    write_to_table <- TRUE

    # specify the name of the results table (used when writeToTable = TRUE and when sqlOnlyIncrementalInsert = TRUE)
    write_table_name <- "dqdashboard_results"

    # DQ check levels to run
    check_levels <- c("TABLE", "FIELD", "CONCEPT")

    # DQ checks to run. # Names can be found at:
    # https://github.com/OHDSI/DataQualityDashboard/blob/main/inst/csv/OMOP_CDMv5.4_Check_Descriptions.csv
    check_names <- c()

    # list of CDM table names to skip evaluating checks against;
    # by default DQD excludes the vocab tables
    tables_to_exclude <- c("CONCEPT", "VOCABULARY", "CONCEPT_ANCESTOR",
                         "CONCEPT_RELATIONSHIP", "CONCEPT_CLASS",
                         "CONCEPT_SYNONYM", "RELATIONSHIP", "DOMAIN")

    # run the job
    DataQualityDashboard::executeDqChecks(
      connectionDetails = connection_details,
      cdmDatabaseSchema = cdm_database_schema,
      resultsDatabaseSchema = results_database_schema,
      cdmVersion = cdm_version,
      cdmSourceName = cdm_source_name,
      numThreads = num_threads,
      outputFolder = output_folder,
      verboseMode = verbose_mode,
      writeToTable = write_to_table,
      writeTableName = write_table_name,
      checkLevels = check_levels,
      tablesToExclude = tables_to_exclude,
      checkNames = check_names
    )

    log_info("Data Quality Dashboard checks: complete.")
}

#' @title Show Data Quality Dashboard log (Shiny App)
#'
#' @description Shows the log created while executing
#' DataQualityDashboard::executeDqChecks()
#'
#'@export
show_dqd_log <- function() {

  # variables used while creating the log
  cdm_source_name <- amstel_env$config$metadata$cdm_source_name
  output_folder <- amstel_env$config$data$dqd

  # inspect logs
  ParallelLogger::launchLogViewer(
    logFileName = file.path(
      output_folder,
      sprintf("log_DqDashboard_%s.txt", cdm_source_name)
    )
  )

  log_info("Data Quality Dashboard checks: complete.")
}

#' @title Shows the OHDSI Data Quality Dashboard with the most recents results
#'
#' @description Shows the log created while executing
#' DataQualityDashboard::executeDqChecks()
#'
#'@export
data_quality_dashboard <- function() {

  # default output folder for OHDSI Data Quality Dashboard checks
  output_folder <- file.path(amstel_env$config$data$dqd)

  # since files are sorted alphabetically and the name contains date in ISO
  # format the last entry is the most  recent one
  json_files <- list.files(path=output_folder, pattern="*.json")
  json_file <- tail(json_files, 1)

  json_filepath <- file.path(output_folder, json_file)
  DataQualityDashboard::viewDqDashboard(json_filepath)
}
