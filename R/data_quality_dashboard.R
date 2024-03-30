#' @param custom_control
#' Specifies whether to apply the customized control files (thresholds)
#' for AmsterdamUMCdb
#' Default: TRUE
#'
#' @param check_names
#' Vector (`c()`) of data quality checks to run. Names can be found at:
#' https://github.com/OHDSI/DataQualityDashboard/blob/main/inst/csv/OMOP_CDMv5.4_Check_Descriptions.csv
#' Default: c()
#'
#' @param sql_only
#' Specifies to output the SQL query files instead of executing them. The files
#' will be stored in the `sql` folder of the `dqd` output folder as specified in
#' `config.yaml`. Default: FALSE
#'
#' @title Evaluate Data Quality of AmsterdamUMCdb OMOP CDM after ETL.
#'
#' @description This function runs the tests of the OHDSI Data Quality
#' Dashboard.
#' Based on: https://ohdsi.github.io/DataQualityDashboard/articles/DataQualityDashboard.html
#'
#' @details
#' This function runs `DataQualityDashboard::executeDqChecks()`
#' to perform the data quality checks on the AmsterdamUMCdb CDM instance. After
#' a successful execution a json results file will be created, that can be
#' shown in the DataQualityDashboard Shiny app using
#' `amstel::data_quality_dashboard()`.
#'
#'@export
execute_dqd_checks <- function(custom_control = TRUE, check_names = c(),
                               sql_only = FALSE) {

    log_info("Running Data Quality Dashboard quality checks...")
    connection_details <- get_connection_details("cdm")
    conn <- DatabaseConnector::connect(connection_details)
    on.exit(DatabaseConnector::disconnect(conn))

    # creates results schema if it does not exist
    sql <- "CREATE SCHEMA IF NOT EXISTS @cdm_results"
    sql <- SqlRender::render(sql, cdm_results = amstel_env$config$databases$results$schema)
    sql <- SqlRender::translate(sql, targetDialect = connection_details$dbms)
    DatabaseConnector::executeSql(
      progressBar = interactive(),
      connection = conn,
      sql = sql
      )

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

    # output folder (set in config.yaml), modify for sql_only mode
    if(sql_only == TRUE) {
      output_folder <- file.path(amstel_env$config$data$dqd, "sql")
    } else {
      output_folder <- amstel_env$config$data$dqd
    }

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
                         "CONCEPT_SYNONYM", "RELATIONSHIP", "DOMAIN",
                         "DRUG_STRENGTH")


    # apply custom control files:
    # https://github.com/OHDSI/DataQualityDashboard/raw/main/inst/doc/Thresholds.pdf
    if(custom_control == TRUE) {
      dqd_folder <- file.path(amstel_env$config$data$dqd)
      table_check_threshold <- file.path(dqd_folder,
                                         "OMOP_CDMv5.4_Table_Level.csv")
      field_check_threshold <- file.path(dqd_folder,
                                        "OMOP_CDMv5.4_Field_Level.csv")
      concept_check_threshold <- file.path(dqd_folder,
                                           "OMOP_CDMv5.4_Concept_Level.csv")
    }
    else {
      table_check_threshold <- "default"
      field_check_threshold <- "default"
      concept_check_threshold <- "default"
    }

    # write only the sql queries for debugging
    if(sql_only == TRUE) {
      sql_only_incremental_insert = TRUE
    }
    else {
      sql_only_incremental_insert = FALSE
    }

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
      checkNames = check_names,

      # create only queries (e.g. for debugging the ETL) when set to TRUE
      sqlOnly = sql_only,

      # create queries that INSERT result in the table when set to TRUE
      sqlOnlyIncrementalInsert = sql_only_incremental_insert,

      tableCheckThresholdLoc = table_check_threshold,
      fieldCheckThresholdLoc = field_check_threshold,
      conceptCheckThresholdLoc = concept_check_threshold
    )

    log_info("Data Quality Dashboard checks: complete.")
}

#' @title Show Data Quality Dashboard log
#'
#' @description Shows the log created by `amstel::exececute_dqd_checks()`.
#'
#' @details
#' Runs the ParallelLogger logviewer shiny app using
#' `ParallelLogger::launchLogViewer`.
#'
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
}

#' @title Shows the OHDSI Data Quality Dashboard
#'
#' @description Shows DataQualityDashboard with results creating using
#' `amstel::execute_dqd_checks`.
#'
#' @param json_file
#' Specifies the json file to use in `./data/dqd/`. By default, the most
#' recent results file will be used. Default: ""
#'
#' @details
#' Runs `DataQualityDashboard::viewDqDashboard()` to show the results.
#'
#'@export
data_quality_dashboard <- function(json_file = "") {

  log_info("Running DataQualityDashboard app...")

  # default output folder for OHDSI Data Quality Dashboard checks
  output_folder <- file.path(amstel_env$config$data$dqd)
  # since files are sorted alphabetically and the name contains date in ISO
  # format the last entry is the most recent one
  json_files <- list.files(path=output_folder, pattern="*.json")

  log_info("Available DQD JSON result files:")
  log_info(paste(json_files, sep="\n"))

  if(json_file == "") {
    json_file <- tail(json_files, 1)
    json_filepath <- file.path(output_folder, json_file)
  }
  else {
    json_filepath <- file.path(output_folder, json_file)
  }
  log_info(paste0("Using DQD results file: ", json_filepath))
  DataQualityDashboard::viewDqDashboard(json_filepath)
}
