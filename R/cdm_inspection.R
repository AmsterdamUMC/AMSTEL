#' Runs EHDEN CdmInspection
#'
#' @description
#' Runs the EHDEN CdmInspection tool to check the OMOP-CDM instance
#' before submission to EHDEN.
#'
#' CdmInspection was created to support quality control inspection of an
#' OMOP-CDM instance.
#'
#' @export
cdm_inspection <- function() {

  log_info("Running CdmInspection...")

  connection_details <- get_connection_details("cdm")
  cdm_schema <- amstel_env$config$databases$cdm$schema
  results_schema <- amstel_env$config$databases$results$schema
  small_cell_count <- 5
  webapi_base_url <- amstel_env$config$webapi$url
  output_folder <- amstel_env$config$data$cdm_inspection
  verbose_mode <- TRUE

  authors <- amstel_env$config$metadata$cdm_inspection_authors
  database_id <- amstel_env$config$metadata$cdm_source_abbreviation
  database_name <- amstel_env$config$metadata$cdm_source_abbreviation
  database_description <- amstel_env$config$metadata$source_description

  results <- CdmInspection::cdmInspection(
    connectionDetails = connection_details,
    cdmDatabaseSchema = cdm_schema,
    resultsDatabaseSchema = results_schema,
    vocabDatabaseSchema = cdm_schema,
    databaseId = database_id,
    databaseName = database_name,
    databaseDescription = database_description,
    runVocabularyChecks = TRUE,
    runDataTablesChecks = TRUE,
    runPerformanceChecks = TRUE,
    runWebAPIChecks = TRUE,
    smallCellCount = small_cell_count,
    baseUrl = webapi_base_url,
    sqlOnly = FALSE,
    outputFolder = output_folder,
    verboseMode = verbose_mode
  )

  log_info("Generating CdmInspection report...")

  CdmInspection::generateResultsDocument(
    results = results,
    outputFolder = output_folder,
    authors = authors,
    databaseId = database_id,
    databaseName = database_name,
    databaseDescription = database_description,
    smallCellCount = small_cell_count
  )
}
