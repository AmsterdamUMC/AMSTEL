#' Run EHDEN CatalogueExport
#'
#' @description
#' Runs CatalogueExport to export the data from the OMOP-CDM for inclusion in
#' the EHDEN Database Catalogue. CatalogueExport executes a
#' set of analyses based on OHDSI [Achilles](https://github.com/OHDSI/Achilles).
#'
#' @export
catalogue_export <- function() {

  log_info("Running catalogueExport")

  connection_details <- get_connection_details("cdm")
  cdm_schema <- amstel_env$config$databases$cdm$schema
  results_schema <- amstel_env$config$databases$results$schema
  output_folder <- amstel_env$config$data$catalogue_export
  database_name <- amstel_env$config$metadata$cdm_source_abbreviation

  verbose_mode <- TRUE
  small_cell_count <- 5

  CatalogueExport::catalogueExport(connection_details,
                  cdmDatabaseSchema = cdm_schema,
                  resultsDatabaseSchema = results_schema,
                  vocabDatabaseSchema = cdm_schema,
                  sourceName = database_name,
                  smallCellCount = small_cell_count,
                  outputFolder = output_folder,
                  verboseMode = verbose_mode,
                  cdmVersion = "5.4")
}
