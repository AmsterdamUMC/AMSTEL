#' @title Runs ACHILLES on AmsterdamUMCdb converted to the OMOP CDM
#'
#' @description This function runs the Automated Characterization of Health
#' Information at Large-Scale Longitudinal Evidence Systems (ACHILLES)
#' Achilles provides descriptive statistics on an OMOP CDM database.
#'
#'@export

achilles <- function() {

    log_info("Running OHDSI ACHILLES database characterization...")
    connection_details <- get_connection_details("cdm")

    cdm_database_schema <- amstel_env$config$databases$cdm$schema
    cdm_results_schema <- amstel_env$config$databases$results$schema
    cdm_version <- as.character(amstel_env$config$metadata$cdm_version)
    cdm_source_name <- amstel_env$config$metadata$cdm_source_name
    output_folder <- amstel_env$config$data$achilles

    # run Achilles
    result <- Achilles::achilles(connection_details,
                       cdmDatabaseSchema = cdm_database_schema,
                       resultsDatabaseSchema = cdm_results_schema,
                       sourceName = cdm_source_name,
                       cdmVersion = cdm_version,
                       outputFolder = output_folder)

    log_info(result)
    log_info("ACHILLES: complete")

}
