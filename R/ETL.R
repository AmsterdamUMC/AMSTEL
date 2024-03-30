#' Runs the main ETL steps for converting AmsterdamUMCdb to the OMOP CDM
#'
#' @description
#' This function executes the ETL by creating the CDM and vocabulary tables,
#' loading the vocabulary tables, indexing those tables,
#' loading the source_to_concept_map, loading the data tables,
#' indexing the data tables, creating example cohort tables and
#' running the DataQualityDashboard checks and Achilles aggregation.
#'
#' @param vocabulary
#' Determines whether the vocabulary tables should be processed. Since the
#' vocabularies will not change often, testing run time can be reduced by
#' ignoring these tables.
#' Default: TRUE
#'
#' @param mappings
#' Determines whether the source to concept mappings will be generated using
#' the mappings files created with OHDSI Usagi.
#' Default: TRUE
#'
#' @param achilles
#' Determines whether OHDSI Achilles should be run after loading the data in
#' the CDM to generate table aggregations for use in WebAPI/ATLAS.
#' Setting this to FALSE will significantly reduce testing run time.
#' Default: TRUE
#'
#' @param dqd
#' Determines whether the checks of OHDSI DataQualityDashboard should be run
#' after loading the data in the CDM. Setting this to FALSE will significantly
#' reduce testing run time.
#' Default: TRUE
#'
#' @details
#' This function requires a configured `config.yaml` file created using
#' `amstel::create_config()`.
#'
#' The ETL for AmsterdamUMCdb is based on the excellent and well-documented
#' ETL of the Sythea project available at: https://github.com/OHDSI/ETL-Synthea.

#'
#' @export
etl <- function(vocabulary = TRUE, mappings = TRUE,
                achilles = TRUE, dqd = TRUE) {
  # Create CDM data tables
  drop_data_tables()
  create_data_tables()

  if (vocabulary == TRUE) {
    # Create CDM vocabulary tables
    drop_vocabulary_tables()
    create_vocabulary_tables()

    # Load the vocabulary csv files into the CDM
    load_vocabulary_tables()

    # Index vocabularies to improve query performance
    index_vocabulary_tables()
  }

  # Load the source_to_concept_map into the CDM
  if (mappings == TRUE) {
    load_source_to_concept_map()
  }

  # Load the clinical data into the CDM
  load_data_tables()

  # Index the CDM to improve query performance
  index_data_tables()

  # correct data issues
  apply_corrections()

  # Create example AmsterdamUMCdb cohort tables (mechanical ventilation)
  create_cohort_tables()

  # Runs OHDSI Data Quality Dashboard data quality checks
  if (dqd == TRUE) {
    execute_dqd_checks()
  }

  # Runs OHDSI Achilles database characterization
  if (achilles == TRUE) {
    achilles()
  }

}
