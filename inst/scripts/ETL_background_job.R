# Script for running the ETL as an RStudio background job
#
# The ETL for AmsterdamUMCdb is based on the excellent and well-documented
# ETL of the Synthea project available at: https://github.com/OHDSI/ETL-Synthea
#
# This script runs the different ETL steps sequentially. This is equivalent to
# calling `amstel::etl()` but allows the end-user to see the progress of the
# steps in the Background Jobs panel by using 'Code Section'

# Create 'fresh' CDM data tables ----
amstel::drop_data_tables()
amstel::create_data_tables()

# Create 'fresh' CDM vocabulary tables ----
amstel::drop_vocabulary_tables()
amstel::create_vocabulary_tables()

# Load the vocabulary csv files into the CDM ----
amstel::load_vocabulary_tables()

# Index vocabularies to improve query performance ----
amstel::index_vocabulary_tables()

# Load the source_to_concept_map into the CDM ----
amstel::load_source_to_concept_map()

# Load the clinical data into the CDM ----
amstel::load_data_tables()

# Index the CDM to improve query performance ----
amstel::index_data_tables()

# correct data issues
amstel::apply_corrections()

# Create example AmsterdamUMCdb cohort tables (mechanical ventilation) ----
amstel::create_cohort_tables()

# Runs OHDSI Data Quality Dashboard data quality checks ----
amstel::execute_dqd_checks()

# Runs OHDSI Achilles database characterization ----
amstel::achilles()

# Creates Web API tables ----
amstel::create_webapi_tables()

# Development Only:
# Runs OHDSI CDMInspection package for submission ----
# amstel::cdm_inspection()

# Development Only:
# Runs EHDEN CatalogueExport for submission the EHDEN Database Catalogue
# amstel::catalogue_export()
