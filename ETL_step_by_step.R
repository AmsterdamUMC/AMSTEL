# The ETL for AmsterdamUMCdb is based on the excellent and well-documented
# ETL of the Synthea project available at: https://github.com/OHDSI/ETL-Synthea
library(amstel)

# Create CDM tables
amstel::create_cdm_tables()

# Load the vocabulary csv files into the CDM
amstel::load_vocabulary_tables()

# Creates primary keys and indices for vocabularies
amstel::index_vocabulary_tables()

# Load the source_to_concept_map into the CDM
amstel::load_source_to_concept_map()

# Load the clinical data into the CDM
amstel::load_data_tables()

# Index the CDM to improve query improvement
amstel::index_data_tables()

# Create example AmsterdamUMCdb cohort tables (mechanical ventilation)
amstel::create_cohort_tables()

# Runs OHDSI Data Quality Dashboard data quality checks
amstel::execute_dqd_checks()

# Runs OHDSI Achilles database characterization
amstel::achilles()
