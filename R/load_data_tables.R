#' @title Load CDM Standardized Clinical Data Tables.
#'
#' @description This function loads the CDM Clinical Data tables with data from
#' AmsterdamUMCdb
#'
#' @details
#' This function requires a configured `config.yaml` file created using
#' `amstel::create_config()`. It will use the connection configuration specified
#' in the `amsterdamumcdb` and `cdm` sections to extract, transform and load the
#' data from AmsterdamUMCdb into the Common Data Model.
#'
#' This function assumes `amstel::create_cdm_tables()`,
#' `amstel::load_vocabulary_tables()` and `amstel::load_source_to_concept_map()`
#' have been run successfully.
#'
#'@export


load_data_tables <- function() {

  log_info("Populating CDM Data Tables with data from AmsterdamUMCdb")

  connection_details <- get_connection_details("cdm")
  conn <- DatabaseConnector::connect(connection_details)
  on.exit(DatabaseConnector::disconnect(conn))

  # drop remove indices before commencing bulk loading
  log_info("Dropping tables before bulk loading")
  sql <- load_sql('drop_index_data_tables.sql')
  DatabaseConnector::renderTranslateExecuteSql(
    progressBar = interactive(),
    connection = conn,
    sql = sql
  )

  # cdm source
  log_info("Standardized Clinical Data:")
  log_info("- CDM_SOURCE")
  sql <- load_sql('insert_cdm_source.sql')
  DatabaseConnector::renderTranslateExecuteSql(
    progressBar = interactive(),
    connection = conn,
    sql = sql,
    cdm_schema = amstel_env$config$databases$cdm$schema,
    cdm_source_name = amstel_env$config$metadata$cdm_source_name,
    cdm_source_abbreviation = amstel_env$config$metadata$cdm_source_abbreviation,
    cdm_holder = amstel_env$config$metadata$cdm_holder,
    source_description = amstel_env$config$metadata$source_description,
    source_documentation_reference = amstel_env$config$metadata$source_documentation_reference,
    cdm_etl_reference = amstel_env$config$metadata$cdm_etl_reference,
    cdm_version = amstel_env$config$metadata$cdm_version,
    source_release_date = amstel_env$config$metadata$source_release_date
  )

  # admissions_scalar: intermediate table from admissions that contains scalar values for
  # - age
  # - admission year
  # - weight
  # - height
  # For easier calculating/manipulating dates in CDM tables
  log_info("- admission_scalar (intermediate table)")
  sql <- load_sql('create_admissions_scalar.sql')
  DatabaseConnector::renderTranslateExecuteSql(
    progressBar = interactive(),
    connection = conn,
    sql = sql,
    cdm_schema = amstel_env$config$databases$cdm$schema,
    amsterdamumcdb_schema = amstel_env$config$databases$amsterdamumcdb$schema
  )

  # person
  log_info("- PERSON")
  sql <- load_sql('insert_person.sql')
  DatabaseConnector::renderTranslateExecuteSql(
    progressBar = interactive(),
    connection = conn,
    sql = sql,
    cdm_schema = amstel_env$config$databases$cdm$schema
  )

  # death
  log_info("- DEATH")
  sql <- load_sql('insert_death.sql')
  DatabaseConnector::renderTranslateExecuteSql(
    progressBar = interactive(),
    connection = conn,
    sql = sql,
    cdm_schema = amstel_env$config$databases$cdm$schema
  )

  # care_site
  log_info("- CARE_SITE")
  sql <- load_sql('insert_care_site.sql')
  DatabaseConnector::renderTranslateExecuteSql(
    progressBar = interactive(),
    connection = conn,
    sql = sql,
    cdm_schema = amstel_env$config$databases$cdm$schema
  )

  # location
  log_info("- LOCATION")
  sql <- load_sql('insert_location.sql')
  DatabaseConnector::renderTranslateExecuteSql(
    progressBar = interactive(),
    connection = conn,
    sql = sql,
    cdm_schema = amstel_env$config$databases$cdm$schema
  )

  # provider
  log_info("- PROVIDER")
  sql <- load_sql('insert_provider.sql')
  DatabaseConnector::renderTranslateExecuteSql(
    progressBar = interactive(),
    connection = conn,
    sql = sql,
    cdm_schema = amstel_env$config$databases$cdm$schema
  )

  # visit occurrence
  log_info("- VISIT_OCCURRENCE")
  sql <- load_sql('insert_visit_occurrence.sql')
  DatabaseConnector::renderTranslateExecuteSql(
    progressBar = interactive(),
    connection = conn,
    sql = sql,
    cdm_schema = amstel_env$config$databases$cdm$schema
  )

  # observation_period
  # This table requires visit_occurrence already populated
  log_info("- OBSERVATION_PERIOD")
  sql <- load_sql('insert_observation_period.sql')
  DatabaseConnector::renderTranslateExecuteSql(
    progressBar = interactive(),
    connection = conn,
    sql = sql,
    cdm_schema = amstel_env$config$databases$cdm$schema
  )

  # creates the intermediate stem_table
  log_info("- stem_table (intermediate table)")
  sql <- load_sql('create_stem_table.sql')
  DatabaseConnector::renderTranslateExecuteSql(
    progressBar = interactive(),
    connection = conn,
    sql = sql,
    cdm_schema = amstel_env$config$databases$cdm$schema
  )

  # inserts reason for admission (from listitems) into stem_table
  log_info("- reason for admission -> stem_table")
  sql <- load_sql('insert_stem_table_from_reason_for_admission.sql')
  DatabaseConnector::renderTranslateExecuteSql(
    progressBar = interactive(),
    connection = conn,
    sql = sql,
    cdm_schema = amstel_env$config$databases$cdm$schema,
    ams_schema = amstel_env$config$databases$amsterdamumcdb$schema
  )

  # inserts admissions (body height/weight) into stem_table
  log_info("- admissions -> stem_table")
  sql <- load_sql('insert_stem_table_from_admissions.sql')
  DatabaseConnector::renderTranslateExecuteSql(
    progressBar = interactive(),
    connection = conn,
    sql = sql,
    cdm_schema = amstel_env$config$databases$cdm$schema
  )

  # inserts drugitems into stem_table
  log_info("- drugitems -> stem_table")
  sql <- load_sql('insert_stem_table_from_drugitems.sql')
  DatabaseConnector::renderTranslateExecuteSql(
    progressBar = interactive(),
    connection = conn,
    sql = sql,
    cdm_schema = amstel_env$config$databases$cdm$schema,
    ams_schema = amstel_env$config$databases$amsterdamumcdb$schema
  )

  # inserts fluidin into stem_table
  # requires populated drug_exposure records in stem_table
  log_info("- fluidin -> stem_table")
  sql <- load_sql('insert_stem_table_from_fluidin.sql')
  DatabaseConnector::renderTranslateExecuteSql(
    progressBar = interactive(),
    connection = conn,
    sql = sql,
    cdm_schema = amstel_env$config$databases$cdm$schema
  )

  # inserts specimen_source into stem_table
  log_info("- specimen source -> stem_table")
  sql <- load_sql('insert_stem_table_from_specimen_source.sql')
  DatabaseConnector::renderTranslateExecuteSql(
    progressBar = interactive(),
    connection = conn,
    sql = sql,
    cdm_schema = amstel_env$config$databases$cdm$schema,
    ams_schema = amstel_env$config$databases$amsterdamumcdb$schema
  )

  # inserts freetextitems into stem_table
  log_info("- freetextitems -> stem_table")
  sql <- load_sql('insert_stem_table_from_freetextitems.sql')
  DatabaseConnector::renderTranslateExecuteSql(
    progressBar = interactive(),
    connection = conn,
    sql = sql,
    cdm_schema = amstel_env$config$databases$cdm$schema,
    ams_schema = amstel_env$config$databases$amsterdamumcdb$schema
  )

  # inserts listitems into stem_table
  log_info("- listitems -> stem_table")
  sql <- load_sql('insert_stem_table_from_listitems.sql')
  DatabaseConnector::renderTranslateExecuteSql(
    progressBar = interactive(),
    connection = conn,
    sql = sql,
    cdm_schema = amstel_env$config$databases$cdm$schema,
    ams_schema = amstel_env$config$databases$amsterdamumcdb$schema
  )

  # inserts numericitems into stem_table
  log_info("- numericitems -> stem_table")
  sql <- load_sql('insert_stem_table_from_numericitems.sql')
  DatabaseConnector::renderTranslateExecuteSql(
    progressBar = interactive(),
    connection = conn,
    sql = sql,
    cdm_schema = amstel_env$config$databases$cdm$schema,
    ams_schema = amstel_env$config$databases$amsterdamumcdb$schema
  )

  # inserts processitems into stem_table
  log_info("- processitems -> stem_table")
  sql <- load_sql('insert_stem_table_from_processitems.sql')
  DatabaseConnector::renderTranslateExecuteSql(
    progressBar = interactive(),
    connection = conn,
    sql = sql,
    cdm_schema = amstel_env$config$databases$cdm$schema,
    ams_schema = amstel_env$config$databases$amsterdamumcdb$schema
  )

  # inserts procedureorderitems into stem_table
  log_info("- procedureorderitems -> stem_table")
  sql <- load_sql('insert_stem_table_from_procedureorderitems.sql')
  DatabaseConnector::renderTranslateExecuteSql(
    progressBar = interactive(),
    connection = conn,
    sql = sql,
    cdm_schema = amstel_env$config$databases$cdm$schema,
    ams_schema = amstel_env$config$databases$amsterdamumcdb$schema
  )

  # inserts records into the clinical data tables based on domain_id from
  # stem_table:
  # condition occurrence (domain_id = 19)
  log_info("- stem_table -> CONDITION_OCCURRENCE")
  sql <- load_sql('insert_condition_occurrence.sql')
  DatabaseConnector::renderTranslateExecuteSql(
    progressBar = interactive(),
    connection = conn,
    sql = sql,
    cdm_schema = amstel_env$config$databases$cdm$schema
  )

  # device_exposure (domain_id = 17)
  log_info("- stem_table -> DEVICE_EXPOSURE")
  sql <- load_sql('insert_device_exposure.sql')
  DatabaseConnector::renderTranslateExecuteSql(
    progressBar = interactive(),
    connection = conn,
    sql = sql,
    cdm_schema = amstel_env$config$databases$cdm$schema
  )

  # drug_exposure (domain_id = 13)
  log_info("- stem_table -> DRUG_EXPOSURE")
  sql <- load_sql('insert_drug_exposure.sql')
  DatabaseConnector::renderTranslateExecuteSql(
    progressBar = interactive(),
    connection = conn,
    sql = sql,
    cdm_schema = amstel_env$config$databases$cdm$schema
  )

  # measurement (domain_id = 21)
  log_info("- stem_table -> MEASUREMENT")
  sql <- load_sql('insert_measurement.sql')
  DatabaseConnector::renderTranslateExecuteSql(
    progressBar = interactive(),
    connection = conn,
    sql = sql,
    cdm_schema = amstel_env$config$databases$cdm$schema
  )

  # observation (domain_id = 27)
  log_info("- stem_table -> OBSERVATION")
  sql <- load_sql('insert_observation.sql')
  DatabaseConnector::renderTranslateExecuteSql(
    progressBar = interactive(),
    connection = conn,
    sql = sql,
    cdm_schema = amstel_env$config$databases$cdm$schema
  )

  # procedure_occurrence (domain_id = 10)
  log_info("- stem_table -> PROCEDURE_OCCURRENCE")
  sql <- load_sql('insert_procedure_occurrence.sql')
  DatabaseConnector::renderTranslateExecuteSql(
    progressBar = interactive(),
    connection = conn,
    sql = sql,
    cdm_schema = amstel_env$config$databases$cdm$schema
  )

  # specimen (domain_id = 36)
  log_info("- stem_table -> SPECIMEN")
  sql <- load_sql('insert_specimen.sql')
  DatabaseConnector::renderTranslateExecuteSql(
    progressBar = interactive(),
    connection = conn,
    sql = sql,
    cdm_schema = amstel_env$config$databases$cdm$schema
  )

  # Standard Derived Tables
  log_info("- Standard Derived Tables")

  # condition era
  log_info("  * CONDITION_ERA")
  sql <- load_sql('insert_condition_era.sql')
  DatabaseConnector::renderTranslateExecuteSql(
    progressBar = interactive(),
    connection = conn,
    sql = sql,
    cdm_schema = amstel_env$config$databases$cdm$schema
  )

  # drug era
  log_info("  * DRUG_ERA")
  sql <- load_sql('insert_drug_era.sql')
  DatabaseConnector::renderTranslateExecuteSql(
    progressBar = interactive(),
    connection = conn,
    sql = sql,
    cdm_schema = amstel_env$config$databases$cdm$schema
  )

  log_info("Loading CDM Data tables: complete.")
}
