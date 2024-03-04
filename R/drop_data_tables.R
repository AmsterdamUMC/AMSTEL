#' @title Drop CDM Data Tables.
#'
#' @description This function drops CDM tables, excluding the vocabulary tables.
#'
#' @details
#' This function requires a configured `config.yaml` file created using
#' `amstel::create_config()`. It will use the connection configuration specified
#' in the `cdm` section to delete the Clinical Data tables from the
#' Common Data Model.
#'
#'@export
drop_data_tables <- function(cascade = FALSE)
{
  cdm_schema <- amstel_env$config$cdm$schema
  connection_details <- get_connection_details("cdm")

  data_tables <- c(
    "CARE_SITE",
    "CDM_SOURCE",
    "COHORT",
    "COHORT_DEFINITION",
    "COHORT_ATTRIBUTE",
    "CONDITION_ERA",
    "CONDITION_OCCURRENCE",
    "COST",
    "DEATH",
    "EPISODE",
    "EPISODE_EVENT",
    "DEVICE_EXPOSURE",
    "DOSE_ERA",
    "DRUG_ERA",
    "DRUG_EXPOSURE",
    "FACT_RELATIONSHIP",
    "LOCATION",
    "MEASUREMENT",
    "METADATA",
    "NOTE",
    "NOTE_NLP",
    "OBSERVATION",
    "OBSERVATION_PERIOD",
    "PAYER_PLAN_PERIOD",
    "PERSON",
    "PROCEDURE_OCCURRENCE",
    "PROVIDER",
    "SPECIMEN",
    "VISIT_DETAIL",
    "VISIT_OCCURRENCE",
    "STEM_TABLE"
  )

  log_info("Dropping CDM Data Tables...")
  conn <- DatabaseConnector::connect(connection_details)
  on.exit(DatabaseConnector::disconnect(conn))
  all_tables <- DatabaseConnector::getTableNames(conn, cdm_schema)
  tables_to_drop <- all_tables[which(toupper(all_tables) %in% data_tables)]
  cascade_option <- if(cascade == TRUE) " CASCADE" else ""

  sql <-
    paste("DROP TABLE IF EXISTS @cdm_schema.",
          tables_to_drop,
          cascade_option,
          ";",
          collapse = "\n",
          sep = "")

  DatabaseConnector::renderTranslateExecuteSql(
    connection = conn,
    sql = sql,
    cdm_schema = amstel_env$config$databases$cdm$schema
  )

  log_info("Dropping CDM Data Tables: Done.")
}
