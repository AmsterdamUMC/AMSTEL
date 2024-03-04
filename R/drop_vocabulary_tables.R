#' @title Drop Vocabulary Tables.
#'
#' @description This function drops Vocabulary tables in a CDM.
#'
#' @param cascade Determines whether to drop dependent objects too.
#' **WARNING**: Since all CDM data tables depend on the vocabulary,
#' this will delete most data tables as well. Default: FALSE
#'
#'@export

drop_vocabulary_tables <- function(cascade = FALSE) {
  connection_details <- get_connection_details("cdm")
  cdm_schema <- amstel_env$config$databases$cdm$schema

  vocabulary_tables <- c(
    "ATTRIBUTE_DEFINITION",
    "COHORT_DEFINITION",
    "CONCEPT",
    "CONCEPT_ANCESTOR",
    "CONCEPT_CLASS",
    "CONCEPT_RELATIONSHIP",
    "CONCEPT_SYNONYM",
    "DOMAIN",
    "DRUG_STRENGTH",
    "RELATIONSHIP",
    "SOURCE_TO_CONCEPT_MAP",
    "VOCABULARY"
  )

  log_info("Dropping CDM Vocabulary Tables...")
  conn <- DatabaseConnector::connect(connection_details)
  on.exit(DatabaseConnector::disconnect(conn))
  all_tables <- DatabaseConnector::getTableNames(conn, cdm_schema)
  tables_to_drop <- all_tables[which(toupper(all_tables) %in% vocabulary_tables)]
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
    cdm_schema = cdm_schema
  )
  log_info("Dropping CDM Vocabulary Tables: Done.")
}
