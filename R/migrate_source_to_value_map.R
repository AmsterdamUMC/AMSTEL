#' @title Migrates the source_to_concept_map data to the
#' new source_to_value_map table
#'
#'@export
migrate_source_to_value_map  <- function() {
  log_info("Migrating source_to_value_map...")

  connection_details <- get_connection_details("cdm")
  conn <- DatabaseConnector::connect(connection_details)
  on.exit(DatabaseConnector::disconnect(conn))

  sql <- load_sql('migrate_source_to_value_map.sql')
  DatabaseConnector::renderTranslateExecuteSql(
    progressBar = interactive(),
    connection = conn,
    sql = sql,
    cdm_schema = amstel_env$config$databases$cdm$schema
  )

  # load current contents of the migrated table and
  # save to source_to_value_map

  sql <- "SELECT * FROM @cdm_schema.source_to_value_map"
  source_to_value_map <- DatabaseConnector::renderTranslateQuerySql(
    connection = conn,
    sql = sql,
    cdm_schema = amstel_env$config$databases$cdm$schema
  )

  log_info("Saving source_to_value_map.csv in mappings folder...")
  stvm_file <- file.path(amstel_env$config$data$mappings,
                         "source_to_value_map.csv")
  source_to_value_map %>% readr::write_csv(
    file=stvm_file,
    na = "",
    quote = "needed")

  log_info("Migration complete.")
}
