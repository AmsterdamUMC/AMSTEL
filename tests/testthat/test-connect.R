test_that("connection works", {
  # verify that we actually connect to instance containing AmsterdamUMCdb
  # tables by comparing the list of tables stored in the database

  tables <- c("admissions", "drugitems", "freetextitems", "listitems",
    "numericitems", "procedureorderitems", "processitems")

  # setup the connection
  con <- connect("amsterdamumcdb")
  on.exit(DatabaseConnector::disconnect(con))
  # retrieve the list of tables
  tables_con <- DBI::dbListTables(con)

  for(table in tables) {
    log_info(paste0('\n', table))
    expect_true(table %in% tables_con)
  }
})
