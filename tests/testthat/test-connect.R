test_that("connection works", {
  # verify that we actually connect to instance containing AmsterdamUMCdb
  # tables by comparing the list of tables stored in the database

  tables <- c("admissions", "drugitems", "freetextitems", "listitems",
    "numericitems", "procedureorderitems", "processitems")

  # setup the connection
  con <- connect()
  # retrieve the list of tables
  tables_con <- DBI::dbListTables(con)

  expect_equal(tables_con, tables)
})
