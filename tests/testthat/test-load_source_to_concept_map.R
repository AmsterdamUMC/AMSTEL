test_that("Importing Source to concept mapping works", {
  # Set the environment to test schemes to allow functions that reference
  # the environment to access the correct schema
  cdm_test_schema <- "cdm_test"
  connections = create_environment(name="test")
  cdm_conn = connections$cdm_conn
  on.exit(DatabaseConnector::disconnect(cdm_conn), add = TRUE)

  stcm <- load_source_to_concept_map()

  # should contain two entries (one for "AUMC Laboratory" and
  # one for "AUMC Numeric Unit") for source code "11850" (Monocyten % (bloed))
  mapping <- stcm %>% dplyr::filter(
    .data$source_code == "11850"
  )
  expect_equal(nrow(mapping), 2)

  # check mapping target concept
  mapping <- stcm %>% dplyr::filter(
    .data$source_code == '11850',
    .data$source_vocabulary_id == "AUMC Laboratory"
    )

  # one row
  expect_equal(nrow(mapping), 1)
  # mapping of measurement concept ()
  expect_equal(mapping$target_concept_id, 3019069)

  # check mapping corrected unit
  mapping <- stcm %>% dplyr::filter(
    .data$source_code == '11850',
    .data$source_vocabulary_id == "AUMC Numeric Unit"
  )
  # one row
  expect_equal(nrow(mapping), 1)
  # mapping of measurement unit (percent)
  expect_equal(mapping$target_concept_id, 8554)
})
