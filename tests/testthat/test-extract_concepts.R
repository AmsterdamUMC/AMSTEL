test_that("Source concept extraction", {
  library(dplyr)
  con <- amstel::connect("amsterdamumcdb")
  admissions <- tbl(con, "admissions")

  concepts <- admissions %>%
    group_by(source_concept=origin) %>%
    summarise(count=n()) %>%
    collect()

  concepts_extract <- extract_concepts(concepts, "admissions_origin")

  # verify that the new tibble contains these three columns
  expect_equal(
    concepts_extract %>% colnames(),
    c("source_concept", "count", "source_concept_en")
  )
})
