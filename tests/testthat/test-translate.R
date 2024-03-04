test_that("translation works", {
  cloud_auth()
  translation <- amstel::translate("Het is mooi weer vandaag.")
  expect_equal(translation$translatedText, "The weather is beautiful today.")
})
