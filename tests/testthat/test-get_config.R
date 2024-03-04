test_that("get_configuration works", {
  # reads config file.
  pkg_info <- get_pkg_info()
  if (!file.exists(pkg_info$config_filepath)) {
    expect_error(get_config())
  }
  else {
    config <- get_config()
    expect_equal(length(config$postgres), 5)
  }
})
