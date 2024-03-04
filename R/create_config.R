#' Creates `amstel` configuration file
#'
#' @description
#' Creates the `config.yaml` configuration file in the default OS-defined
#' user configuration folder for setting up access to the databases and files
#  used during the ETL process.
#'
#' @export
#'
#' @examples
#' amstel::create_config()
create_config <- function(is_missing = FALSE) {
  pkg_info <- get_pkg_info()
  tryCatch(
    {
      if (!dir.exists(pkg_info$config_dir)) {
        dir.create(pkg_info$config_dir, recursive = TRUE)
      }
      res <- file.copy(pkg_info$config_pkg, pkg_info$config_dir)
    },
    error = function(c) {
      if (is_missing) {
        msg <- paste(
          "Configuration file missing.\n",
          "However, a new configuration file could not be created at: ", pkg_info$config_filepath, "\n"
        )
      } else {
        msg <- paste(
          "ERROR: a new configuration file could not be created at: ", pkg_info$config_filepath, "\n"
        )
      }
      stop(msg)
    }
  )
  if (!res) {
    # file copy skipped (already exists)
    msg <- paste("Configuration file already exists at:", pkg_info$config_filepath, "\n")
  }
  else {
    msg <- paste("A new configuration file has been created at:", pkg_info$config_filepath, "\n")
  }
  if (is_missing) {
    message(paste(
      "Configuration file missing.\n",
      msg,
      "Please open this file in your favourite editor and change the credentials",
      "for accessing the database(s) and try again."
    ))
  } else {
    message(msg)
  }

  # open the file an RStudio tab or window
  message("Opening configuration file in RStudio...")
  res_nav <- rstudioapi::navigateToFile(pkg_info$config_filepath)
}
