#' Gets the configuration for working with the amstel package. Reads the
#' config.yaml file from the user config folder or creates a new file if the
#' configuration file is missing.
#'
#' @return list containing configuration settings
#' @export
#'
#' @examples
#' config <- get_config()
get_config <- function() {
  pkg_info <- get_pkg_info()

  # if file is missing from the OS-dependent user config folder, copy it
  # to that location
  if (!file.exists(pkg_info$config_filepath)) {
    create_config(is_missing=TRUE)
  }
  else {
    config <- yaml::read_yaml(file=pkg_info$config_filepath)
    config <- verify_config(config)
    config$package_info$package <- pkg_info$package
    config$package_info$organization <- pkg_info$organization
    return(config)
  }
}

# returns the absolute path to the base/root of this repository
# if the path is a relative path (or even just the current directory '.')
# tries to locate the path by traversing through the parent directories
# This is mainly needed to allow scripts in subdirectories to find the data
# files
get_base_path <- function(path) {
  base_file <- "amstel.Rproj"
  base_path <- normalizePath(path)
  verify_file <- normalizePath(base_path, base_file)
  while (!file.exists(verify_file)) {
    base_path <- normalizePath(base_path, "..")
    message(base_path)
    verify_file <- normalizePath(base_path, base_file)
    if (base_path == "/") {
      stop("The `amstel.Rproj` file could not be found. This package should be
           loaded from within the cloned GitHub respository, or modify the
           `repository/path` section in the `config.yaml` file.")
    }
  }
  return(base_path)
}

get_pkg_info <- function() {
  CONFIG_SAMPLE_FILE <- "config.SAMPLE.yaml"
  CONFIG_FILE <- "config.yaml"

  # package info based on DESCRIPTION file
  description_pkg = system.file("DESCRIPTION", package=pkgload::pkg_name())
  description <- read.dcf(description_pkg, fields = c("Package", "Organization"))
  package <- description[[1,"Package"]]
  organization <- description[[1,"Organization"]]

  # user config folder based on operating system
  config_dir <- rappdirs::user_config_dir(
    appname=package,
    appauthor=organization
  )

  # full path to the config file
  config_filepath <- file.path(config_dir, CONFIG_FILE)

  # the 'sample' config file as stored in this package
  config_pkg <- system.file(CONFIG_SAMPLE_FILE, package=package)

  return(
    list(
      config_dir=config_dir,
      config_filepath=config_filepath,
      config_pkg=config_pkg,
      package=package,
      organization=organization
    )
  )
}

get_connection_details <- function(database) {
  db_config <- amstel_env$config$database[[database]]
  return(
    DatabaseConnector::createConnectionDetails(
      dbms = db_config$dbms,
      user = db_config$user,
      password = db_config$password,
      server = db_config$server,
      port = db_config$port,
      pathToDriver = amstel_env$config$data$jdbc_jar_folder
    )
  )
}

verify_config <- function(config) {
  # change all values to absolute path and check if directory exists
  config$repository$path <- get_base_path(config$repository$path)

  for (element in names(config$data)) {
    data_path <- config$data[[element]]

    if (!grepl("(^/)|(^[a-zA-Z]:[\\\\/])|(^\\\\\\\\)", data_path, perl = TRUE)) { # not already absolute path
      # Absolute paths:
      # Unix: "^/" (starts with "/")
      # Windows: "^[a-zA-Z]:[\\\\/]" (starts with drive letter, colon,
      #          backslash/forward slash)
      # Windows UNC path: "^\\\\\\\\" (starts with "\\")

      # set data path relative to base path of repository
      abs_path <- normalizePath(file.path(config$repository$path,
                                          data_path))
    }
    if (!dir.exists(abs_path))
      stop(paste0(
        "Configuration of '", element, "' is invalid (",
        element, ": ", config$data[[element]],
        " -> ", abs_path, " )\n",
        "Directory does not exist. Please make sure you run the package ",
        "from the repository, or set the `repository/path` section in the ",
        "`config.yaml` file to an absolute path on your file system."))

    config$data[element] <- abs_path
  }
  return(config)
}
