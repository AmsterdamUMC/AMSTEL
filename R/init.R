# wrappers for logging different levels
log_debug <- function(message, ...) {
  logger <- amstel_env$logger
  log4r::debug(logger, message, ...)
}

log_info <- function(message, ...) {
  logger <- amstel_env$logger
  log4r::info(logger, message, ...)
}

log_warn <- function(message, ...) {
  logger <- amstel_env$logger
  log4r::warn(logger, message, ...)
}

log_fatal <- function(message, ...) {
  logger <- amstel_env$logger
  log4r::fatal(logger, message, ...)
}

console_layout <- function(level, ...) {
  paste0(..., "\n", collapse = "")
}

logfile_layout <- function(level, ...) {
  paste0(format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3%z"),
         ":", deparse(sys.call(-4)), ":", level, ":", ..., "\n", collapse = "")
}

# setup package environment
amstel_env <- new.env(parent = emptyenv())
amstel_env$config <- get_config()
amstel_env$log_file <- normalizePath(file.path(
  amstel_env$config$data$logs,
  paste0(amstel_env$config$package_info$package, ".log")
  ))
amstel_env$logger <- log4r::logger(
  threshold = amstel_env$config$logging$log_level,
  appenders = c(
    log4r::console_appender(layout = console_layout),
    log4r::file_appender(file = amstel_env$log_file,
                         layout = logfile_layout)
    )
  )

# first log item
log_debug(paste0("Amstel repository path: ", amstel_env$config$repository$path))
log_debug(paste0("JDBC folder: ", amstel_env$config$data$jdbc_jar_folder))

# logs standard output to log file (and console: split = TRUE)
# sink(file = amstel_env$log_file, append = TRUE, type = "output", split = TRUE)

load_sql <- function(sql_file) {
  file <- system.file(file.path('sql', sql_file),
                      package = amstel_env$config$package_info$package)
  return(paste(readLines(file), collapse = "\n"))
}
