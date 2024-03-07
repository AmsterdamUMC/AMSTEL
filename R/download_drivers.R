#' Downloads JDBC drivers
#'
#' @description
#' Downloads the required JDBC drivers for connecting to the database
#' management system(s) as specified by the `config.yaml` file.
#'
#' @export
#'
#' @examples
#' amstel::download_drivers()
download_drivers <- function() {
  # enumerate database management systems defined in config
  dbms <- c()
  for (db in amstel_env$config$databases) {
    dbms <- c(dbms, (db$dbms))
  }
  dbms <- unique(dbms)

  # download drivers for used dbms
  jdbc_jar_folder <- amstel_env$config$data$jdbc_jar_folder
  tryCatch(
    DatabaseConnector::downloadJdbcDrivers(dbms, pathToDriver = jdbc_jar_folder),
    message = function(msg) {
      home <- path.expand('~')
      message <- sub(home, '~', msg$message)
      log_info(message)
    }
  )
}
