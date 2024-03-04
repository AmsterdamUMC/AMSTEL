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
  for (db in amstel_env$config) {
    dbms <- c(dbms, (db$dbms))
  }
  dbms <- unique(dbms)

  # download drivers for used dbms
  DatabaseConnector::downloadJdbcDrivers(dbms, pathToDriver = config$data$jdbc_jar_folder)
}
