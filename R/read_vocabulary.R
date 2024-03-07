#' Read vocabulary
#'
#' @description
#' Reads the vocabulary from the one of the csv files in the Vocabulary path (
#' Default: `data/vocabulary`). If the file does not exist, it will show the
#' available csv files in that folder.
#'
#' @param vocabulary Vocabulary name, either the name of the vocabulary table
#' or name of the file. Case-insensitive.
#'
#' @return Tibble containing data from one of the vocabulary files
#' @export
#'
#' @examples
#' concept <- read_vocabulary("concept")
#' concept
read_vocabulary <- function(vocabulary) {
  vocabulary_path <- amstel_env$config$data$vocabulary

  file_list <- list.files(vocabulary_path, pattern = "*.csv")

  if (!grepl( ".csv", vocabulary, fixed = TRUE)) {
    matched = match(paste0(vocabulary, ".csv"), tolower(file_list))
  }
  else {
    matched = match(vocabulary, tolower(file_list))
  }

  if (!is.na(matched)) {
    vocabulary_file = paste0(vocabulary_path, "/", file_list[matched])
    vocabulary <- readr::read_tsv(file=vocabulary_file, show_col_types = FALSE)
    return(vocabulary)
  }
  else {
    message(paste0(
      "Vocabulary '", vocabulary, "' does not exist. ",
      "Please verify the name used.\n",
      "Available vocabulary files:\n",
      paste0("- ", file_list, collapse = "\n")
    ))
  }
}
