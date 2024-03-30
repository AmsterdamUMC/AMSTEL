#' Extract concepts
#' @description
#' Create source concept lists for mapping in OHDSI Usagi. Translates column_name into
#' English using the Google Translate Cloud API for matching using OHDSI Usagi.
#' Exports the tibble to the `source_concepts` folder.
#'
#' @param concepts
#' tibble containing concepts (column name: "source_concept") and the number
#' of occurrences in the database (column name: "count") of those concepts.
#' @param concept_group
#' Name this group of concepts should be called for extracting
#' @param force
#' Forces overwriting an existing concept list. Default: FALSE.
#'
#' @return tibble containing concepts and their translation
#' @export
#'
#' @examplesIf has_adb_environment()
#' library(dplyr)
#' conn <- amstel::connect("amsterdamumcdb")
#' admissions <- tbl(conn, "admissions")
#' concepts <- admissions %>%
#'   group_by(source_concept=origin) %>%
#'   summarise(count=n()) %>%
#'   collect()
#' concepts <- extract_concepts(concepts, "admissions_origin")
#' concepts
extract_concepts <- function(concepts, concept_group, force=FALSE) {
  concepts <- amstel::translate_concepts(concepts)
  concepts <- amstel::save_concepts(concepts, concept_group, force)
  return(concepts)
}

#' Translate concepts
#' @description
#' Translates column_name into English using the Google Translate Cloud API for
#' matching using OHDSI Usagi.
#'
#' @param concepts tibble containing concepts (column name: "source_concept")
#'   and the number of occurrences in the database (column name: "count") of
#'   those concepts.
#'
#' @return tibble containing concepts and their translation
#' @export
#'
#' @examples has_adb_environment()
#' library(dplyr)
#' conn <- amstel::connect("amsterdamumcdb")
#' admissions <- tbl(conn, "admissions")
#' concepts <- admissions %>% group_by(source_concept=origin) %>% summarise(count=n()) %>% collect()
#' concepts <- translate_concepts(concepts)
#' concepts
translate_concepts <- function(concepts) {
  # authenticate with Google Translate Cloud API
  amstel::cloud_auth()

  # ungroup tibble (if applicable) to allow assigning translated column to
  # new column
  concepts <- concepts %>% dplyr::ungroup()

  # translate the column
  translation <- amstel::translate(concepts$source_concept)

  concepts <- concepts %>% dplyr::mutate(source_concept_en = translation$translatedText)

  # return translated concepts
  return(concepts)
}

#' Save Concepts
#' @description
#' Saves the concepts list into the source_concepts folder
#'
#' @param concepts tibble containing the concepts
#' @param concept_group concept group name that will be used to create the file name
#' @param force forces overwriting file. WARNING: this may delete already mapped concepts
#'
#' @return (unmodified) tibble containing the concepts
#' @export
#'
#' @examplesIf has_adb_environment()
#' library(dplyr)
#' conn <- amstel::connect("amsterdamumcdb")
#' admissions <- tbl(conn, "admissions")
#' concepts <- admissions %>%
#'   group_by(source_concept=origin) %>%
#'   summarise(count=n()) %>%
#'   collect()
#' concepts <- save_concepts(concepts, "admissions_origin")
#' concepts
save_concepts <- function(concepts, concept_group, force = FALSE) {
  SOURCE_CONCEPTS_FOLDER = amstel_env$config$data$source_concepts

  tryCatch(
    {
      # create folder if it does not already exist
      if (!dir.exists(SOURCE_CONCEPTS_FOLDER)) {
        dir.create(SOURCE_CONCEPTS_FOLDER, recursive = TRUE)
      }

      # write file if it does not exist to prevent already mapped concepts
      # please manually delete the file if extraction is desired
      output_file <- file.path(SOURCE_CONCEPTS_FOLDER, paste0(concept_group, ".csv"))
      if (file.exists(output_file) && force == FALSE) {
        stop(paste(
          "WARNING: file already exists at: ", normalizePath(output_file), "\n",
          "Concepts not extracted. Please delete the file if re-extraction is desired."
        ))
      }
      else {
        # save concepts
        concepts %>% readr::write_csv(
          file=output_file,
          na = "",
          quote = "needed")
      }

    },
    error = function(err) {
      if (err$message != "") {
        message(err$message)
      }
      else {
        message(paste0(
          "Source concept file could not be saved at: ", normalizePath(output_file), "\n"))
      }
    }
  )
  # return (translated) concepts
  return(concepts)
}


#' Load concepts
#' @description
#' Reads a (previously generated) concept list by based on the concept group name
#'
#' @param concept_group concept group name that will be used to read the file
#'
#' @return tibble containing the concepts belonging to this group
#' @export
#'
#' @examples
#' providers <- load_concepts("providers")
#' providers
load_concepts <- function(concept_group) {
  SOURCE_CONCEPTS_FOLDER = amstel_env$config$data$source_concepts

  tryCatch(
    {
      input_file <- file.path(SOURCE_CONCEPTS_FOLDER, paste0(concept_group, ".csv"))
      if (!file.exists(input_file)) {
        stop(paste(
          "File not found at: ", normalizePath(input_file)
        ))
      }
      else {
        concepts <- readr::read_csv(
          file = input_file,
          show_col_types = FALSE)
        # return translated concepts
        return(concepts)
      }

    },
    error = function(err) {
      if (err$message != "") {
        message(err$message)
      }
    }
  )
}

#' Load Usagi Concepts
#' @description
#' Reads a (partially mapped) concept list based on the concept group name. This
#' function assumes the mapped concept list had been stored in the `data/mappings`
#' folder.
#'
#' @param concept_group concept group name that will be used to read the file
#'
#' @return tibble containing the concepts belonging to this group
#' @export
#'
#' @examples
#' providers <- load_usagi_concepts("providers")
#' providers
load_usagi_concepts <- function(concept_group) {
  MAPPINGS_FOLDER = amstel_env$config$data$mappings

  tryCatch(
    {
      input_file <- file.path(MAPPINGS_FOLDER, paste0(concept_group, ".usagi.csv"))
      if (!file.exists(input_file)) {
        stop(paste(
          "File not found at: ", normalizePath(input_file)
        ))
      }
      else {
        concepts <- readr::read_csv(
          col_types = list("comment" = readr::col_character()),
          file = input_file,
          show_col_types = FALSE)
        # return translated concepts
        return(concepts)
      }

    },
    error = function(err) {
      if (err$message != "") {
        message(err$message)
      }
    }
  )
}

#' Swap counts
#'
#' @description
#' Swaps the sourceFrequency and counts/counts_validated columns to allow
#' correct sorting in OHDSI Usagi. This is mainly used to help prioritizing
#' mapping to (manually) validated values vs not-validated (device) data.
#'
#' @param concept_group concept group name that will be used to read the file
#'
#' @return tibble containing the Usagi mappings with counts swapped
#' @export
#'
swap_usagi_counts <- function(concept_group) {
  usagi_mappings <- load_usagi_concepts(concept_group)

  if("ADD_INFO:count_validated" %in% colnames(usagi_mappings))  {
    values_count_validated <- usagi_mappings$`ADD_INFO:count_validated`
    values_count <- usagi_mappings$sourceFrequency

    usagi_mappings <- usagi_mappings %>%
      dplyr::rename(`ADD_INFO:count` = .data$`ADD_INFO:count_validated`) %>%
      dplyr::mutate(
        sourceFrequency = values_count_validated,
        `ADD_INFO:count` = values_count)
  } else if("ADD_INFO:count" %in% colnames(usagi_mappings)) {
    values_count_validated <- usagi_mappings$sourceFrequency
    values_count <- usagi_mappings$`ADD_INFO:count`
    usagi_mappings <- usagi_mappings %>%
      dplyr::rename(`ADD_INFO:count_validated` = .data$`ADD_INFO:count`) %>%
      dplyr::mutate(
        sourceFrequency = values_count,
        `ADD_INFO:count_validated` = values_count_validated)
  }

  # save the file
  MAPPINGS_FOLDER = amstel_env$config$data$mappings

  tryCatch(
    {
      output_file <- file.path(MAPPINGS_FOLDER, paste0(
        concept_group, ".usagi.csv"))
      usagi_mappings %>% readr::write_csv(
          file=output_file,
          na = "",
          quote = "needed")
    },
    error = function(err) {
      if (err$message != "") {
        message(err$message)
      }
      else {
        message(paste0(
          "Usagi mappings file could not be saved at: ", normalizePath(output_file), "\n"))
      }
    }
  )
  # return swapped usagi mappings
  return(usagi_mappings)
}
