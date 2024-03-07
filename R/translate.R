#' Small wrapper for authentication https://github.com/ropensci/googleLanguageR.
#' Allows authentication using Google Cloud API credentials
#' in default user folder'
#'
#' @export
#'
#' @examples
#' cloud_auth()
cloud_auth <- function() {
  pkg_info <- get_pkg_info()

  GL_AUTH_FILE = "credentials.json"
  credentials_filepath = file.path(pkg_info$config_dir, GL_AUTH_FILE)

  googleLanguageR::gl_auth(credentials_filepath)
}


#' Small wrapper for translating using googleLanguageR.
#'
#' @param text Character vector to translate
#' @param source Source language using two letter code.
#' @param target Target language using two letter code. Default: 'en' for English
#'
#' @return translated character vector
#' @export
#'
#' @examples
#' translation <- translate("Mooi weer is het vandaag")
translate <- function(text, source="nl", target = "en") {

  # translate text using Google Translate Cloud API
  translated_text <- googleLanguageR::gl_translate(text, source=source, target=target)

  # remove translated entries when original concept was NA (incorrectly
  # translated to 'AFTER')
  translated_text <- translated_text %>%
    dplyr::mutate(translatedText = ifelse(
      is.na(text), NA, .data$translatedText))

  return(translated_text)
}
