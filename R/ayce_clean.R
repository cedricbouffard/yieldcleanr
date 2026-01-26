#' AYCE: Auto Yield Cleaning Engine (Imperial Output)
#'
#' Système expert automatisé pour le nettoyage des données de rendement
#' sans intervention humaine, basé sur les méthodes USDA Yield Editor.
#' Cette fonction retourne un tibble avec les unités impériales (bu/acre).
#'
#' @param file_path Path to input file (txt format)
#' @param output_file Path to output CSV file
#' @param log_file Path to output log file
#' @param params List of AYCE parameters
#' @return Cleaned tibble with imperial units
#' @noRd
#' @examples
#' \dontrun{
#' cleaned <- ayce_clean("data/original.txt")
#' }
ayce_clean <- function(file_path, output_file = NULL, log_file = NULL,
                       params = NULL) {
  clean_yield(file_path = file_path,
              metrique = FALSE,
              polygon = FALSE,
              params = params,
              output_file = output_file,
              log_file = log_file)
}


#' Quick AYCE cleaning with defaults
#'
#' Wrapper for ayce_clean with minimal parameters.
#'
#' @param file_path Path to input file
#' @param output_file Path to output file
#' @return Cleaned tibble
#' @noRd
quick_ayce <- function(file_path, output_file = NULL) {
  ayce_clean(file_path = file_path, output_file = output_file)
}
