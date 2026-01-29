#' AYCE : Auto Yield Cleaning Engine (sortie imperiale)
#'
#' Systeme expert automatise pour le nettoyage des donnees de rendement
#' sans intervention humaine, base sur les methodes USDA Yield Editor.
#' Cette fonction retourne un tibble en unites imperiales (bu/acre).
#'
#' @param file_path Chemin du fichier d'entree (txt)
#' @param output_file Chemin du fichier CSV de sortie
#' @param log_file Chemin du journal de sortie
#' @param params Liste des parametres AYCE
 #' @return Tibble nettoye en unites imperiales
 #' @export
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


#' Nettoyage AYCE rapide avec valeurs par defaut
#'
#' Enveloppe de ayce_clean avec un minimum de parametres.
#'
#' @param file_path Chemin du fichier d'entree
#' @param output_file Chemin du fichier de sortie
#' @return Tibble nettoye
#' @noRd
quick_ayce <- function(file_path, output_file = NULL) {
  ayce_clean(file_path = file_path, output_file = output_file)
}
