#' Lancer l'application Shiny de nettoyage des rendements
#'
#' Lance une application Shiny interactive pour visualiser le nettoyage des
#' donnees de rendement. L'utilisateur peut importer un fichier, voir les
#' suppressions par etape, visualiser les cartes et telecharger les resultats.
#'
#' @return Lance l'application Shiny dans le navigateur (invisible)
#' @export
#' @examples
#' \dontrun{
#' launch_shiny_app()
#' }
launch_shiny_app <- function() {
  app_dir <- system.file("shinyapp", package = "yieldcleanr")
  if (app_dir == "" || !dir.exists(app_dir)) {
    stop("Shiny app directory not found. Please reinstall the package.")
  }
  
  shiny::runApp(app_dir, launch.browser = TRUE)
}
