#' Launch Yield Cleaning Shiny App
#'
#' Launches an interactive Shiny application for visualizing yield data cleaning.
#' Users can upload files, see which points are removed and why, view yield maps,
#' and download cleaned data.
#'
#' @return Launches Shiny app in browser (invisible)
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
