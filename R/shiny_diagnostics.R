#' Construire les donnees de diagnostic par filtre
#'
#' Prepare un jeu de donnees par etape de filtrage afin de faciliter
#' l'affichage des diagnostics dans l'application Shiny.
#'
#' @param data_raw Tibble avec au minimum orig_row_id et Flow.
#' @param deletions Tibble des suppressions avec orig_row_id et step.
#' @param metrique TRUE pour convertir le flux en kg/s, FALSE pour lb/s.
#' @return Liste de tibbles, une par etape, avec les colonnes:
#'   - orig_row_id
#'   - etape
#'   - statut (Conserve/Supprime)
#'   - valeur (flux)
#'   - unite (kg/s ou lb/s)
#' @noRd
build_filter_diagnostics <- function(data_raw, deletions, metrique = TRUE) {
  if (is.null(data_raw) || is.null(deletions) || nrow(deletions) == 0) {
    return(list())
  }

  if (!"orig_row_id" %in% names(data_raw)) {
    data_raw <- data_raw |>
      dplyr::mutate(orig_row_id = dplyr::row_number())
  }

  base_data <- data_raw |>
    dplyr::select(orig_row_id, Flow)

  base_data <- base_data |>
    dplyr::mutate(
      valeur = if (isTRUE(metrique)) Flow * 0.453592 else Flow,
      unite = if (isTRUE(metrique)) "kg/s" else "lb/s"
    )

  steps <- unique(deletions$step)

  diagnostics <- lapply(steps, function(step_name) {
    removed_ids <- deletions |>
      dplyr::filter(step == step_name) |>
      dplyr::pull(orig_row_id)

    base_data |>
      dplyr::mutate(
        etape = step_name,
        statut = dplyr::if_else(orig_row_id %in% removed_ids, "Supprime", "Conserve")
      )
  })

  names(diagnostics) <- steps
  diagnostics
}
