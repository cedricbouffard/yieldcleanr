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

#' Creer un graphique de diagnostic informatif
#'
#' Genere un graphique complet avec histogrammes, statistiques et informations
#' sur la distribution des valeurs conservees et supprimees.
#'
#' @param diag_data Donnees de diagnostic pour une etape specifique
#' @param step_name Nom de l'etape de filtrage
#' @param base_size Taille de base pour les polices
#' @return Objet ggplot
#' @noRd
create_diagnostic_plot <- function(diag_data, step_name, base_size = 11) {
  if (is.null(diag_data) || nrow(diag_data) == 0) {
    return(ggplot2::ggplot() + 
             ggplot2::annotate("text", x = 0.5, y = 0.5, 
                              label = "Pas de donnees disponibles") +
             ggplot2::theme_void())
  }
  
  plot_data <- diag_data |>
    dplyr::filter(is.finite(valeur)) |>
    dplyr::mutate(
      statut = factor(statut, levels = c("Conserve", "Supprime"))
    )
  
  if (nrow(plot_data) == 0) {
    return(ggplot2::ggplot() + 
             ggplot2::annotate("text", x = 0.5, y = 0.5, 
                              label = "Pas de valeurs valides") +
             ggplot2::theme_void())
  }
  
  # Calcul des statistiques
  removed_count <- sum(plot_data$statut == "Supprime", na.rm = TRUE)
  kept_count <- sum(plot_data$statut == "Conserve", na.rm = TRUE)
  total_count <- removed_count + kept_count
  removal_rate <- ifelse(total_count > 0, round(removed_count / total_count * 100, 1), 0)
  
  kept_data <- plot_data |> dplyr::filter(statut == "Conserve")
  removed_data <- plot_data |> dplyr::filter(statut == "Supprime")
  
  # Statistiques pour les valeurs conservees
  kept_stats <- if (nrow(kept_data) > 0) {
    list(
      mean = round(mean(kept_data$valeur, na.rm = TRUE), 2),
      median = round(median(kept_data$valeur, na.rm = TRUE), 2),
      sd = round(stats::sd(kept_data$valeur, na.rm = TRUE), 2),
      q25 = round(stats::quantile(kept_data$valeur, 0.25, na.rm = TRUE), 2),
      q75 = round(stats::quantile(kept_data$valeur, 0.75, na.rm = TRUE), 2)
    )
  } else {
    list(mean = NA, median = NA, sd = NA, q25 = NA, q75 = NA)
  }
  
  # Statistiques pour les valeurs supprimees
  removed_stats <- if (nrow(removed_data) > 0) {
    list(
      mean = round(mean(removed_data$valeur, na.rm = TRUE), 2),
      min = round(min(removed_data$valeur, na.rm = TRUE), 2),
      max = round(max(removed_data$valeur, na.rm = TRUE), 2)
    )
  } else {
    list(mean = NA, min = NA, max = NA)
  }
  
  unit_label <- unique(plot_data$unite)
  
  # Creer le graphique avec deux panneaux
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = valeur, fill = statut)) +
    ggplot2::geom_histogram(alpha = 0.7, bins = 40, position = "identity") +
    ggplot2::scale_fill_manual(
      values = c("Conserve" = "#5b6470", "Supprime" = "#b04a3b"),
      labels = c(paste0("Conserve (n=", kept_count, ")"), 
                 paste0("Supprime (n=", removed_count, ")"))
    ) +
    ggplot2::labs(
      title = step_name,
      subtitle = paste0("Taux de suppression: ", removal_rate, "% | ",
                       "Total: ", total_count, " points"),
      x = paste0("Flux (", unit_label, ")"),
      y = "Frequence",
      caption = paste0(
        "Conserve - Moy: ", kept_stats$mean, " | Med: ", kept_stats$median,
        " | ET: ", kept_stats$sd, " | Q1-Q3: [", kept_stats$q25, " - ", kept_stats$q75, "]\n",
        "Supprime - Moy: ", removed_stats$mean, " | Min: ", removed_stats$min, " | Max: ", removed_stats$max
      )
    ) +
    ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      legend.position = "top",
      legend.title = element_blank(),
      plot.title = ggplot2::element_text(face = "bold", size = base_size + 4),
      plot.subtitle = ggplot2::element_text(color = "#5b6470", size = base_size + 1),
      plot.caption = ggplot2::element_text(color = "#5b6470", size = base_size - 2, hjust = 0),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank()
    )
  
  # Ajouter des lignes verticales pour les statistiques si possible
  if (nrow(kept_data) > 0 && !is.na(kept_stats$median)) {
    p <- p + ggplot2::geom_vline(
      ggplot2::aes(xintercept = kept_stats$median, linetype = "Mediane"),
      color = "#2f6f6d", linewidth = 0.8, alpha = 0.8
    )
  }
  
  if (nrow(kept_data) > 0 && !is.na(kept_stats$mean)) {
    p <- p + ggplot2::geom_vline(
      ggplot2::aes(xintercept = kept_stats$mean, linetype = "Moyenne"),
      color = "#b7832f", linewidth = 0.8, alpha = 0.8
    )
  }
  
  # Ajouter la legende des lignes si des lignes ont ete ajoutees
  if ((nrow(kept_data) > 0 && !is.na(kept_stats$median)) || 
      (nrow(kept_data) > 0 && !is.na(kept_stats$mean))) {
    p <- p + ggplot2::scale_linetype_manual(
      name = "Statistiques",
      values = c("Mediane" = "solid", "Moyenne" = "dashed")
    )
  }
  
  p
}

#' Exporter les donnees nettoyees en raster
#'
#' Convertit les donnees de rendement nettoyees en raster avec une resolution
#' specifique et decoupe selon un polygone concave.
#'
#' @param data Donnees nettoyees (objet sf avec geometrie points ou polygones)
#' @param cell_size Taille des cellules en metres (defaut: 1)
#' @param column_colonne Nom de la colonne contenant les valeurs a rasteriser
#' @param fun Fonction d'agregation (defaut: mean)
#' @param crs_code Code EPSG du systeme de coordonnees (defaut: NULL, auto-detecte)
#' @return Objet SpatRaster
#' @export
export_raster <- function(data, cell_size = 1, column_colonne = "Yield_kg_ha", 
                          fun = mean, crs_code = NULL) {
  if (!requireNamespace("terra", quietly = TRUE)) {
    stop("Le package 'terra' est requis pour cette fonction. Installez-le avec: install.packages('terra')")
  }
  if (!requireNamespace("concaveman", quietly = TRUE)) {
    stop("Le package 'concaveman' est requis pour cette fonction. Installez-le avec: install.packages('concaveman')")
  }
  
  # Verifier que data est un objet sf
  if (!inherits(data, "sf")) {
    stop("Les donnees doivent etre un objet sf")
  }
  
  # Verifier que la colonne existe
  if (!column_colonne %in% names(data)) {
    # Essayer de trouver une colonne de rendement alternative
    alt_cols <- c("Yield_kg_ha", "Yield_buacre", "Yield", "yield", "Rendement")
    found_col <- alt_cols[alt_cols %in% names(data)]
    if (length(found_col) > 0) {
      column_colonne <- found_col[1]
      message(paste0("Colonne '", column_colonne, "' utilisee pour le raster"))
    } else {
      stop(paste0("Colonne '", column_colonne, "' non trouvee dans les donnees"))
    }
  }
  
  # Convertir en points si ce sont des polygones
  if (any(sf::st_geometry_type(data) %in% c("POLYGON", "MULTIPOLYGON"))) {
    data <- sf::st_centroid(data)
  }
  
  # Determiner le CRS
  if (is.null(crs_code)) {
    crs_info <- sf::st_crs(data)
    if (is.na(crs_info$epsg)) {
      # Projecter en UTM si necessaire
      data <- latlon_to_utm(data)
    }
  } else {
    data <- sf::st_transform(data, crs_code)
  }
  
  # Extraire les coordonnees et valeurs
  coords <- sf::st_coordinates(data)
  values <- data[[column_colonne]]
  
  # Creer un SpatVector pour terra
  vect_data <- terra::vect(data)
  
  # Creer une grille raster vide
  ext <- terra::ext(vect_data)
  
  # Ajouter une marge
  margin <- cell_size * 5
  ext <- terra::extend(ext, margin)
  
  # Creer le raster template
  r_template <- terra::rast(
    ext = ext,
    resolution = cell_size,
    crs = terra::crs(vect_data)
  )
  
  # Rasteriser les points
  r <- terra::rasterize(
    x = vect_data,
    y = r_template,
    field = column_colonne,
    fun = fun
  )
  
  # Creer le polygone concave
  coords_df <- as.data.frame(coords)
  names(coords_df) <- c("x", "y")
  
  # Utiliser concaveman pour creer un polygone concave
  concave_poly <- concaveman::concaveman(coords_df, concavity = 2)
  
  # Convertir en objet sf
  concave_sf <- sf::st_as_sf(concave_poly, coords = c("x", "y"), crs = sf::st_crs(data))
  concave_sf <- sf::st_combine(concave_sf) |> 
    sf::st_cast("POLYGON") |>
    sf::st_sf()
  
  # Convertir en SpatVector pour terra
  concave_vect <- terra::vect(concave_sf)
  
  # Masquer le raster avec le polygone concave
  r_masked <- terra::mask(r, concave_vect)
  
  # Ajouter des metadonnees
  names(r_masked) <- column_colonne
  terra::units(r_masked) <- ifelse(grepl("kg", column_colonne), "kg/ha", "bu/acre")
  
  return(r_masked)
}

#' Sauvegarder un raster en fichier
#'
#' @param raster Objet SpatRaster
#' @param file_path Chemin du fichier de sortie
#' @param format Format de sortie ("tif", "asc", "grd")
#' @export
save_raster <- function(raster, file_path, format = "tif") {
  if (!requireNamespace("terra", quietly = TRUE)) {
    stop("Le package 'terra' est requis pour cette fonction")
  }
  
  # Determiner le driver en fonction du format
  driver <- switch(format,
                   "tif" = "GTiff",
                   "asc" = "AAIGrid",
                   "grd" = "RRASTER",
                   "GTiff")
  
  terra::writeRaster(raster, file_path, filetype = driver, overwrite = TRUE)
  message(paste0("Raster sauvegarde: ", file_path))
}
