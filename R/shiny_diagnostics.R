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

  # S'assurer que les donnees sont en format tibble, pas sf
  if (inherits(data_raw, "sf")) {
    data_raw <- sf::st_drop_geometry(data_raw)
  }

  if (!"orig_row_id" %in% names(data_raw)) {
    data_raw <- data_raw |>
      dplyr::mutate(orig_row_id = dplyr::row_number())
  }

  # Determiner la colonne de rendement a utiliser
  yield_col <- NULL
  if (isTRUE(metrique)) {
    if ("Yield_kg_ha" %in% names(data_raw)) {
      yield_col <- "Yield_kg_ha"
    } else if ("Yield_buacre" %in% names(data_raw)) {
      yield_col <- "Yield_buacre"
    }
  } else {
    if ("Yield_buacre" %in% names(data_raw)) {
      yield_col <- "Yield_buacre"
    } else if ("Yield_kg_ha" %in% names(data_raw)) {
      yield_col <- "Yield_kg_ha"
    }
  }

  # Si aucune colonne de rendement trouvee, utiliser Flow comme fallback
  if (is.null(yield_col)) {
    if ("Flow" %in% names(data_raw)) {
      yield_col <- "Flow"
    } else {
      return(list())  # Aucune donnee disponible
    }
  }

  base_data <- data_raw |>
    dplyr::select(orig_row_id, dplyr::all_of(yield_col))

  # Conversion des unites si necessaire
  if (yield_col == "Yield_buacre" && isTRUE(metrique)) {
    # Convertir bu/acre en kg/ha
    base_data <- base_data |>
      dplyr::mutate(
        valeur = !!rlang::sym(yield_col) * 67.25,
        unite = "kg/ha"
      )
  } else if (yield_col == "Yield_kg_ha" && !isTRUE(metrique)) {
    # Convertir kg/ha en bu/acre
    base_data <- base_data |>
      dplyr::mutate(
        valeur = !!rlang::sym(yield_col) / 67.25,
        unite = "bu/acre"
      )
  } else if (yield_col == "Flow") {
    # Flux en kg/s ou lb/s
    base_data <- base_data |>
      dplyr::mutate(
        valeur = if (isTRUE(metrique)) Flow * 0.453592 else Flow,
        unite = if (isTRUE(metrique)) "kg/s" else "lb/s"
      )
  } else {
    # Rendement deja dans la bonne unite
    base_data <- base_data |>
      dplyr::mutate(
        valeur = !!rlang::sym(yield_col),
        unite = if (isTRUE(metrique)) "kg/ha" else "bu/acre"
      )
  }

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
  # Determiner le label de l'axe X selon l'unite
  x_label <- if (unit_label %in% c("kg/s", "lb/s")) {
    paste0("Flux (", unit_label, ")")
  } else {
    paste0("Rendement (", unit_label, ")")
  }

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
      x = x_label,
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
 #' @param method Methode d'interpolation: "auto" (defaut), "tps", "idw", ou "nearest"
 #'   - "auto": Choisit automatiquement selon le nombre de points
 #'   - "tps": Thin Plate Spline (lent mais precis, max 5000 points)
 #'   - "idw": Inverse Distance Weighting (rapide, recommande)
 #'   - "nearest": Plus proche voisin (tres rapide)
 #' @param max_points_tps Nombre maximum de points pour TPS (defaut: 5000)
 #'   Si plus de points, echantillonnage aleatoire ou switch vers IDW
 #' @return Objet SpatRaster
 #' @export
export_raster <- function(data, cell_size = 1, column_colonne = "Yield_kg_ha",
                            fun = mean, crs_code = NULL, 
                            method = c("auto", "tps", "idw", "nearest"),
                            max_points_tps = 5000) {
  
  method <- match.arg(method)
  # Charger les packages necessaires
  if (!requireNamespace("terra", quietly = TRUE)) {
    stop("Le package 'terra' est requis pour cette fonction. Installez-le avec: install.packages('terra')")
  }
  
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("Le package 'sf' est requis pour cette fonction. Installez-le avec: install.packages('sf')")
  }
  
  # Charger explicitement les namespaces pour eviter les problemes de conversion
  requireNamespace("terra", quietly = TRUE)
  requireNamespace("sf", quietly = TRUE)

  # Verifier que data est un objet sf
  if (!inherits(data, "sf")) {
    stop("Les donnees doivent etre un objet sf")
  }

  # Verifier que la colonne de geometrie existe et n'est pas vide
  geom_check <- tryCatch({
    sf::st_geometry(data)
  }, error = function(e) {
    NULL
  })
  if (is.null(geom_check)) {
    stop("Aucune colonne de geometrie presente dans l'objet sf")
  }
  if (length(geom_check) == 0 || all(sf::st_is_empty(geom_check))) {
    stop("La colonne de geometrie est vide dans l'objet sf")
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

  # Convertir en terra - utiliser vect() directement
  message("Etape 1: Conversion sf -> terra...")
  
  # Conversion sf -> terra avec tryCatch pour capturer les erreurs
  vect_data <- tryCatch({
    v <- terra::vect(data)
    if (is.null(v) || terra::nrow(v) == 0) {
      stop("Conversion resultat vide")
    }
    message(paste("  -> Conversion reussie:", terra::nrow(v), "geometries"))
    message(paste("  -> Type de geometrie:", terra::geomtype(v)))
    message(paste("  -> CRS source:", terra::crs(v, describe = TRUE)$name))
    v
  }, error = function(e) {
    stop(paste("Erreur lors de la conversion sf vers terra:", e$message))
  })
  
  if (is.null(vect_data) || terra::nrow(vect_data) == 0) {
    stop("Echec de la conversion en SpatVector - aucune geometrie valide")
  }

  # Si polygones, convertir en centroides
  geom_types <- sf::st_geometry_type(data)
  if (any(geom_types %in% c("POLYGON", "MULTIPOLYGON"))) {
    message("Conversion des polygones en centroides...")
    vect_data <- terra::centroids(vect_data)
  }

  # Determiner le CRS et projeter en coordonnees metriques si necessaire
  utm_epsg <- NULL
  
  if (is.null(crs_code)) {
    crs_info <- sf::st_crs(data)
    current_epsg <- crs_info$epsg

    # Si les donnees sont en WGS84 (4326) ou sans CRS, projeter en UTM
    if (is.na(current_epsg) || current_epsg == 4326) {
      # Calculer la zone UTM appropriee
      coords_ll <- terra::geom(vect_data)[, c("x", "y")]
      mean_lon <- mean(coords_ll[, 1], na.rm = TRUE)
      mean_lat <- mean(coords_ll[, 2], na.rm = TRUE)
      utm_zone <- floor((mean_lon + 180) / 6) + 1
      utm_epsg <- ifelse(mean_lat < 0, 32700 + utm_zone, 32600 + utm_zone)

      message(paste("Projection en UTM zone", utm_zone, "(EPSG:", utm_epsg, ")"))

      # Projeter avec terra - utiliser tryCatch
      message("Etape 2: Projection terra...")
      vect_data <- tryCatch({
        v <- terra::project(vect_data, paste0("EPSG:", utm_epsg))
        if (is.null(v) || terra::nrow(v) == 0) {
          stop("Projection resultat vide")
        }
        message(paste("  -> Projection reussie:", terra::nrow(v), "points projetes"))
        message(paste("  -> CRS apres projection:", terra::crs(v, describe = TRUE)$name))
        v
      }, error = function(e) {
        stop(paste("Erreur lors de la projection UTM:", e$message))
      })
    }
  } else {
    message(paste("Projection en EPSG:", crs_code))
    utm_epsg <- crs_code
    # Projeter avec terra
    vect_data <- tryCatch({
      terra::project(vect_data, paste0("EPSG:", crs_code))
    }, error = function(e) {
      stop(paste("Erreur lors de la projection:", e$message))
    })
  }

  # Verifier que vect_data est valide
  if (is.null(vect_data) || terra::nrow(vect_data) == 0) {
    stop("Echec de la conversion en SpatVector")
  }

  # Verifier que les donnees contiennent des valeurs valides
  values_check <- terra::values(vect_data)[[column_colonne]]
  valid_count <- sum(!is.na(values_check))

  if (valid_count == 0) {
    stop(paste("Aucune valeur valide dans la colonne", column_colonne))
  }

  # Creer une grille raster vide
  message("Etape 3: Creation du template raster...")
  
  ext <- terra::ext(vect_data)
  message(paste("  -> Extension:", as.character(ext)))

  # Ajouter une marge
  margin <- cell_size * 5
  ext <- terra::extend(ext, margin)

  # Creer le raster template
  r_template <- terra::rast(
    ext = ext,
    resolution = cell_size,
    crs = terra::crs(vect_data)
  )
  message(paste("  -> Template cree:", terra::ncell(r_template), "cellules"))

  # Interpolation
  n_points <- terra::nrow(vect_data)
  message(paste("Etape 4: Interpolation (", n_points, "points )..."))
  message(paste("  - Resolution cellule:", cell_size, "m"))
  message(paste("  - Colonne utilisee:", column_colonne))
  
  # Determiner la methode d'interpolation
  use_tps <- FALSE
  if (method == "tps") {
    use_tps <- TRUE
  } else if (method == "auto") {
    # TPS uniquement si peu de points et package fields disponible
    use_tps <- (n_points <= max_points_tps) && requireNamespace("fields", quietly = TRUE)
    if (n_points > max_points_tps) {
      message(paste("  -> TPS desactive:", n_points, ">", max_points_tps, "points"))
    }
  }
  
  # Echantillonnage si trop de points pour TPS
  vect_data_interp <- vect_data
  if (use_tps && n_points > max_points_tps) {
    message(paste("  -> Echantillonnage:", max_points_tps, "sur", n_points, "points"))
    sample_idx <- sample(n_points, max_points_tps)
    vect_data_interp <- vect_data[sample_idx]
  }
  
  r <- NULL
  
  # Essayer TPS si approprie
  if (use_tps && requireNamespace("fields", quietly = TRUE)) {
    message("  -> Methode: Thin Plate Spline")
    r <- tryCatch({
      coords <- terra::geom(vect_data_interp)[, c("x", "y")]
      values <- terra::values(vect_data_interp)[[column_colonne]]
      
      message("     Creation du modele TPS...")
      tps_model <- fields::Tps(coords, values)
      
      message("     Interpolation sur la grille...")
      r_interpolated <- terra::interpolate(r_template, tps_model)
      
      message("     OK - TPS reussie")
      r_interpolated
    }, error = function(e) {
      message(paste("     ERREUR TPS:", e$message))
      NULL
    })
  }
  
  # Si TPS echoue ou non disponible, utiliser IDW
  if (is.null(r)) {
    if (method == "nearest") {
      message("  -> Methode: Nearest Neighbor (rapide)")
      r <- tryCatch({
        terra::interpNear(
          x = r_template,
          y = vect_data,
          field = column_colonne,
          radius = cell_size * 10
        )
      }, error = function(e) {
        message(paste("     ERREUR:", e$message))
        NULL
      })
    } else {
      message("  -> Methode: IDW (Inverse Distance Weighting)")
      r <- tryCatch({
        terra::interpIDW(
          x = r_template,
          y = vect_data,
          field = column_colonne,
          radius = cell_size * 15,
          power = 2
        )
      }, error = function(e) {
        message(paste("     ERREUR IDW:", e$message))
        message("  -> Fallback vers Nearest Neighbor...")
        NULL
      })
    }
  }
  
  # Fallback final: si tout echoue, utiliser Nearest Neighbor
  if (is.null(r)) {
    message("Etape 4b: Fallback final - Nearest Neighbor...")
    r <- tryCatch({
      terra::interpNear(
        x = r_template,
        y = vect_data,
        field = column_colonne,
        radius = cell_size * 20
      )
    }, error = function(e) {
      stop(paste("Erreur lors de l'interpolation:", e$message))
    })
  }

  # Creer un polygone de masque en utilisant terra directement
  message("Creation du polygone de masque...")

  # Creer un buffer autour des points pour definir la zone d'interpolation
  buffer_dist <- cell_size * 3
  
  mask_vect <- tryCatch({
    terra::vect(concaveman::concaveman(vect_data |> sf::st_as_sf() |> sf::st_centroid()))
  }, error = function(e) {
    message(paste("Avertissement lors du buffer:", e$message))
    # Si le buffer echoue, utiliser les points directement
    vect_data
  })

  # Fusionner tous les buffers en un seul polygone avec tryCatch
  mask_vect <- tryCatch({
    terra::aggregate(mask_vect)
  }, error = function(e) {
    message(paste("Avertissement lors de l'aggregation:", e$message))
    mask_vect  # Retourner le vecteur non aggrege si l'aggregation echoue
  })

  # Masquer le raster avec le polygone - avec tryCatch
  r_masked <- tryCatch({
    terra::mask(r, mask_vect)
  }, error = function(e) {
    message(paste("Avertissement lors du masquage:", e$message))
    r  # Retourner le raster non masque si le masquage echoue
  })

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
