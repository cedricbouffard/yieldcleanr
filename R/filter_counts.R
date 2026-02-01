#' Calculer le nombre de points retires par chaque filtre
#'
#' Cette fonction calcule combien de points seraient retires par chaque filtre
#' sans les appliquer reellement. Utile pour afficher le nombre de points
#' retires dans l'interface utilisateur avant le traitement.
#'
#' @param data Donnees brutes (tibble)
#' @param params Liste des parametres de filtrage
 #' @return Liste avec le nombre de points retires par filtre
 #' @export
 calculate_filter_counts <- function(data, params = NULL) {
  if (is.null(data) || nrow(data) == 0) {
    return(list())
  }

  # S'assurer que les donnees sont en format tibble, pas sf
  if (inherits(data, "sf")) {
    data <- sf::st_drop_geometry(data)
  }

  counts <- list()

  # S'assurer que data a orig_row_id
  if (!"orig_row_id" %in% names(data)) {
    data <- data |> dplyr::mutate(orig_row_id = dplyr::row_number())
  }

  # Conversion UTM si necessaire
  has_xy <- all(c("X", "Y") %in% names(data))
  has_latlon <- all(c("Latitude", "Longitude") %in% names(data))

  if (!has_xy && has_latlon) {
    data <- latlon_to_utm(data)
    # S'assurer que latlon_to_utm ne retourne pas un objet sf
    if (inherits(data, "sf")) {
      data <- sf::st_drop_geometry(data)
    }
   } else if (!has_xy && !has_latlon) {
     rlang::warn("Colonnes Latitude/Longitude ou X/Y requises pour le calcul des filtres - resultat partiel")
     return(list(rows_removed = 0, retention_rate = 1))
   }

   # Etape 5 : filtre header
   if ("HeaderStatus" %in% names(data)) {
     to_keep <- dplyr::filter(data, HeaderStatus %in% c(0, 1, 33) | is.na(HeaderStatus))
     counts$header <- nrow(data) - nrow(to_keep)
   }

   # Etape 6 : filtre GPS
   if ("GPSStatus" %in% names(data)) {
     data$GPSStatus <- suppressWarnings(as.numeric(data$GPSStatus))
     to_keep <- dplyr::filter(data, is.na(GPSStatus) | GPSStatus >= 4)
     counts$gps <- nrow(data) - nrow(to_keep)
   }

   # Etape 7-8 : calcul vitesse et filtre vitesse
   if (has_xy) {
     data <- data |>
       dplyr::mutate(
         velocity = sqrt((X - dplyr::lag(X))^2 + (Y - dplyr::lag(Y))^2) /
                       dplyr::coalesce(Interval, 1)
       )
     data$velocity[is.na(data$velocity) | !is.finite(data$velocity)] <- 0

     # Calculer les seuils automatiques
     thresholds <- calculate_auto_thresholds(data,
       yllim = params$yllim %||% 0.10,
       yulim = params$yulim %||% 0.90,
       yscale = params$yscale %||% 1.1,
       vllim = params$v_lim %||% 0.05,
       vulim = params$v_ulim %||% 0.95,
       vscale = params$v_scale %||% 1.1,
       minv_abs = params$minv_abs %||% 0.5,
       miny_abs = params$miny_abs %||% 0,
       gbuffer = params$gbuffer %||% 100
     )

     to_keep <- dplyr::filter(data, velocity >= thresholds$min_velocity & velocity <= thresholds$max_velocity)
     counts$velocity <- nrow(data) - nrow(to_keep)
   }

  # Etape 8b : filtre changements brusques de vitesse
  if (isTRUE(params$apply_velocity_jump)) {
    result <- filter_velocity_jumps(
      data,
      max_acceleration = params$max_acceleration %||% 3,
      max_deceleration = params$max_deceleration %||% -5
    )
    counts$velocity_jump <- nrow(result$removed)
  }

  # Etape 8c : filtre anomalies de direction
  if (isTRUE(params$apply_heading_anomaly)) {
    result <- filter_heading_anomalies(
      data,
      max_heading_change = params$max_heading_change %||% 60
    )
    counts$heading_anomaly <- nrow(result$removed)
  }

  # Etape 10 : suppression des rendements nuls
  if ("Yield_kg_ha" %in% names(data)) {
    to_keep <- dplyr::filter(data, Yield_kg_ha > 0)
    counts$null_yield <- nrow(data) - nrow(to_keep)
  }

  # Etape 11 : filtre plage de rendement
  if ("Yield_kg_ha" %in% names(data)) {
    to_keep <- dplyr::filter(data,
      Yield_kg_ha >= thresholds$min_yield &
      Yield_kg_ha <= thresholds$max_yield)
    counts$yield_range <- nrow(data) - nrow(to_keep)
  }

  # Etape 12 : filtre humidite
  if ("Moisture" %in% names(data) && !all(is.na(data$Moisture))) {
    moisture_mean <- mean(data$Moisture, na.rm = TRUE)
    moisture_sd <- stats::sd(data$Moisture, na.rm = TRUE)
    n_std <- params$n_std %||% 3
    moisture_min <- moisture_mean - n_std * moisture_sd
    moisture_max <- moisture_mean + n_std * moisture_sd

    to_keep <- dplyr::filter(data,
      is.na(Moisture) | (Moisture >= moisture_min & Moisture <= moisture_max))
    counts$moisture <- nrow(data) - nrow(to_keep)
  }

  # Etape 13 : filtre de chevauchement
  if ("Swath" %in% names(data) && !all(is.na(data$Swath))) {
    n_before <- nrow(data)
    result <- apply_overlap_filter(data,
      cellsize = params$cellsize_overlap %||% 0.3,
      overlap_threshold = params$overlap_threshold %||% 0.5
    )
    counts$overlap <- n_before - nrow(result)
  }

  # Etape 14 : filtre ecart-type localise
  if ("Swath" %in% names(data) && !all(is.na(data$Swath))) {
    n_before <- nrow(data)
    result <- apply_local_sd_filter(data,
      n_swaths = params$n_swaths %||% 5,
      lsd_limit = params$lsd_limit %||% 2.4,
      min_cells = params$min_cells %||% 3
    )
    counts$local_sd <- n_before - nrow(result)
  }

  counts
}
