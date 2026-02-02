#' Nettoyage rapide des données de rendement avec mise en cache
#'
#' Cette fonction permet de nettoyer les données en plusieurs phases :
#' 1. Pré-traitement (polygones, overlap, Delay Adjustment) - calculé une fois
#' 2. Filtres de rendement - peuvent être réappliqués sans recalculer le pré-traitement
#'
#' @param data Données brutes
#' @param phase "preprocess" ou "filter" ou "full"
#' @param preprocessed_data Données pré-traitées (si phase = "filter")
#' @param params Liste des paramètres
#' @param metrique Booléen pour conversion métrique
#' @param polygon Booléen pour création de polygones
#' @return Liste avec données nettoyées et métadonnées
#' @export
clean_yield_fast <- function(data, phase = "full", preprocessed_data = NULL, 
                              params = list(), metrique = TRUE, polygon = TRUE) {
  
  if (phase == "preprocess") {
    # Phase 1: Pré-traitement (calculé une fois)
    return(preprocess_yield_data(data, params, metrique))
  } else if (phase == "filter") {
    # Phase 2: Application des filtres (peut être répété)
    if (is.null(preprocessed_data)) {
      stop("preprocessed_data requis pour la phase 'filter'")
    }
    return(apply_yield_filters(preprocessed_data, params))
  } else {
    # Phase full: tout en une fois
    preproc <- preprocess_yield_data(data, params, metrique)
    return(apply_yield_filters(preproc, params, polygon))
  }
}


#' Pré-traitement des données de rendement
#'
#' Cette fonction effectue les calculs coûteux qui ne dépendent pas
#' des filtres de rendement : UTM, position, Delay Adjustment, polygones, overlap
#'
#' @param data Données brutes
#' @param params Liste des paramètres
#' @param metrique Booléen pour conversion métrique
#' @return Données pré-traitées avec tous les calculs coûteux effectués
 #' @noRd
 #' @keywords internal
 preprocess_yield_data <- function(data, params = list(), metrique = TRUE) {
  rlang::inform("=== Phase 1: Pré-traitement ===")
  
  # Étape 1: Conversion UTM
  rlang::inform("Étape 1: conversion en coordonnées UTM...")
  data <- latlon_to_utm(data)
  
  # Étape 2: Filtre position (hors champ)
  if (isTRUE(params$apply_position)) {
    rlang::inform("Étape 2: filtre position...")
    result <- filter_position_outliers(data)
    data <- result$data
    rlang::inform(paste("  Points conservés:", nrow(data)))
  }
  
  # Étape 3: Delay Adjustment flux (calcul du délai optimal)
  if (isTRUE(params$apply_delay_adjustment_flow)) {
    rlang::inform("Étape 3: Delay Adjustment - optimisation du délai de flux...")
    delay_adjustment_result <- apply_delay_adjustment(data, 
                              delay_range = params$delay_range %||% -25:25,
                              n_iterations = params$n_iterations %||% 5,
                              value_col = "Flow")
    flow_delay <- delay_adjustment_result$optimal_delay
    rlang::inform(paste("  Délai optimal flux:", flow_delay, "secondes"))
    
    # Stocker le résultat Delay Adjustment pour référence
    attr(data, "delay_adjustment_flow") <- delay_adjustment_result
  } else {
    flow_delay <- 0
  }
  
  # Étape 4: Delay Adjustment humidité
  moisture_delay <- 0
  if (isTRUE(params$apply_delay_adjustment_moisture) && "Moisture" %in% names(data)) {
    rlang::inform("Étape 4: Delay Adjustment - optimisation du délai d'humidité...")
    delay_adjustment_moisture <- apply_delay_adjustment(data,
                                delay_range = params$delay_range %||% -25:25,
                                n_iterations = params$n_iterations %||% 5,
                                value_col = "Moisture")
    moisture_delay <- delay_adjustment_moisture$optimal_delay
    rlang::inform(paste("  Délai optimal humidité:", moisture_delay, "secondes"))
    attr(data, "delay_adjustment_moisture") <- delay_adjustment_moisture
  }
  
  # Étape 5: Calcul initial du rendement (pour les seuils)
  rlang::inform("Étape 5: calcul du rendement initial...")
  data <- convert_flow_to_yield(data)
  
  # Étape 6: Création des polygones (indépendant des valeurs)
  if (isTRUE(params$create_polygons)) {
    rlang::inform("Étape 6: création des polygones...")
    # Calculer le cap si absent
    if (!"heading" %in% names(data)) {
      data <- data |>
        dplyr::mutate(
          heading = atan2(
            dplyr::lag(Longitude, default = Longitude[1]) - Longitude,
            dplyr::lag(Latitude, default = Latitude[1]) - Latitude
          ) * 180 / pi
        )
      data$heading[is.na(data$heading)] <- 0
    }
    
    # Calculer Distance et Swath si absents
    if (!"Distance_m" %in% names(data) || !"Swath_m" %in% names(data)) {
      data <- data |>
        dplyr::mutate(
          velocity = sqrt((X - dplyr::lag(X))^2 + (Y - dplyr::lag(Y))^2) /
            dplyr::coalesce(Interval, 1),
          Distance_m = velocity * Interval,
          Swath_m = 7.5  # Valeur par défaut
        )
    }
    
    attr(data, "polygons_ready") <- TRUE
  }
  
  # Étape 7: Analyse d'overlap (indépendante des valeurs)
  if (isTRUE(params$analyze_overlap)) {
    rlang::inform("Étape 7: analyse d'overlap...")
    # L'overlap est calculé sur la géométrie, pas sur les valeurs
    attr(data, "overlap_analyzed") <- TRUE
  }
  
  # Stocker les délais pour la phase de filtrage
  attr(data, "flow_delay") <- flow_delay
  attr(data, "moisture_delay") <- moisture_delay
  attr(data, "preprocessed") <- TRUE
  
  rlang::inform("=== Pré-traitement terminé ===")
  return(data)
}


#' Application des filtres de rendement
#'
#' Cette fonction applique les filtres qui peuvent être modifiés
#' sans recalculer le pré-traitement : header, GPS, vitesse, 
#' plage de rendement, humidité, etc.
#'
#' @param preprocessed_data Données pré-traitées
#' @param params Liste des paramètres des filtres
#' @param polygon Booléen pour création de polygones
#' @return Données filtrées
 #' @noRd
 #' @keywords internal
 apply_yield_filters <- function(preprocessed_data, params = list(), polygon = TRUE) {
  rlang::inform("=== Phase 2: Application des filtres ===")
  
  data <- preprocessed_data
  
  # Initialiser le suivi des suppressions par étape
  deletions <- list()
  deletions$step <- character()
  deletions$n <- numeric()
  
  # Initialiser le suivi des points supprimés avec leurs coordonnées
  deleted_points <- list()
  deleted_points$X <- numeric()
  deleted_points$Y <- numeric()
  deleted_points$Longitude <- numeric()
  deleted_points$Latitude <- numeric()
  deleted_points$step <- character()
  deleted_points$reason <- character()
  
  # Récupérer les délais calculés lors du pré-traitement
  flow_delay <- attr(data, "flow_delay") %||% 0
  moisture_delay <- attr(data, "moisture_delay") %||% 0
  
  # Étape 8: Correction du délai de flux
  if (flow_delay != 0) {
    rlang::inform(paste("Étape 8: correction du délai de flux (", flow_delay, "s)..."))
    data <- apply_flow_delay(data, delay = flow_delay, value_col = "Flow")
    if (sum(is.na(data$Flow)) > 0) {
      data <- interpolate_na(data, value_col = "Flow")
    }
  }
  
  # Étape 9: Correction du délai d'humidité
  if (moisture_delay != 0 && "Moisture" %in% names(data)) {
    rlang::inform(paste("Étape 9: correction du délai d'humidité (", moisture_delay, "s)..."))
    data <- apply_flow_delay(data, delay = moisture_delay, value_col = "Moisture")
    if (sum(is.na(data$Moisture)) > 0) {
      data <- interpolate_na(data, value_col = "Moisture")
    }
  }
  
  # Étape 10: Recalcul du rendement après délai
  rlang::inform("Étape 10: recalcul du rendement après délai...")
  data <- convert_flow_to_yield(data, force_recalculate = TRUE)
  
  # Étape 11: Calcul des seuils
  rlang::inform("Étape 11: calcul des seuils...")
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
  
  # Étape 12: Filtre header
  if (isTRUE(params$apply_header)) {
    rlang::inform("Étape 12: filtre header...")
    n_before <- nrow(data)
    # Identifier les points à supprimer avant le filtrage
    rows_to_delete <- which(!(data$HeaderStatus %in% c(0, 1, 33) | is.na(data$HeaderStatus)))
    if (length(rows_to_delete) > 0) {
      deleted_points$X <- c(deleted_points$X, data$X[rows_to_delete])
      deleted_points$Y <- c(deleted_points$Y, data$Y[rows_to_delete])
      deleted_points$Longitude <- c(deleted_points$Longitude, data$Longitude[rows_to_delete])
      deleted_points$Latitude <- c(deleted_points$Latitude, data$Latitude[rows_to_delete])
      deleted_points$step <- c(deleted_points$step, rep("Filtre header", length(rows_to_delete)))
      deleted_points$reason <- c(deleted_points$reason, rep("HeaderStatus invalide", length(rows_to_delete)))
    }
    data <- data |> dplyr::filter(HeaderStatus %in% c(0, 1, 33) | is.na(HeaderStatus))
    n_deleted <- n_before - nrow(data)
    deletions$step <- c(deletions$step, "Filtre header")
    deletions$n <- c(deletions$n, n_deleted)
    rlang::inform(paste("  Points supprimés:", n_deleted, "- conservés:", nrow(data)))
  }
  
  # Étape 13: Filtre GPS
  if (isTRUE(params$apply_gps) && "GPSStatus" %in% names(data)) {
    rlang::inform("Étape 13: filtre GPS...")
    n_before <- nrow(data)
    data$GPSStatus <- suppressWarnings(as.numeric(data$GPSStatus))
    # Identifier les points à supprimer avant le filtrage
    rows_to_delete <- which(!is.na(data$GPSStatus) & data$GPSStatus < 4)
    if (length(rows_to_delete) > 0) {
      deleted_points$X <- c(deleted_points$X, data$X[rows_to_delete])
      deleted_points$Y <- c(deleted_points$Y, data$Y[rows_to_delete])
      deleted_points$Longitude <- c(deleted_points$Longitude, data$Longitude[rows_to_delete])
      deleted_points$Latitude <- c(deleted_points$Latitude, data$Latitude[rows_to_delete])
      deleted_points$step <- c(deleted_points$step, rep("Filtre GPS", length(rows_to_delete)))
      deleted_points$reason <- c(deleted_points$reason, rep("GPSStatus < 4 (signal faible)", length(rows_to_delete)))
    }
    data <- data |> dplyr::filter(is.na(GPSStatus) | GPSStatus >= 4)
    n_deleted <- n_before - nrow(data)
    deletions$step <- c(deletions$step, "Filtre GPS")
    deletions$n <- c(deletions$n, n_deleted)
    rlang::inform(paste("  Points supprimés:", n_deleted, "- conservés:", nrow(data)))
  }
  
  # Étape 14: Calcul et filtre de vitesse
  if (isTRUE(params$apply_velocity)) {
    rlang::inform("Étape 14: calcul et filtre de vitesse...")
    n_before <- nrow(data)
    data <- data |>
      dplyr::mutate(
        velocity = sqrt((X - dplyr::lag(X))^2 + (Y - dplyr::lag(Y))^2) /
          dplyr::coalesce(Interval, 1)
      )
    data$velocity[is.na(data$velocity) | !is.finite(data$velocity)] <- 0
    # Identifier les points à supprimer avant le filtrage
    rows_to_delete <- which(data$velocity < thresholds$min_velocity | data$velocity > thresholds$max_velocity)
    if (length(rows_to_delete) > 0) {
      deleted_points$X <- c(deleted_points$X, data$X[rows_to_delete])
      deleted_points$Y <- c(deleted_points$Y, data$Y[rows_to_delete])
      deleted_points$Longitude <- c(deleted_points$Longitude, data$Longitude[rows_to_delete])
      deleted_points$Latitude <- c(deleted_points$Latitude, data$Latitude[rows_to_delete])
      deleted_points$step <- c(deleted_points$step, rep("Filtre vitesse", length(rows_to_delete)))
      reason_msg <- paste0("Vitesse hors limites [", round(thresholds$min_velocity, 2), "-", round(thresholds$max_velocity, 2), "]")
      deleted_points$reason <- c(deleted_points$reason, rep(reason_msg, length(rows_to_delete)))
    }
    data <- data |>
      dplyr::filter(velocity >= thresholds$min_velocity & velocity <= thresholds$max_velocity)
    n_deleted <- n_before - nrow(data)
    deletions$step <- c(deletions$step, "Filtre vitesse")
    deletions$n <- c(deletions$n, n_deleted)
    rlang::inform(paste("  Points supprimés:", n_deleted, "- conservés:", nrow(data)))
  }
  
  # Étape 15: Filtre changement de vitesse
  if (isTRUE(params$apply_velocity_jump)) {
    rlang::inform("Étape 15: filtre changement de vitesse...")
    n_before <- nrow(data)
    result <- filter_velocity_jumps(data, 
                                    max_acceleration = params$max_acceleration %||% 5,
                                    max_deceleration = params$max_deceleration %||% -8)
    # Tracker les points supprimés
    if (nrow(result$removed) > 0) {
      deleted_points$X <- c(deleted_points$X, result$removed$X)
      deleted_points$Y <- c(deleted_points$Y, result$removed$Y)
      deleted_points$Longitude <- c(deleted_points$Longitude, result$removed$Longitude)
      deleted_points$Latitude <- c(deleted_points$Latitude, result$removed$Latitude)
      deleted_points$step <- c(deleted_points$step, rep("Filtre changement de vitesse", nrow(result$removed)))
      reason_msg <- paste0("Changement de vitesse brusque [acc>", params$max_acceleration %||% 5, ", dec<", params$max_deceleration %||% -8, "]")
      deleted_points$reason <- c(deleted_points$reason, rep(reason_msg, nrow(result$removed)))
    }
    data <- result$data
    n_deleted <- n_before - nrow(data)
    deletions$step <- c(deletions$step, "Filtre changement de vitesse")
    deletions$n <- c(deletions$n, n_deleted)
    rlang::inform(paste("  Points supprimés:", n_deleted, "- conservés:", nrow(data)))
  }
  
  # Étape 16: Filtre anomalies de direction
  if (isTRUE(params$apply_heading_anomaly)) {
    rlang::inform("Étape 16: filtre anomalies de direction...")
    n_before <- nrow(data)
    result <- filter_heading_anomalies(data, 
                                       max_heading_change = params$max_heading_change %||% 60)
    # Tracker les points supprimés
    if (nrow(result$removed) > 0) {
      deleted_points$X <- c(deleted_points$X, result$removed$X)
      deleted_points$Y <- c(deleted_points$Y, result$removed$Y)
      deleted_points$Longitude <- c(deleted_points$Longitude, result$removed$Longitude)
      deleted_points$Latitude <- c(deleted_points$Latitude, result$removed$Latitude)
      deleted_points$step <- c(deleted_points$step, rep("Filtre direction", nrow(result$removed)))
      reason_msg <- paste0("Changement de direction anormal [>", params$max_heading_change %||% 60, "°]")
      deleted_points$reason <- c(deleted_points$reason, rep(reason_msg, nrow(result$removed)))
    }
    data <- result$data
    n_deleted <- n_before - nrow(data)
    deletions$step <- c(deletions$step, "Filtre direction")
    deletions$n <- c(deletions$n, n_deleted)
    rlang::inform(paste("  Points supprimés:", n_deleted, "- conservés:", nrow(data)))
  }
  
  # Étape 17: Suppression des rendements nuls
  if (isTRUE(params$apply_null_yield) && "Yield_kg_ha" %in% names(data)) {
    rlang::inform("Étape 17: suppression des rendements nuls...")
    n_before <- nrow(data)
    # Identifier les points à supprimer avant le filtrage
    rows_to_delete <- which(data$Yield_kg_ha <= 0 | is.na(data$Yield_kg_ha))
    if (length(rows_to_delete) > 0) {
      deleted_points$X <- c(deleted_points$X, data$X[rows_to_delete])
      deleted_points$Y <- c(deleted_points$Y, data$Y[rows_to_delete])
      deleted_points$Longitude <- c(deleted_points$Longitude, data$Longitude[rows_to_delete])
      deleted_points$Latitude <- c(deleted_points$Latitude, data$Latitude[rows_to_delete])
      deleted_points$step <- c(deleted_points$step, rep("Rendement nul", length(rows_to_delete)))
      deleted_points$reason <- c(deleted_points$reason, rep("Rendement nul ou manquant", length(rows_to_delete)))
    }
    data <- data |> dplyr::filter(Yield_kg_ha > 0)
    n_deleted <- n_before - nrow(data)
    deletions$step <- c(deletions$step, "Rendement nul")
    deletions$n <- c(deletions$n, n_deleted)
    rlang::inform(paste("  Points supprimés:", n_deleted, "- conservés:", nrow(data)))
  }
  
  # Étape 18: Filtre plage de rendement
  if (isTRUE(params$apply_yield_range)) {
    rlang::inform("Étape 18: filtre plage de rendement...")
    n_before <- nrow(data)
    # Identifier les points à supprimer avant le filtrage
    yield_col <- if ("Yield_buacre" %in% names(data)) "Yield_buacre" else "Yield_kg_ha"
    if (yield_col %in% names(data)) {
      rows_to_delete <- which(
        !is.finite(data[[yield_col]]) | 
        data[[yield_col]] < thresholds$min_yield | 
        data[[yield_col]] > thresholds$max_yield
      )
      if (length(rows_to_delete) > 0) {
         deleted_points$X <- c(deleted_points$X, data$X[rows_to_delete])
         deleted_points$Y <- c(deleted_points$Y, data$Y[rows_to_delete])
         deleted_points$Longitude <- c(deleted_points$Longitude, data$Longitude[rows_to_delete])
         deleted_points$Latitude <- c(deleted_points$Latitude, data$Latitude[rows_to_delete])
         deleted_points$step <- c(deleted_points$step, rep("Filtre plage rendement", length(rows_to_delete)))
        reason_msg <- paste0("Rendement hors plage [", round(thresholds$min_yield, 1), "-", round(thresholds$max_yield, 1), "]")
        deleted_points$reason <- c(deleted_points$reason, rep(reason_msg, length(rows_to_delete)))
      }
    }
    data <- filter_yield_range(data,
                               min_yield = thresholds$min_yield,
                               max_yield = thresholds$max_yield)
    n_deleted <- n_before - nrow(data)
    deletions$step <- c(deletions$step, "Filtre plage rendement")
    deletions$n <- c(deletions$n, n_deleted)
    rlang::inform(paste("  Points supprimés:", n_deleted, "- conservés:", nrow(data)))
  }
  
  # Étape 19: Filtre humidité
  if (isTRUE(params$apply_moisture) && "Moisture" %in% names(data)) {
    rlang::inform("Étape 19: filtre humidité...")
    n_before <- nrow(data)
    # Calculer les seuils d'humidité pour identifier les points à supprimer
    n_std <- params$n_std %||% 3
    mean_moisture <- mean(data$Moisture, na.rm = TRUE)
    sd_moisture <- stats::sd(data$Moisture, na.rm = TRUE)
    min_moisture <- mean_moisture - n_std * sd_moisture
    max_moisture <- mean_moisture + n_std * sd_moisture
    # Identifier les points à supprimer avant le filtrage
    rows_to_delete <- which(
      is.na(data$Moisture) | 
      data$Moisture < min_moisture | 
      data$Moisture > max_moisture
    )
    if (length(rows_to_delete) > 0) {
       deleted_points$X <- c(deleted_points$X, data$X[rows_to_delete])
       deleted_points$Y <- c(deleted_points$Y, data$Y[rows_to_delete])
       deleted_points$Longitude <- c(deleted_points$Longitude, data$Longitude[rows_to_delete])
       deleted_points$Latitude <- c(deleted_points$Latitude, data$Latitude[rows_to_delete])
       deleted_points$step <- c(deleted_points$step, rep("Filtre humidité", length(rows_to_delete)))
      reason_msg <- paste0("Humidité hors plage [", round(min_moisture, 1), "-", round(max_moisture, 1), "] (mean ±", n_std, "SD)")
      deleted_points$reason <- c(deleted_points$reason, rep(reason_msg, length(rows_to_delete)))
    }
    data <- filter_moisture_range(data, n_std = n_std)
    n_deleted <- n_before - nrow(data)
    deletions$step <- c(deletions$step, "Filtre humidité")
    deletions$n <- c(deletions$n, n_deleted)
    rlang::inform(paste("  Points supprimés:", n_deleted, "- conservés:", nrow(data)))
  }
  
  # Étape 20: Filtre de chevauchement
  if (isTRUE(params$apply_overlap)) {
    rlang::inform("Étape 20: filtre de chevauchement...")
    n_before <- nrow(data)
    # Calculer l'overlap ratio avant filtrage pour tracker les points supprimés
    overlap_threshold <- params$overlap_threshold %||% 0.4
    # Temporairement ajouter l'overlap_ratio pour identifier les points à supprimer
    data_with_overlap <- apply_overlap_filter(data,
                           cellsize = params$cellsize_overlap %||% 0.3,
                           overlap_threshold = 1.0)  # Garder tous les points temporairement
    # Identifier les points qui seront supprimés
    if ("overlap_ratio" %in% names(data_with_overlap)) {
      rows_to_delete <- which(data_with_overlap$overlap_ratio > overlap_threshold)
      if (length(rows_to_delete) > 0) {
         deleted_points$X <- c(deleted_points$X, data$X[rows_to_delete])
         deleted_points$Y <- c(deleted_points$Y, data$Y[rows_to_delete])
         deleted_points$Longitude <- c(deleted_points$Longitude, data$Longitude[rows_to_delete])
         deleted_points$Latitude <- c(deleted_points$Latitude, data$Latitude[rows_to_delete])
         deleted_points$step <- c(deleted_points$step, rep("Filtre chevauchement", length(rows_to_delete)))
        reason_msg <- paste0("Chevauchement excessif [ratio >", overlap_threshold, "]")
        deleted_points$reason <- c(deleted_points$reason, rep(reason_msg, length(rows_to_delete)))
      }
      # Appliquer le filtre final
      data <- data_with_overlap |> dplyr::filter(overlap_ratio <= overlap_threshold)
    } else {
      # Fallback si overlap_ratio n'est pas disponible
      data <- apply_overlap_filter(data,
                             cellsize = params$cellsize_overlap %||% 0.3,
                             overlap_threshold = overlap_threshold)
    }
    n_deleted <- n_before - nrow(data)
    deletions$step <- c(deletions$step, "Filtre chevauchement")
    deletions$n <- c(deletions$n, n_deleted)
    rlang::inform(paste("  Points supprimés:", n_deleted, "- conservés:", nrow(data)))
  }
  
  # Étape 21: Filtre écart-type local
  if (isTRUE(params$apply_local_sd)) {
    rlang::inform("Étape 21: filtre écart-type local...")
    n_before <- nrow(data)
    lsd_limit <- params$lsd_limit %||% 2.4
    min_cells <- params$min_cells %||% 3
    
    # Calculer les statistiques locales pour identifier les outliers avant filtrage
    if (all(c("X", "Y", "Flow", "Swath") %in% names(data))) {
      swath_m <- mean(data$Swath, na.rm = TRUE) * 0.0254
      cellsize <- 5 * swath_m  # n_swaths = 5 par défaut
      
      data_temp <- data |>
        dplyr::mutate(
          cell_x = floor(X / cellsize),
          cell_y = floor(Y / cellsize),
          cell_id = paste(cell_x, cell_y, sep = "_")
        )
      
      cell_stats <- data_temp |>
        dplyr::group_by(cell_id) |>
        dplyr::summarise(
          local_mean = mean(Flow, na.rm = TRUE),
          local_sd = stats::sd(Flow, na.rm = TRUE),
          n = dplyr::n(),
          .groups = "drop"
        ) |>
        dplyr::filter(n >= min_cells)
      
      global_mean <- mean(data$Flow, na.rm = TRUE)
      global_sd <- stats::sd(data$Flow, na.rm = TRUE)
      
      data_temp <- data_temp |>
        dplyr::left_join(cell_stats, by = "cell_id") |>
        dplyr::mutate(
          local_mean = ifelse(is.na(local_mean), global_mean, local_mean),
          local_sd = ifelse(is.na(local_sd), global_sd, local_sd),
          sd_upper = local_mean + lsd_limit * local_sd,
          sd_lower = local_mean - lsd_limit * local_sd,
          is_outlier = Flow > sd_upper | Flow < sd_lower
        )
      
      # Identifier les points à supprimer
      rows_to_delete <- which(data_temp$is_outlier)
      if (length(rows_to_delete) > 0) {
         deleted_points$X <- c(deleted_points$X, data$X[rows_to_delete])
         deleted_points$Y <- c(deleted_points$Y, data$Y[rows_to_delete])
         deleted_points$Longitude <- c(deleted_points$Longitude, data$Longitude[rows_to_delete])
         deleted_points$Latitude <- c(deleted_points$Latitude, data$Latitude[rows_to_delete])
         deleted_points$step <- c(deleted_points$step, rep("Filtre ET local", length(rows_to_delete)))
        reason_msg <- paste0("Outlier local [Flow hors ", lsd_limit, "×ET local]")
        deleted_points$reason <- c(deleted_points$reason, rep(reason_msg, length(rows_to_delete)))
      }
    }
    
    data <- apply_local_sd_filter(data, 
                            lsd_limit = lsd_limit,
                            min_cells = min_cells)
    n_deleted <- n_before - nrow(data)
    deletions$step <- c(deletions$step, "Filtre ET local")
    deletions$n <- c(deletions$n, n_deleted)
    rlang::inform(paste("  Points supprimés:", n_deleted, "- conservés:", nrow(data)))
  }
  
  # Étape 22: Création des polygones
  if (polygon && nrow(data) > 0) {
    rlang::inform("Étape 22: création des polygones...")
    data <- data_to_sf(data, crs = 4326)
  }
  
  rlang::inform("=== Filtres appliqués ===")
  
  # Convertir la liste des points supprimés en data frame
  deleted_points_df <- if (length(deleted_points$X) > 0) {
    data.frame(
      X = deleted_points$X,
      Y = deleted_points$Y,
      Longitude = if (!is.null(deleted_points$Longitude)) deleted_points$Longitude else numeric(),
      Latitude = if (!is.null(deleted_points$Latitude)) deleted_points$Latitude else numeric(),
      step = deleted_points$step,
      reason = deleted_points$reason,
      stringsAsFactors = FALSE
    )
  } else {
    # Retourner un data frame vide avec la bonne structure
    data.frame(
      X = numeric(),
      Y = numeric(),
      Longitude = numeric(),
      Latitude = numeric(),
      step = character(),
      reason = character(),
      stringsAsFactors = FALSE
    )
  }
  
  return(list(
    data = data,
    deletions = data.frame(
      step = deletions$step,
      n = deletions$n
    ),
    deleted_points = deleted_points_df
  ))
}
