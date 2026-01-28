#' Nettoyage des donnees de rendement avec suivi des suppressions
#'
#' Variante de clean_yield() qui enregistre les points supprimes a chaque etape
#' ainsi que la raison. Utile pour la visualisation et le diagnostic.
#'
#' @inheritParams clean_yield
#' @param data Donnees brutes a nettoyer (tibble). Alternative a file_path.
#'   Si fourni, file_path est ignore.
#' @return A list containing:
#'   - data_clean: Donnees nettoyees (tibble ou objet SF)
#'   - deletions: Tableau des suppressions avec raisons
#'   - stats: Statistiques de synthese
#' @export
#' @examples
#' \dontrun{
#' # Avec un fichier
#' result <- clean_yield_with_tracking("data.txt", metrique = TRUE, polygon = TRUE)
#' 
#' # Avec des donnees en memoire
#' result <- clean_yield_with_tracking(data = my_data, metrique = TRUE, polygon = TRUE)
#' 
#' print(result$stats)
#' head(result$deletions)
#' }
clean_yield_with_tracking <- function(file_path = NULL, data = NULL, metrique = TRUE, polygon = TRUE,
                                    params = NULL) {
  
  # Check that either file_path or data is provided
  if (is.null(file_path) && is.null(data)) {
    rlang::abort("Either 'file_path' or 'data' must be provided")
  }
  
  # Load data from file or use provided data
  if (!is.null(file_path)) {
    data_raw <- read_yield_data(file_path)
  } else {
    data_raw <- data
  }
  
  rlang::inform("================================================")
  rlang::inform("   Nettoyage des rendements avec suivi       ")
  rlang::inform("================================================")
  rlang::inform(paste("  -", nrow(data_raw), "observations chargees"))
  
  # Initialiser le suivi des suppressions
  deletions <- data.frame(
    orig_row_id = integer(0),
    step = character(0),
    reason = character(0),
    stringsAsFactors = FALSE
  )
  
  data <- data_raw |> dplyr::mutate(orig_row_id = dplyr::row_number())
  
  # Creer Yield_kg_ha_wet si Flow_Wet existe (pour fichiers non-ZIP)
  if ("Flow_Wet" %in% names(data) && !all(is.na(data$Flow_Wet))) {
    if (!"Yield_kg_ha_wet" %in% names(data)) {
      rlang::inform("Creation de Yield_kg_ha_wet a partir de Flow_Wet...")
      data$Yield_kg_ha_wet <- data$Flow_Wet
    }
  }
  
  # Calculer le rendement sec a partir du rendement humide et de l'humidite
  # Formule: Rendement sec = Rendement humide × (100 - Humidite%) / (100 - 14.5%)
  if ("Moisture" %in% names(data) && !all(is.na(data$Moisture))) {
    if ("Yield_kg_ha_wet" %in% names(data) && !all(is.na(data$Yield_kg_ha_wet))) {
      rlang::inform("Calcul du rendement sec a partir du rendement humide...")
      data$Yield_kg_ha <- data$Yield_kg_ha_wet * (100 - data$Moisture) / 85.5
      rlang::inform(paste("Rendement sec calcule:", round(mean(data$Yield_kg_ha, na.rm = TRUE), 1), "kg/ha"))
    }
    if ("Flow_Wet" %in% names(data) && !all(is.na(data$Flow_Wet))) {
      data$Flow <- data$Flow_Wet * (100 - data$Moisture) / 85.5
    }
  }
  
  # Etape 2 : conversion UTM
  rlang::inform("Etape 2 : conversion en coordonnees UTM...")
  data <- latlon_to_utm(data)
  
  # Etape 3 : PCDI - optimisation du delai de flux
  # Verifier si les donnees contiennent deja des colonnes de rendement (sans Flow)
  has_yield_cols <- any(c("Yield_kg_ha", "Yield_buacre") %in% names(data))
  has_flow_col <- "Flow" %in% names(data) && !all(is.na(data$Flow))
  
  # PCDI flux - optionnel
  flow_delay <- 0
  if (isTRUE(params$apply_pcdi_flow)) {
    if (has_yield_cols && !has_flow_col) {
      # Donnees John Deere : calculer Flow a partir de Yield pour PCDI
      rlang::inform("Etape 3 : calcul du flux a partir du rendement (John Deere)...")
      
      # Verifier les colonnes necessaires
      required_for_flow <- c("Swath", "Distance", "Interval")
      missing_cols <- setdiff(required_for_flow, names(data))
      
      if (length(missing_cols) > 0) {
        rlang::warn(paste("Colonnes manquantes pour calculer le flux:", paste(missing_cols, collapse = ", ")))
        rlang::inform("  Utilisation de valeurs par defaut...")
        
        # Ajouter des valeurs par defaut
        if (!"Swath" %in% names(data)) data$Swath <- 7.5  # 7.5 metres (30 pieds)
        if (!"Distance" %in% names(data)) data$Distance <- 1.5  # 1.5 metres par intervalle
        if (!"Interval" %in% names(data)) data$Interval <- 1  # 1 seconde
      }
      
      # Determiner quelle colonne de rendement utiliser
      if ("Yield_kg_ha" %in% names(data)) {
        yield_col <- "Yield_kg_ha"
        # Conversion kg/ha -> kg/s approximatif
        # Flow (kg/s) = Yield (kg/ha) * Swath (m) * Distance (m) / (Interval (s) * 10000 m2/ha)
        data <- data |>
          dplyr::mutate(
            Flow = .data$Yield_kg_ha * .data$Swath * .data$Distance / 
                   (.data$Interval * 10000)
          )
      } else if ("Yield_buacre" %in% names(data)) {
        yield_col <- "Yield_buacre"
        # Conversion bu/acre -> lbs/s approximatif (utiliser 56 lbs/bu par defaut pour mais)
        lbs_per_bu <- 56
        sqft_per_acre <- 43560
        # Flow (lbs/s) = Yield (bu/acre) * lbs_per_bu * Distance (ft) * Swath (ft) / (Interval (s) * sqft_per_acre)
        # Convertir Swath et Distance de metres en pieds
        data <- data |>
          dplyr::mutate(
            Flow = .data$Yield_buacre * lbs_per_bu * (.data$Distance * 3.28084) * (.data$Swath * 3.28084) / 
                   (.data$Interval * sqft_per_acre)
          )
      }
      
      valid_flow <- sum(!is.na(data$Flow) & data$Flow > 0)
      rlang::inform(paste("  Flux calcule:", valid_flow, "valeurs valides"))
      
      # Si aucun flux valide, on ne peut pas faire PCDI
      if (valid_flow == 0) {
        rlang::warn("Aucun flux valide calcule - PCDI saute")
        flow_delay <- 0
      } else {
        # Etape 3 : PCDI
        rlang::inform("Etape 3 : PCDI - optimisation du delai de flux...")
        pcdi_result <- apply_pcdi(data,
          delay_range = params$delay_range %||% -25:10,
          n_iterations = params$n_iterations %||% 5,
          noise_level = params$noise_level %||% 0.05
        )
        flow_delay <- pcdi_result$optimal_delay
        rlang::inform(paste("  Delai optimal :", flow_delay, "secondes"))
      }
    } else {
      # Donnees avec Flow existant : PCDI normal
      rlang::inform("Etape 3 : PCDI - optimisation du delai de flux...")
      pcdi_result <- apply_pcdi(data,
        delay_range = params$delay_range %||% -25:10,
        n_iterations = params$n_iterations %||% 5,
        noise_level = params$noise_level %||% 0.05,
        value_col = "Flow"
      )
      flow_delay <- pcdi_result$optimal_delay
      rlang::inform(paste("  Delai optimal flux:", flow_delay, "secondes"))
    }
  } else {
    rlang::inform("Etape 3 : PCDI flux saute (desactive)")
  }
  
  # Etape 3b : PCDI sur l'humidite si disponible - optionnel
  moisture_delay <- 0
  if (isTRUE(params$apply_pcdi_moisture) && "Moisture" %in% names(data) && !all(is.na(data$Moisture))) {
    rlang::inform("Etape 3b : PCDI - optimisation du delai d'humidite...")
    pcdi_moisture <- apply_pcdi(data,
      delay_range = params$delay_range %||% -25:10,
      n_iterations = params$n_iterations %||% 5,
      noise_level = params$noise_level %||% 0.05,
      value_col = "Moisture"
    )
    moisture_delay <- pcdi_moisture$optimal_delay
    rlang::inform(paste("  Delai optimal humidite:", moisture_delay, "secondes"))
  } else {
    rlang::inform("Etape 3b : PCDI humidite saute (desactive ou donnees non disponibles)")
  }

  # Etape 3c : calcul du rendement initial
  data <- convert_flow_to_yield(data)
  
  # Etape 4 : seuils automatiques
  rlang::inform("Etape 4 : calcul des seuils automatiques...")
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
  
  # Etape 5 : filtre header (optionnel)
  if (isTRUE(params$apply_header) && "HeaderStatus" %in% names(data)) {
    rlang::inform("Etape 5 : filtre header...")
    to_keep <- dplyr::filter(data, HeaderStatus %in% c(1, 33) | is.na(HeaderStatus))
    new_deletions <- dplyr::setdiff(data, to_keep) |>
      dplyr::mutate(step = "Filtre header", reason = "HeaderStatus inactif")
    deletions <- dplyr::bind_rows(deletions, new_deletions |> dplyr::select(orig_row_id, step, reason))
    data <- to_keep
    rlang::inform(paste("  Supprimes :", nrow(new_deletions), "points"))
  } else {
    rlang::inform("Etape 5 : filtre header saute")
  }

  # Etape 6 : filtre GPS (optionnel)
  if (isTRUE(params$apply_gps) && "GPSStatus" %in% names(data)) {
    rlang::inform("Etape 6 : filtre GPS...")
    data$GPSStatus <- suppressWarnings(as.numeric(data$GPSStatus))
    to_keep <- dplyr::filter(data, is.na(GPSStatus) | GPSStatus >= 4)
    new_deletions <- dplyr::setdiff(data, to_keep) |>
      dplyr::mutate(step = "Filtre GPS", reason = "GPSStatus < 4")
    deletions <- dplyr::bind_rows(deletions, new_deletions |> dplyr::select(orig_row_id, step, reason))
    data <- to_keep
    rlang::inform(paste("  Supprimes :", nrow(new_deletions), "points"))
  } else {
    rlang::inform("Etape 6 : filtre GPS saute")
  }

  # Etape 7 : calcul de la vitesse (toujours fait, necessaire pour les filtres suivants)
  rlang::inform("Etape 7 : calcul de la vitesse...")
  data <- data |>
    dplyr::mutate(
      velocity = sqrt((X - dplyr::lag(X))^2 + (Y - dplyr::lag(Y))^2) /
                    dplyr::coalesce(Interval, 1)
    )
  data$velocity[is.na(data$velocity) | !is.finite(data$velocity)] <- 0

  # Etape 8 : filtre vitesse (optionnel)
  if (isTRUE(params$apply_velocity)) {
    rlang::inform("Etape 8 : filtre vitesse...")
    to_keep <- dplyr::filter(data, velocity >= thresholds$min_velocity & velocity <= thresholds$max_velocity)
    new_deletions <- dplyr::setdiff(data, to_keep) |>
      dplyr::mutate(
        step = "Filtre vitesse",
        reason = paste0("Vitesse hors plage [", round(thresholds$min_velocity, 2), ", ", round(thresholds$max_velocity, 2), "] m/s")
      )
    deletions <- dplyr::bind_rows(deletions, new_deletions |> dplyr::select(orig_row_id, step, reason))
    data <- to_keep
    rlang::inform(paste("  Supprimes :", nrow(new_deletions), "points"))
  } else {
    rlang::inform("Etape 8 : filtre vitesse saute")
  }

  # Etape 8b : filtre changements brusques de vitesse
  if (isTRUE(params$apply_velocity_jump)) {
    rlang::inform("Etape 8b : filtre changements brusques de vitesse...")
    result <- filter_velocity_jumps(
      data,
      max_acceleration = params$max_acceleration %||% 3,
      max_deceleration = params$max_deceleration %||% -5
    )
    new_deletions <- result$removed |>
      dplyr::mutate(
        step = "Filtre changement vitesse",
        reason = "Changement brusque de vitesse"
      )
    deletions <- dplyr::bind_rows(deletions, new_deletions |> dplyr::select(orig_row_id, step, reason))
    data <- result$data
    rlang::inform(paste("  Supprimes :", nrow(new_deletions), "points"))
  }

  # Etape 8c : filtre anomalies de direction
  if (isTRUE(params$apply_heading_anomaly)) {
    rlang::inform("Etape 8c : filtre anomalies de direction...")
    result <- filter_heading_anomalies(
      data,
      max_heading_change = params$max_heading_change %||% 15
    )
    new_deletions <- result$removed |>
      dplyr::mutate(
        step = "Filtre direction",
        reason = "Anomalie de direction"
      )
    deletions <- dplyr::bind_rows(deletions, new_deletions |> dplyr::select(orig_row_id, step, reason))
    data <- result$data
    rlang::inform(paste("  Supprimes :", nrow(new_deletions), "points"))
  }

  # Etape 9 : correction du delai de flux (si necessaire)
  if (flow_delay != 0) {
    rlang::inform(paste("Etape 9 : correction du delai de flux (", flow_delay, "s)..."))
    data <- apply_flow_delay(data, delay = -flow_delay, value_col = "Flow")

    # Etape 9b : recalcul du rendement apres correction du delai
    data <- convert_flow_to_yield(data)
  } else {
    rlang::inform("Etape 9 : pas de correction de delai de flux necessaire (delai = 0)")
  }
  
  # Etape 9c : correction du delai d'humidite (si necessaire)
  if (moisture_delay != 0 && "Moisture" %in% names(data)) {
    rlang::inform(paste("Etape 9c : correction du delai d'humidite (", moisture_delay, "s)..."))
    data <- apply_flow_delay(data, delay = -moisture_delay, value_col = "Moisture")
  } else {
    rlang::inform("Etape 9c : pas de correction de delai d'humidite necessaire (delai = 0)")
  }
  
  # Etape 9c : recalcul des seuils
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
  
  # Etape 9e : suppression des points de bordure
  rlang::inform("Etape 9e : suppression des points de bordure...")
  if ("Pass" %in% names(data) && "Interval" %in% names(data) && !all(is.na(data$Pass))) {
    if (flow_delay == 0) {
      # Pas de delai de flux (donnees John Deere) - pas de suppression de bordure liee au flux
      rlang::inform("  Ignore - pas de delai de flux a corriger (donnees avec rendement direct)")
    } else {
      n_before <- nrow(data)
      abs_delay <- abs(flow_delay)

      data <- data |>
        dplyr::arrange(GPS_Time) |>
        dplyr::group_by(Pass) |>
        dplyr::mutate(
          interval_cumsum = cumsum(Interval),
          total_interval = sum(Interval, na.rm = TRUE),
          interval_from_end = total_interval - interval_cumsum
        ) |>
        dplyr::ungroup()

      if (flow_delay < 0) {
        to_keep <- dplyr::filter(data, interval_from_end > abs_delay | is.na(interval_from_end))
      } else {
        to_keep <- dplyr::filter(data, interval_cumsum > abs_delay | is.na(interval_cumsum))
      }

      new_deletions <- dplyr::setdiff(data, to_keep) |>
        dplyr::mutate(step = "Suppression bordure", reason = paste0("Bordure delai flux (", flow_delay, "s)"))
      deletions <- dplyr::bind_rows(deletions, new_deletions |> dplyr::select(orig_row_id, step, reason))
      data <- to_keep
      rlang::inform(paste("  Supprimes :", nrow(new_deletions), "points"))
    }
  }
  
  # Etape 10 : suppression des rendements nuls (optionnel)
  if (isTRUE(params$apply_null_yield) && "Yield_buacre" %in% names(data)) {
    rlang::inform("Etape 10 : suppression des rendements nuls...")
    to_keep <- dplyr::filter(data, Yield_buacre > 0)
    new_deletions <- dplyr::setdiff(data, to_keep) |>
      dplyr::mutate(step = "Rendement nul", reason = "Rendement = 0")
    deletions <- dplyr::bind_rows(deletions, new_deletions |> dplyr::select(orig_row_id, step, reason))
    data <- to_keep
    rlang::inform(paste("  Supprimes :", nrow(new_deletions), "points"))
  } else {
    rlang::inform("Etape 10 : suppression des rendements nuls saute")
  }

  # Etape 11 : filtre plage de rendement (optionnel)
  if (isTRUE(params$apply_yield_range) && "Yield_buacre" %in% names(data)) {
    rlang::inform("Etape 11 : filtre plage de rendement...")
    to_keep <- dplyr::filter(data,
      Yield_buacre >= thresholds$min_yield &
      Yield_buacre <= thresholds$max_yield)
    new_deletions <- dplyr::setdiff(data, to_keep) |>
      dplyr::mutate(
        step = "Filtre plage rendement",
        reason = paste0("Rendement hors plage [", round(thresholds$min_yield, 2), ", ", round(thresholds$max_yield, 2), "] bu/acre")
      )
    deletions <- dplyr::bind_rows(deletions, new_deletions |> dplyr::select(orig_row_id, step, reason))
    data <- to_keep
    rlang::inform(paste("  Supprimes :", nrow(new_deletions), "points"))
  } else {
    rlang::inform("Etape 11 : filtre plage de rendement saute")
  }

  # Etape 12 : filtre humidite (optionnel)
  if (isTRUE(params$apply_moisture) && "Moisture" %in% names(data) && !all(is.na(data$Moisture))) {
    rlang::inform("Etape 12 : filtre humidite (auto-detection)...")
    moisture_mean <- mean(data$Moisture, na.rm = TRUE)
    moisture_sd <- stats::sd(data$Moisture, na.rm = TRUE)
    n_std <- params$n_std %||% 3
    moisture_min <- moisture_mean - n_std * moisture_sd
    moisture_max <- moisture_mean + n_std * moisture_sd

    to_keep <- dplyr::filter(data,
      is.na(Moisture) | (Moisture >= moisture_min & Moisture <= moisture_max))
    new_deletions <- dplyr::setdiff(data, to_keep) |>
      dplyr::mutate(
        step = "Filtre humidite",
        reason = paste0("Humidite hors plage [", round(moisture_min, 2), ", ", round(moisture_max, 2), "]")
      )
    deletions <- dplyr::bind_rows(deletions, new_deletions |> dplyr::select(orig_row_id, step, reason))
    data <- to_keep
    rlang::inform(paste("  Supprimes :", nrow(new_deletions), "points"))
  } else {
    rlang::inform("Etape 12 : filtre humidite saute")
  }
   
    # Etape 12b : lissage du cap pour les polygones
    rlang::inform("Etape 12b : lissage du cap pour les polygones...")
    data <- data |> dplyr::arrange(GPS_Time)
    n <- nrow(data)
    
    # Verifier qu'il y a assez de donnees pour le lissage
    if (n < 2) {
      rlang::warn("Pas assez de points pour le lissage du cap, etape ignoree")
      data$heading <- 0
    } else {
     if (all(c("X", "Y") %in% names(data))) {
       data <- data |>
         dplyr::mutate(
           heading = atan2(
             dplyr::lag(X, default = X[1]) - X,
             dplyr::lag(Y, default = Y[1]) - Y
           ) * 180 / pi
         )
     } else {
       data <- data |>
         dplyr::mutate(
           heading = atan2(
             dplyr::lag(Longitude, default = Longitude[1]) - Longitude,
             dplyr::lag(Latitude, default = Latitude[1]) - Latitude
           ) * 180 / pi
         )
     }
    data$heading[is.na(data$heading)] <- 0
    
    heading_diff <- c(data$heading[2:n] - data$heading[1:(n-1)], 0)
    heading_diff <- ((heading_diff + 180) %% 360) - 180
    turn_threshold <- 30
    is_turn <- abs(heading_diff) > turn_threshold
    segment_id <- cumsum(c(TRUE, is_turn[1:(n-1)]))
    data$segment_id <- segment_id
   
   data <- data |>
     dplyr::group_by(segment_id) |>
     dplyr::mutate(
       row_in_segment = dplyr::row_number(),
       n_in_segment = dplyr::n(),
       heading_var = {
         angles_rad <- heading * pi / 180
         sin_mean <- mean(sin(angles_rad), na.rm = TRUE)
         cos_mean <- mean(cos(angles_rad), na.rm = TRUE)
         R <- sqrt(sin_mean^2 + cos_mean^2)
         ifelse(R < 0.001, 0, sqrt(-2 * log(R)) * 180 / pi)
       },
       heading_smooth = dplyr::case_when(
         row_in_segment == 1 | row_in_segment == n_in_segment ~ heading,
         heading_var > 15 ~ {
           zoo::rollapply(
             heading,
             width = 3,
             align = "center",
             partial = TRUE,
             FUN = function(x) {
               angles_rad <- x * pi / 180
               atan2(mean(sin(angles_rad), na.rm = TRUE),
                     mean(cos(angles_rad), na.rm = TRUE)) * 180 / pi
             }
           )
         },
         n_in_segment >= 3 ~ {
           angles_rad <- heading * pi / 180
           atan2(mean(sin(angles_rad), na.rm = TRUE),
                 mean(cos(angles_rad), na.rm = TRUE)) * 180 / pi
         },
         TRUE ~ heading
       )
     ) |>
     dplyr::ungroup()
   
    data$heading <- data$heading_smooth
    data$heading[is.na(data$heading)] <- 0
    data$segment_id <- NULL
    data$row_in_segment <- NULL
    data$n_in_segment <- NULL
    data$heading_var <- NULL
    data$heading_smooth <- NULL
    
    rlang::inform(paste("  Cap lisse pour", nrow(data), "points"))
    }
   
   # Etape 13 : filtre de chevauchement (optionnel)
   if (isTRUE(params$apply_overlap) && "Swath" %in% names(data) && !all(is.na(data$Swath))) {
     rlang::inform("Etape 13 : filtre de chevauchement bitmap...")
     n_before <- nrow(data)
     current_orig_ids <- data$orig_row_id
     data <- apply_overlap_filter(data,
       cellsize = params$cellsize_overlap %||% 0.3,
       overlap_threshold = params$overlap_threshold %||% 0.5
     )
     n_removed <- n_before - nrow(data)
     if (n_removed > 0) {
       kept_ids <- data$orig_row_id
       removed_ids <- setdiff(current_orig_ids, kept_ids)
       if (length(removed_ids) > 0) {
         new_deletions <- tibble::tibble(
           orig_row_id = head(removed_ids, min(length(removed_ids), 1000)),
           step = "Filtre chevauchement",
           reason = "Chevauchement excessif"
         )
         deletions <- dplyr::bind_rows(deletions, new_deletions)
       }
     }
     rlang::inform(paste("  Supprimes :", n_removed, "points"))
   } else {
     rlang::inform("Etape 13 : filtre de chevauchement saute")
   }

   # Etape 13b : suppression precoce des polygones chevauchants
  if (polygon && nrow(data) > 100 && "Pass" %in% names(data)) {
    rlang::inform("Etape 13b : suppression des polygones chevauchants (precoce)...")
    n_before_poly <- nrow(data)

    tryCatch({
      sf_use_s2(FALSE)

      sf_result <- data_to_sf(data, crs = 4326)
      
      if (is.null(sf_result)) {
        rlang::warn("Impossible de creer les polygones - etape de chevauchement saute")
        return(data)
      }

      median_lon <- stats::median(sf_result$Longitude, na.rm = TRUE)
      median_lat <- stats::median(sf_result$Latitude, na.rm = TRUE)
      utm_zone <- floor((median_lon + 180) / 6) + 1
      utm_epsg <- ifelse(median_lat < 0, 32700 + utm_zone, 32600 + utm_zone)
      utm_crs <- sf::st_crs(paste0("EPSG:", utm_epsg))
      sf_proj <- sf::st_transform(sf_result, utm_crs)

      # Calculate polygon areas in projected CRS
      sf_proj$poly_area <- as.numeric(sf::st_area(sf_proj))

      # Get unique passes sorted
      passes <- sort(unique(sf_proj$Pass))
      pass_window <- params$overlap_pass_window %||% 2
      max_candidates <- params$overlap_max_candidates %||% 150
      overlap_area_threshold <- params$overlap_area_threshold %||% 0.08

      # Track rows to remove
      rows_to_remove <- integer(0)

      for (p in passes[-1]) {
        current_mask <- sf_proj$Pass == p
        previous_mask <- sf_proj$Pass < p & sf_proj$Pass >= (p - pass_window)

        if (!any(current_mask) || !any(previous_mask)) next

        current_polys <- sf_proj[current_mask, ]
        previous_polys <- sf_proj[previous_mask, ]

        if (nrow(current_polys) == 0 || nrow(previous_polys) == 0) next

        # Find previous polygons within 10m
        nearby_idx <- sf::st_is_within_distance(
          current_polys,
          previous_polys,
          dist = 10,
          sparse = TRUE
        )

        if (length(nearby_idx) == 0) next

        current_geom <- sf::st_geometry(current_polys)
        previous_geom <- sf::st_geometry(previous_polys)

        for (i in seq_len(nrow(current_polys))) {
          idx <- nearby_idx[[i]]
          if (length(idx) == 0) next
          if (length(idx) > max_candidates) {
            idx <- idx[seq_len(max_candidates)]
          }

          cur_area <- current_polys$poly_area[i]
          if (is.na(cur_area) || cur_area == 0) next

          inter <- tryCatch({
            sf::st_intersection(previous_geom[idx], current_geom[i])
          }, error = function(e) NULL)

          if (is.null(inter) || length(inter) == 0 || inherits(inter, "sfc_EMPTY")) next

          total_inter <- sum(as.numeric(sf::st_area(inter)))

          if (total_inter > overlap_area_threshold * cur_area) {
            rows_to_remove <- c(rows_to_remove, current_polys$orig_row_id[i])
          }
        }
      }

      # Remove overlapping polygons from data
      if (length(rows_to_remove) > 0) {
        data <- data[!(data$orig_row_id %in% rows_to_remove), ]
      }

      n_removed_poly <- n_before_poly - nrow(data)
      if (n_removed_poly > 0) {
        rlang::inform(paste("  Supprimes :", n_removed_poly, "polygones chevauchants"))
      } else {
        rlang::inform("  Aucun chevauchement significatif")
      }

      sf_use_s2(FALSE)

    }, error = function(e) {
      rlang::warn(paste("Impossible de supprimer les chevauchements :", e$message))
      sf_use_s2(FALSE)
    })
  }

  # Etape 14 : filtre ecart-type localise (optionnel)
  if (isTRUE(params$apply_local_sd) && "Swath" %in% names(data) && !all(is.na(data$Swath))) {
    rlang::inform("Etape 14 : filtre ecart-type localise...")
    n_before <- nrow(data)
    current_orig_ids <- data$orig_row_id
    data <- apply_local_sd_filter(data,
      n_swaths = params$n_swaths %||% 5,
      lsd_limit = params$lsd_limit %||% 2.4,
      min_cells = params$min_cells %||% 3
    )
    n_removed <- n_before - nrow(data)
    if (n_removed > 0) {
      kept_ids <- data$orig_row_id
      removed_ids <- setdiff(current_orig_ids, kept_ids)
      if (length(removed_ids) > 0) {
        new_deletions <- tibble::tibble(
          orig_row_id = head(removed_ids, min(length(removed_ids), 1000)),
          step = "Filtre ET local",
          reason = "Variation locale elevee"
        )
        deletions <- dplyr::bind_rows(deletions, new_deletions)
      }
    }
    rlang::inform(paste("  Supprimes :", n_removed, "points"))
  } else {
    rlang::inform("Etape 14 : filtre ecart-type localise saute")
  }
  rlang::inform(paste("  Supprimes :", n_removed, "points"))
  
  # Etape 15 : filtre par passage
  rlang::inform("Etape 15 : filtre par passage...")
  if ("Pass" %in% names(data) && !all(is.na(data$Pass))) {
    pass_stats <- data |>
      sf::st_drop_geometry() |>
      dplyr::group_by(Pass) |>
      dplyr::summarise(
        mean_yield = mean(Yield_buacre, na.rm = TRUE),
        median_yield = median(Yield_buacre, na.rm = TRUE),
        n_points = dplyr::n(),
        .groups = "drop"
      )
    
    overall_median <- median(pass_stats$median_yield, na.rm = TRUE)
    pass_sd <- sd(pass_stats$median_yield, na.rm = TRUE)
    pass_threshold <- params$pass_threshold %||% 2.5
    
    abnormal_passes <- pass_stats |>
      dplyr::filter(abs(median_yield - overall_median) > pass_threshold * pass_sd) |>
      dplyr::pull(Pass)
    
    if (length(abnormal_passes) > 0) {
      n_before_pass <- nrow(data)
      data <- data |> dplyr::filter(!(Pass %in% abnormal_passes))
      n_removed_pass <- n_before_pass - nrow(data)
      
      if (n_removed_pass > 0) {
        removed_ids <- setdiff(current_orig_ids, data$orig_row_id)
      new_deletions <- tibble::tibble(
        orig_row_id = head(removed_ids, min(length(removed_ids), n_removed_pass)),
        step = "Filtre passage",
        reason = paste0("Passage anormal (mediane : ", 
                        round(pass_stats$median_yield[pass_stats$Pass %in% abnormal_passes][1], 1), 
                         " vs globale : ", round(overall_median, 1), ")")
      )
        deletions <- dplyr::bind_rows(deletions, new_deletions)
      }
      rlang::inform(paste("  Supprimes :", n_removed_pass, "points sur", length(abnormal_passes), "passages anormaux"))
    } else {
      rlang::inform("  Aucun passage anormal detecte")
    }
  } else {
    rlang::inform("  Ignore - pas de colonne Pass")
  }
  
  # Preparer la sortie finale
  data$orig_row_id <- NULL
  
  if (metrique) {
    data$Yield_kg_ha <- data$Yield_buacre * 67.25
    data$Flow_kg_s <- data$Flow * 0.453592
    yield_col <- "Yield_kg_ha"
    # Calculer aussi le rendement humide en kg/ha si disponible
    if ("Yield_buacre_wet" %in% names(data)) {
      data$Yield_kg_ha_wet <- data$Yield_buacre_wet * 67.25
    }
  } else {
    yield_col <- "Yield_buacre"
  }
  
    if (polygon) {
      # Create SF output
      # Detection automatique des unites pour Swath et Distance
      if (!"Swath_m" %in% names(data)) {
        mean_swath <- mean(data$Swath, na.rm = TRUE)
        if (mean_swath > 5) {
          data$Swath_m <- data$Swath  # Deja en metres
        } else {
          data$Swath_m <- data$Swath * 0.0254  # Conversion pouces -> metres
        }
      }
      if (!"Distance_m" %in% names(data)) {
        mean_dist <- mean(data$Distance, na.rm = TRUE)
        if (mean_dist > 0.5) {
          data$Distance_m <- data$Distance  # Deja en metres
        } else {
          data$Distance_m <- data$Distance * 0.0254  # Conversion pouces -> metres
        }
      }
      if (!"Altitude_m" %in% names(data)) {
        data$Altitude_m <- data$Altitude * 0.3048
      }
      
      sf_result <- data_to_sf(data, crs = 4326)
      
      if (is.null(sf_result)) {
        rlang::warn("Impossible de creer les polygones - retour des donnees sous forme de points")
        data_clean <- data
      } else {
        # Overlap removal already applied after bitmap filter
        data_clean <- sf_result
      }
    } else {
     data_clean <- data
   }
  
  # Statistiques
  stats <- list(
    n_raw = nrow(data_raw),
    n_clean = nrow(data_clean),
    n_deleted = nrow(data_raw) - nrow(data_clean),
    retention_rate = nrow(data_clean) / nrow(data_raw),
    flow_delay = flow_delay,
    thresholds = thresholds,
    deletions_by_step = deletions |>
      dplyr::group_by(step) |>
      dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
      dplyr::arrange(dplyr::desc(n))
  )
  
  rlang::inform("")
  rlang::inform("================================================")
  rlang::inform(paste("Termine :", nrow(data_clean), "observations nettoyees"))
  rlang::inform(paste("Taux de retention :", round(stats$retention_rate * 100, 1), "%"))
  rlang::inform("================================================")
  
  list(
    data_clean = data_clean,
    data_raw = data_raw |> dplyr::mutate(orig_row_id = dplyr::row_number()),
    deletions = deletions,
    stats = stats
  )
}
