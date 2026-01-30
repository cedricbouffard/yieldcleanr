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
                                    params = NULL, progress_callback = NULL) {

   # Progress helper function
   report_progress <- function(step, detail = NULL, progress = NULL) {
     if (!is.null(progress_callback)) {
       progress_callback(list(step = step, detail = detail, progress = progress))
     }
     if (!is.null(detail)) {
       rlang::inform(paste(">>> PROGRESS:", step, "-", detail))
     }
   }

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
   
   # Flow est humide par defaut - creer Yield_kg_ha_wet a partir de Flow
   if ("Flow" %in% names(data) && !all(is.na(data$Flow))) {
     if (!"Yield_kg_ha_wet" %in% names(data)) {
       rlang::inform("Flow detecte - creation de Yield_kg_ha_wet a partir du flux humide...")
       data$Yield_kg_ha_wet <- data$Flow
     }
   }
   
   # Creer Yield_kg_ha_wet si Flow_Wet existe egalement
   if ("Flow_Wet" %in% names(data) && !all(is.na(data$Flow_Wet))) {
     if (!"Yield_kg_ha_wet" %in% names(data)) {
       rlang::inform("Creation de Yield_kg_ha_wet a partir de Flow_Wet...")
       data$Yield_kg_ha_wet <- data$Flow_Wet
     }
   }
   
     # Calculer le rendement sec a partir du rendement humide et de l'humidite
     # Formule: Rendement sec = Rendement humide × (100 - Humidite%) / (100 - Humidite_standard%)
     if ("Moisture" %in% names(data) && !all(is.na(data$Moisture))) {
       # Obtenir l'humidite standard selon la culture
       moisture_std <- get_standard_moisture(data)
       moisture_factor <- 100 - moisture_std
       
       if ("Yield_kg_ha_wet" %in% names(data) && !all(is.na(data$Yield_kg_ha_wet))) {
         rlang::inform(paste("Calcul du rendement sec (humidite standard:", moisture_std, "%)..."))
         data$Yield_kg_ha <- data$Yield_kg_ha_wet * (100 - data$Moisture) / moisture_factor
         rlang::inform(paste("Rendement sec calcule:", round(mean(data$Yield_kg_ha, na.rm = TRUE), 1), "kg/ha"))
       }
       # Convertir Flow (humide) en Flow sec pour les filtres
       if ("Flow" %in% names(data) && !all(is.na(data$Flow))) {
         data$Flow <- data$Flow * (100 - data$Moisture) / moisture_factor
       }
    } else {
      # Pas d'humidite - Yield_kg_ha est le meme que Yield_kg_ha_wet
      if ("Yield_kg_ha_wet" %in% names(data) && !"Yield_kg_ha" %in% names(data)) {
        data$Yield_kg_ha <- data$Yield_kg_ha_wet
        rlang::inform(paste("Pas d'humidite - Yield_kg_ha = Yield_kg_ha_wet (moyenne:", round(mean(data$Yield_kg_ha, na.rm = TRUE), 1), "kg/ha)"))
      }
    }
  
    # Etape 2 : conversion UTM
    report_progress("Conversion", "conversion en coordonnees UTM...", 0.05)
    rlang::inform("Etape 2 : conversion en coordonnees UTM...")
   data <- latlon_to_utm(data)

   # Etape 2b : filtre position (optionnel)
   if (isTRUE(params$apply_position)) {
     report_progress("Filtres", "filtre position...", 0.06)
     rlang::inform("Etape 2b : filtre position...")
     n_before <- nrow(data)
     data <- filter_position_outliers(data)
     n_removed <- n_before - nrow(data)
     if (n_removed > 0) {
       removed_ids <- setdiff(data_raw$orig_row_id[1:n_before], data$orig_row_id)
       new_deletions <- tibble::tibble(
         orig_row_id = head(removed_ids, min(length(removed_ids), n_removed)),
         step = "Filtre position",
         reason = "Position aberrante"
       )
       deletions <- dplyr::bind_rows(deletions, new_deletions)
     }
     rlang::inform(paste("  Supprimes :", n_removed, "points"))
   } else {
     rlang::inform("Etape 2b : filtre position saute")
   }
   
   # Etape 3 : PCDI - optimisation du delai de flux
   # Verifier si les donnees contiennent deja des colonnes de rendement (sans Flow)
   has_yield_cols <- "Yield_kg_ha" %in% names(data)
  has_flow_col <- "Flow" %in% names(data) && !all(is.na(data$Flow))
  
  # PCDI flux - optionnel
  flow_delay <- 0
   if (isTRUE(params$apply_pcdi_flow)) {
     if (has_yield_cols && !has_flow_col) {
       # Donnees John Deere : calculer Flow a partir de Yield pour PCDI
       report_progress("PCDI", "calcul du flux a partir du rendement...", 0.08)
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
      
       # Utiliser Yield_kg_ha pour calculer Flow si disponible
       if ("Yield_kg_ha" %in% names(data)) {
         # Conversion kg/ha -> kg/s approximatif
         # Flow (kg/s) = Yield (kg/ha) * Swath (m) * Distance (m) / (Interval (s) * 10000 m2/ha)
         data <- data |>
           dplyr::mutate(
             Flow = .data$Yield_kg_ha * .data$Swath * .data$Distance / 
                    (.data$Interval * 10000)
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
         report_progress("PCDI", "optimisation du delai de flux...", 0.10)
         rlang::inform("Etape 3 : PCDI - optimisation du delai de flux...")
         pcdi_result <- apply_pcdi(data,
           delay_range = params$delay_range %||% -25:25,
           n_iterations = params$n_iterations %||% 10,
           noise_level = params$noise_level %||% 0.03,
           sample_fraction = params$sample_fraction %||% 1,
           method = "Moran"
         )
        flow_delay <- pcdi_result$optimal_delay
        rlang::inform(paste("  Delai optimal :", flow_delay, "secondes"))
      }
     } else {
       # Donnees avec Flow existant : PCDI normal
       report_progress("PCDI", "optimisation du delai de flux...", 0.10)
       rlang::inform("Etape 3 : PCDI - optimisation du delai de flux...")
       pcdi_result <- apply_pcdi(data,
         delay_range = params$delay_range %||% -25:25,
         n_iterations = params$n_iterations %||% 10,
         noise_level = params$noise_level %||% 0.03,
         value_col = "Flow",
         sample_fraction = params$sample_fraction %||% 1,
         method = "Moran"
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
     report_progress("PCDI", "optimisation du delai d'humidite...", 0.12)
     rlang::inform("Etape 3b : PCDI - optimisation du delai d'humidite...")
     pcdi_moisture <- apply_pcdi(data,
       delay_range = params$delay_range %||% -25:25,
       n_iterations = params$n_iterations %||% 10,
       noise_level = params$noise_level %||% 0.03,
       value_col = "Moisture",
       sample_fraction = params$sample_fraction %||% 1,
       method = "Moran"
     )
    moisture_delay <- pcdi_moisture$optimal_delay
    rlang::inform(paste("  Delai optimal humidite:", moisture_delay, "secondes"))
  } else {
    rlang::inform("Etape 3b : PCDI humidite saute (desactive ou donnees non disponibles)")
  }

    # Etape 3c : calcul du rendement initial
    report_progress("Preparation", "calcul du rendement...", 0.14)
    data <- convert_flow_to_yield(data)
    
    # Debug: afficher le rendement initial
    if ("Yield_kg_ha" %in% names(data)) {
      mean_yield_init <- mean(data$Yield_kg_ha, na.rm = TRUE)
      rlang::inform(paste("  Rendement initial (apres convert_flow_to_yield):", round(mean_yield_init, 1), "kg/ha"))
      rlang::inform(paste("  Flow min/max:", round(min(data$Flow, na.rm = TRUE), 3), "/", round(max(data$Flow, na.rm = TRUE), 3)))
      rlang::inform(paste("  Swath min/max:", round(min(data$Swath, na.rm = TRUE), 2), "/", round(max(data$Swath, na.rm = TRUE), 2)))
      rlang::inform(paste("  Distance min/max:", round(min(data$Distance, na.rm = TRUE), 2), "/", round(max(data$Distance, na.rm = TRUE), 2)))
    }

   # Etape 4 : seuils automatiques
   report_progress("Seuils", "calcul des seuils automatiques...", 0.16)
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
  
   # Etape 5 : filtre header (optionnel) - DEPLACE APRES LE DELAI DE FLUX (Etape 9f)
   # Note: Le filtre header est maintenant applique apres le delai de flux pour eviter
   # de supprimer des points qui pourraient devenir valides apres correction du delai
   rlang::inform("Etape 5 : filtre header deplace apres le delai de flux (Etape 12)")
   # if (isTRUE(params$apply_header) && "HeaderStatus" %in% names(data)) {
   #   report_progress("Filtres", "filtre header...", 0.18)
   #   rlang::inform("Etape 5 : filtre header...")
   #  to_keep <- dplyr::filter(data, HeaderStatus %in% c(0, 1, 33) | is.na(HeaderStatus))
   #  new_deletions <- dplyr::setdiff(data, to_keep) |>
   #    dplyr::mutate(step = "Filtre header", reason = "HeaderStatus inactif")
   #  deletions <- dplyr::bind_rows(deletions, new_deletions |> dplyr::select(orig_row_id, step, reason))
   #  data <- to_keep
   #  rlang::inform(paste("  Supprimes :", nrow(new_deletions), "points"))
   # } else {
   #   rlang::inform("Etape 5 : filtre header saute")
   # }

   # Etape 6 : filtre GPS (optionnel)
   if (isTRUE(params$apply_gps) && all(c("Latitude", "Longitude") %in% names(data))) {
     report_progress("Filtres", "filtre GPS...", 0.20)
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
   report_progress("Preparation", "calcul de la vitesse...", 0.22)
   rlang::inform("Etape 7 : calcul de la vitesse...")
  data <- data |>
    dplyr::mutate(
      velocity = sqrt((X - dplyr::lag(X))^2 + (Y - dplyr::lag(Y))^2) /
                    dplyr::coalesce(Interval, 1)
    )
  data$velocity[is.na(data$velocity) | !is.finite(data$velocity)] <- 0

   # Etape 8 : filtre vitesse (optionnel)
   if (isTRUE(params$apply_velocity)) {
     report_progress("Filtres", "filtre vitesse...", 0.24)
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
     report_progress("Filtres", "filtre changements de vitesse...", 0.26)
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
     report_progress("Filtres", "filtre anomalies de direction...", 0.28)
     rlang::inform("Etape 8c : filtre anomalies de direction...")
    result <- filter_heading_anomalies(
      data,
      max_heading_change = params$max_heading_change %||% 60
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
     report_progress("Correction", paste0("correction du delai de flux (", flow_delay, "s)..."), 0.30)
     rlang::inform(paste("Etape 9 : correction du delai de flux (", flow_delay, "s)..."))
     # Si flow_delay positif (+15s), Flow est en retard, donc appliquer +15s pour decaler Flow vers l'avant
     # lead() decale les valeurs vers l'arriere, lag() vers l'avant
     data <- apply_flow_delay(data, delay = flow_delay, value_col = "Flow")
     
     # Interpoler les NA crees par le decalage
     if (sum(is.na(data$Flow)) > 0) {
       data <- interpolate_na(data, value_col = "Flow")
     }

       # Recalcul du rendement apres correction du delai
      data <- convert_flow_to_yield(data, force_recalculate = TRUE)
      
      # Debug: afficher le rendement apres correction du delai
      if ("Yield_kg_ha" %in% names(data)) {
        mean_yield_corrected <- mean(data$Yield_kg_ha, na.rm = TRUE)
        rlang::inform(paste("  Rendement apres correction delai:", round(mean_yield_corrected, 1), "kg/ha"))
      }
   } else {
     rlang::inform("Etape 9 : pas de correction de delai de flux necessaire (delai = 0)")
   }
  
    # Etape 10 : correction du delai d'humidite (si necessaire)
    if (moisture_delay != 0 && "Moisture" %in% names(data)) {
      rlang::inform(paste("Etape 10 : correction du delai d'humidite (", moisture_delay, "s)..."))
     data <- apply_flow_delay(data, delay = moisture_delay, value_col = "Moisture")
     
     # Interpoler les NA crees par le decalage
     if (sum(is.na(data$Moisture)) > 0) {
       data <- interpolate_na(data, value_col = "Moisture")
     }
    } else {
      rlang::inform("Etape 10 : pas de correction de delai d'humidite necessaire (delai = 0)")
    }
  
   # Etape 11 : recalcul des seuils
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
   
     # NOTE - Le delai de flux DECALE les valeurs, il ne supprime pas de points
    # Les valeurs NA crees sont interpolees
    rlang::inform("  (Delai de flux: les valeurs sont decalees et interpolees, tous les points sont conserves)")
    
     # Etape 12 : filtre header (optionnel) - DEPLACE ICI APRES LE DELAI DE FLUX
     # Le filtre header est applique apres le delai de flux pour eviter de supprimer
     # des points qui pourraient devenir valides apres correction du delai
     if (isTRUE(params$apply_header) && "HeaderStatus" %in% names(data)) {
       report_progress("Filtres", "filtre header (apres delai)...", 0.32)
       rlang::inform("Etape 12 : filtre header (apres delai de flux)...")
      to_keep <- dplyr::filter(data, HeaderStatus %in% c(0, 1, 33) | is.na(HeaderStatus))
      new_deletions <- dplyr::setdiff(data, to_keep) |>
        dplyr::mutate(step = "Filtre header", reason = "HeaderStatus inactif")
      deletions <- dplyr::bind_rows(deletions, new_deletions |> dplyr::select(orig_row_id, step, reason))
      data <- to_keep
      rlang::inform(paste("  Supprimes :", nrow(new_deletions), "points"))
    } else {
      rlang::inform("Etape 12 : filtre header saute")
    }
    
     # Etape 13 : suppression des rendements nuls (optionnel)
     if (isTRUE(params$apply_null_yield) && "Yield_kg_ha" %in% names(data)) {
       report_progress("Filtres", "suppression des rendements nuls...", 0.34)
       rlang::inform("Etape 13 : suppression des rendements nuls...")
     to_keep <- dplyr::filter(data, Yield_kg_ha > 0)
     new_deletions <- dplyr::setdiff(data, to_keep) |>
       dplyr::mutate(step = "Rendement nul", reason = "Rendement = 0")
     deletions <- dplyr::bind_rows(deletions, new_deletions |> dplyr::select(orig_row_id, step, reason))
     data <- to_keep
     rlang::inform(paste("  Supprimes :", nrow(new_deletions), "points"))
    } else {
      rlang::inform("Etape 13 : suppression des rendements nuls saute")
    }

     # Etape 14 : filtre plage de rendement (optionnel)
     if (isTRUE(params$apply_yield_range) && "Yield_kg_ha" %in% names(data)) {
       report_progress("Filtres", "filtre plage de rendement...", 0.36)
       rlang::inform("Etape 14 : filtre plage de rendement...")
     to_keep <- dplyr::filter(data,
       Yield_kg_ha >= thresholds$min_yield &
       Yield_kg_ha <= thresholds$max_yield)
     new_deletions <- dplyr::setdiff(data, to_keep) |>
       dplyr::mutate(
         step = "Filtre plage rendement",
         reason = paste0("Rendement hors plage [", round(thresholds$min_yield, 2), ", ", round(thresholds$max_yield, 2), "] kg/ha")
       )
     deletions <- dplyr::bind_rows(deletions, new_deletions |> dplyr::select(orig_row_id, step, reason))
     data <- to_keep
     rlang::inform(paste("  Supprimes :", nrow(new_deletions), "points"))
    } else {
      rlang::inform("Etape 14 : filtre plage de rendement saute")
    }

    # Etape 15 : filtre humidite (optionnel)
    if (isTRUE(params$apply_moisture) && "Moisture" %in% names(data) && !all(is.na(data$Moisture))) {
      report_progress("Filtres", "filtre humidite...", 0.38)
      rlang::inform("Etape 15 : filtre humidite (auto-detection)...")
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
     rlang::inform("Etape 15 : filtre humidite saute")
   }
   
     # Etape 12b : lissage du cap pour les polygones
     report_progress("Preparation", "lissage du cap pour les polygones...", 0.40)
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
   
     # Etape 16 : filtre de chevauchement (optionnel)
     if (isTRUE(params$apply_overlap) && "Swath" %in% names(data) && !all(is.na(data$Swath))) {
       report_progress("Filtres", "filtre de chevauchement...", 0.45)
       rlang::inform("Etape 16 : filtre de chevauchement bitmap...")
     n_before <- nrow(data)
     current_orig_ids <- data$orig_row_id
      data <- apply_overlap_filter(data,
        cellsize = params$cellsize_overlap %||% 0.3,
        overlap_threshold = params$overlap_threshold %||% 0.4  # 40% au lieu de 50%
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
       rlang::inform("Etape 16 : filtre de chevauchement saute")
     }

    # NOTE: L'etape 13b precedente (filtre polygones chevauchants) a ete retiree
    # car trop lente pour grandes donnees. Le filtre bitmap (etape 13) est maintenant
    # plus strict pour compenser.
    rlang::inform("  (Filtre polygones chevauchants desactive pour performance)")

     # Etape 17 : filtre ecart-type localise (optionnel)
    if (isTRUE(params$apply_local_sd) && "Swath" %in% names(data) && !all(is.na(data$Swath))) {
      report_progress("Filtres", "filtre ecart-type localise...", 0.65)
      rlang::inform("Etape 17 : filtre ecart-type localise...")
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
      rlang::inform("Etape 17 : filtre ecart-type localise saute")
    }

     # Etape 18 : filtre par passage
    report_progress("Filtres", "filtre par passage...", 0.70)
    rlang::inform("Etape 18 : filtre par passage...")
  if ("Pass" %in% names(data) && !all(is.na(data$Pass))) {
     pass_stats <- data |>
       sf::st_drop_geometry() |>
       dplyr::group_by(Pass) |>
       dplyr::summarise(
         mean_yield = mean(Yield_kg_ha, na.rm = TRUE),
         median_yield = median(Yield_kg_ha, na.rm = TRUE),
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
  
    # Creer Yield_kg_ha si n'existe pas
     if (!"Yield_kg_ha" %in% names(data)) {
       if ("Flow" %in% names(data) && mean(data$Flow, na.rm = TRUE) > 100) {
         # Flow existe avec valeurs > 100, utiliser comme rendement humide (kg/ha)
         data$Yield_kg_ha <- data$Flow
         rlang::inform(paste("Yield_kg_ha cree a partir de Flow (moyenne:", round(mean(data$Yield_kg_ha, na.rm = TRUE), 0), "kg/ha)"))
       }
     }
     # Creer Yield_kg_ha_wet si n'existe pas
     if (!"Yield_kg_ha_wet" %in% names(data) && "Yield_kg_ha" %in% names(data)) {
       data$Yield_kg_ha_wet <- data$Yield_kg_ha
     }
     if ("Flow" %in% names(data)) {
       data$Flow_kg_s <- data$Flow * 0.453592
     }
     yield_col <- "Yield_kg_ha"
  
     if (polygon) {
       report_progress("Polygones", "creation des polygones...", 0.80)
       # Create SF output
       # Detection automatique des unites pour Swath et Distance
        if (!"Swath_m" %in% names(data)) {
          mean_swath <- mean(data$Swath, na.rm = TRUE)
          # Headers typiques: 6-15m (20-50 pieds)
          # En pouces: 240-600 pouces (6-15m)
          # Un swath reel ne peut pas etre < 3m
          
          if (mean_swath > 100) {
            # > 100: probablement pieds ou valeur aberrante
            if (mean_swath > 200) {
              rlang::warn(paste("Swath moyen tres eleve (", round(mean_swath, 1), ") - verifiez les unites. Traitement avec 8m par defaut."))
              data$Swath_m <- 8
            } else {
              # 100-200: probablement pieds
              rlang::inform(paste("Swath detecte en pieds (moyenne:", round(mean_swath, 1), "ft) - conversion en metres"))
              data$Swath_m <- data$Swath * 0.3048
            }
          } else if (mean_swath >= 3 && mean_swath <= 50) {
            # 3-50m: plage normale pour un swath en metres
            rlang::inform(paste("Swath detecte en metres (moyenne:", round(mean_swath, 2), "m)"))
            data$Swath_m <- data$Swath
          } else if (mean_swath < 3) {
            # < 3m: probablement pouces (un swath reel ne peut pas etre < 3m)
            rlang::inform(paste("Swath detecte en pouces (moyenne:", round(mean_swath, 1), "in) - conversion en metres"))
            data$Swath_m <- data$Swath * 0.0254
          } else {
            # Cas non prevu
            rlang::warn(paste("Swath de", round(mean_swath, 1), "m - unite incertaine, utilisee telle quelle"))
            data$Swath_m <- data$Swath
          }
        }
       if (!"Distance_m" %in% names(data)) {
         mean_dist <- mean(data$Distance, na.rm = TRUE)
         if (mean_dist > 30) {
           rlang::inform(paste("Distance detectee en pieds (moyenne:", round(mean_dist, 1), "ft) - conversion en metres"))
           data$Distance_m <- data$Distance * 0.3048
         } else if (mean_dist > 0.5) {
           rlang::inform(paste("Distance detectee en metres (moyenne:", round(mean_dist, 2), "m)"))
           data$Distance_m <- data$Distance
         } else {
           rlang::inform(paste("Distance detectee en pouces (moyenne:", round(mean_dist, 1), "in) - conversion en metres"))
           data$Distance_m <- data$Distance * 0.0254
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
     data = data_clean,
     data_clean = data_clean,
     data_raw = data_raw |> dplyr::mutate(orig_row_id = dplyr::row_number()),
     deletions = deletions,
     removed_points = deletions,
     stats = stats
   )
}
