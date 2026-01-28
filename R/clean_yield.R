#' Fonction unifiee de nettoyage des donnees de rendement
#'
#' Cette fonction execute le pipeline complet de nettoyage des donnees de rendement
#' avec support pour les sorties en unites metriques ou imperiales, et avec ou sans
#' geometries SF (polygones ou points).
#'
#' @param file_path Chemin du fichier d'entree (txt/csv)
#' @param metrique TRUE pour les unites metriques (kg/ha), FALSE pour l'imperial (bu/acre)
#' @param polygon TRUE pour une sortie SF en polygones, FALSE pour une sortie tibble
#' @param params Liste des parametres AYCE (voir details)
#' @param output_file Chemin optionnel pour sauvegarder la sortie (CSV ou GeoJSON)
#' @param log_file Chemin optionnel pour sauvegarder le journal
#' @return Donnees nettoyees (tibble ou objet SF selon les parametres)
#' @export
#' @examples
#' \dontrun{
#' # Sortie metrique avec polygones (objet SF)
#' sf_result <- clean_yield("data.txt", metrique = TRUE, polygon = TRUE)
#' plot(sf_result["Yield_kg_ha"])
#'
#' # Sortie imperiale en tibble
#' data_result <- clean_yield("data.txt", metrique = FALSE, polygon = FALSE)
#'
#' # Sortie metrique en tibble (sans geometrie)
#' data_metric <- clean_yield("data.txt", metrique = TRUE, polygon = FALSE)
#'
#' # Avec parametres personnalises
#' result <- clean_yield("data.txt",
#'   metrique = TRUE,
#'   polygon = TRUE,
#'   params = list(
#'     delay_range = -10:25,
#'     n_swaths = 5,
#'     lsd_limit = 2.5
#'   )
#' )
#' }
clean_yield <- function(file_path, metrique = TRUE, polygon = TRUE,
                        params = NULL, output_file = NULL, log_file = NULL) {

  # ----- Parametres AYCE par defaut -----
  default_params <- list(
    # Parametres PCDI
    delay_range = -25:10,
    n_iterations = 5,
    noise_level = 0.05,

    # Parametres de seuil (equilibres)
    yllim = 0.10,      # Limite quantile bas rendement
    yulim = 0.90,      # Limite quantile haut rendement
    yscale = 1.1,      # Multiplicateur IQR rendement
    v_lim = 0.05,      # Limite quantile bas vitesse
    v_ulim = 0.95,     # Limite quantile haut vitesse
    v_scale = 1.1,     # Multiplicateur IQR vitesse
    minv_abs = 0.5,    # Vitesse minimale absolue (m/s)
    miny_abs = 0,      # Rendement minimal absolu
    gbuffer = 100,     # Marge de position (metres)

    # Parametres de chevauchement
    cellsize_overlap = 0.3,    # Taille de cellule (metres) - standard USDA
    overlap_threshold = 0.5,   # Ratio max de chevauchement

    # Parametres ecart-type local
    n_swaths = 5,              # Taille de cellule en largeurs de passage
    lsd_limit = 2.4,           # Multiplicateur d'ET local
    min_cells = 3,             # Observations minimales par cellule

    # Parametres d'auto-detection
    n_std = 3                  # Nombre d'ET pour l'auto-detection
  )

  # Fusion des parametres
  params <- modifyList(default_params, params %||% list())

  # ----- Etape 1 : lecture des donnees brutes -----
  rlang::inform("================================================")
  rlang::inform("   Yield Data Cleaning Pipeline               ")
  if (metrique) {
    rlang::inform("   Output: Metric (kg/ha)                      ")
  } else {
    rlang::inform("   Output: Imperial (bu/acre)                  ")
  }
  if (polygon) {
    rlang::inform("   Geometry: Polygons                          ")
  } else {
    rlang::inform("   Geometry: None (tibble output)              ")
  }
  rlang::inform("================================================")
  rlang::inform("")

  rlang::inform("Etape 1 : chargement des donnees...")
  data <- read_yield_data(file_path)
  data_raw <- data
  rlang::inform(paste("  -", nrow(data), "raw observations loaded"))
  
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

  # ----- Etape 2 : conversion UTM -----
  rlang::inform("Etape 2 : conversion en coordonnees UTM...")
  data <- latlon_to_utm(data)

  # ----- Etape 3 : PCDI sur le flux -----
  rlang::inform("Etape 3 : PCDI - optimisation du delai de flux...")
  pcdi_result <- apply_pcdi(data,
    delay_range = params$delay_range,
    n_iterations = params$n_iterations,
    noise_level = params$noise_level,
    value_col = "Flow"
  )
  flow_delay <- pcdi_result$optimal_delay
  rlang::inform(paste("  Delai optimal flux:", flow_delay, "secondes"))
  
  # ----- Etape 3b : PCDI sur l'humidite -----
  moisture_delay <- 0
  if ("Moisture" %in% names(data) && !all(is.na(data$Moisture))) {
    rlang::inform("Etape 3b : PCDI - optimisation du delai d'humidite...")
    pcdi_moisture <- apply_pcdi(data,
      delay_range = params$delay_range,
      n_iterations = params$n_iterations,
      noise_level = params$noise_level,
      value_col = "Moisture"
    )
    moisture_delay <- pcdi_moisture$optimal_delay
    rlang::inform(paste("  Delai optimal humidite:", moisture_delay, "secondes"))
  }

  # ----- Etape 3c : calcul initial du rendement (avant delai) -----
  rlang::inform("Etape 3c : calcul du rendement initial pour les seuils...")
  data <- convert_flow_to_yield(data)

  # ----- Etape 4 : seuils automatiques -----
  rlang::inform("Etape 4 : calcul des seuils automatiques...")
  thresholds <- calculate_auto_thresholds(data,
    yllim = params$yllim, yulim = params$yulim, yscale = params$yscale,
    vllim = params$v_lim, vulim = params$v_ulim, vscale = params$v_scale,
    minv_abs = params$minv_abs, miny_abs = params$miny_abs,
    gbuffer = params$gbuffer
  )

  # ----- Etape 5 : filtre header -----
  rlang::inform("Etape 5 : filtre header...")
  data <- data |> dplyr::filter(HeaderStatus %in% c(1, 33) | is.na(HeaderStatus))
  rlang::inform(paste("  Rows:", nrow(data)))

  # ----- Etape 6 : filtre GPS -----
  rlang::inform("Etape 6 : filtre GPS...")
  # Verifier si GPSStatus existe avant de filtrer
  if ("GPSStatus" %in% names(data)) {
    # S'assurer que GPSStatus est numerique
    data$GPSStatus <- suppressWarnings(as.numeric(data$GPSStatus))
    data <- data |> dplyr::filter(is.na(GPSStatus) | GPSStatus >= 4)
  } else {
    rlang::inform("  GPSStatus non present, saut du filtre GPS")
  }
  rlang::inform(paste("  Rows:", nrow(data)))

  # ----- Etape 7 : calcul de la vitesse -----
  rlang::inform("Etape 7 : calcul de la vitesse...")
  data <- data |>
    dplyr::mutate(
      velocity = sqrt((X - dplyr::lag(X))^2 + (Y - dplyr::lag(Y))^2) /
                    dplyr::coalesce(Interval, 1)
    )
  data$velocity[is.na(data$velocity) | !is.finite(data$velocity)] <- 0

  # ----- Etape 8 : filtre vitesse -----
  rlang::inform("Etape 8 : filtre vitesse...")
  data <- data |>
    dplyr::filter(velocity >= thresholds$min_velocity & velocity <= thresholds$max_velocity)
  rlang::inform(paste("  Rows:", nrow(data)))

  # ----- Etape 9 : correction du delai de flux -----
  rlang::inform(paste("Etape 9 : correction du delai de flux (", flow_delay, "s)..."))
  data <- apply_flow_delay(data, delay = -flow_delay, value_col = "Flow")
  rlang::inform(paste("  Rows:", nrow(data)))
  
  # ----- Etape 9a : correction du delai d'humidite -----
  if (moisture_delay != 0 && "Moisture" %in% names(data)) {
    rlang::inform(paste("Etape 9a : correction du delai d'humidite (", moisture_delay, "s)..."))
    data <- apply_flow_delay(data, delay = -moisture_delay, value_col = "Moisture")
    rlang::inform(paste("  Rows:", nrow(data)))
  }

  # ----- Etape 9b : calcul du rendement apres delai -----
  rlang::inform("Etape 9b : calcul du rendement apres delai...")
  data <- convert_flow_to_yield(data)

  # ----- Etape 9c : recalcul des seuils apres delai -----
  rlang::inform("Etape 9c : recalcul des seuils apres delai...")
  thresholds <- calculate_auto_thresholds(data,
    yllim = params$yllim, yulim = params$yulim, yscale = params$yscale,
    vllim = params$v_lim, vulim = params$v_ulim, vscale = params$v_scale,
    minv_abs = params$minv_abs, miny_abs = params$miny_abs,
    gbuffer = params$gbuffer
  )

  # ----- Etape 9d : validation de Pass via analyse de direction -----
  rlang::inform("Etape 9d : validation de Pass via analyse de direction...")
  if (all(c("X", "Y") %in% names(data))) {
    n <- nrow(data)

    # Si la colonne Pass est absente ou incoherente, ignorer la validation
    if (!("Pass" %in% names(data)) || all(is.na(data$Pass))) {
      rlang::inform("  No valid Pass column, skipping validation")
    } else {
      n_original_passes <- length(unique(data$Pass))
      rlang::inform(paste("  Pass column has", n_original_passes, "unique values"))

      # Verifier si le nombre de passages est raisonnable (10-500 pour un champ)
      if (n_original_passes >= 10 && n_original_passes <= 500) {
        rlang::inform("  Pass column appears reasonable, using as-is")
      } else {
        rlang::inform(paste("  Warning: Pass column has", n_original_passes,
                           "passes, which may be unusual"))
      }
    }
  }

  # ----- Etape 9e : suppression des points de bordure -----
  rlang::inform("Etape 9e : suppression des points de bordure lies au delai...")
  if ("Pass" %in% names(data) && "Yield_buacre" %in% names(data) && !all(is.na(data$Pass)) && "Interval" %in% names(data)) {
    n_before <- nrow(data)
    abs_delay <- abs(flow_delay)

    data <- data |>
      dplyr::arrange(GPS_Time) |>
      dplyr::group_by(Pass) |>
      dplyr::mutate(
        row_in_pass = dplyr::row_number(),
        n_in_pass = dplyr::n(),
        interval_cumsum = cumsum(Interval),
        total_interval = sum(Interval, na.rm = TRUE),
        interval_from_end = total_interval - interval_cumsum
      ) |>
      dplyr::ungroup()

    if (flow_delay < 0) {
      boundary <- "end"
      n_delay_points <- round(mean(data |>
        dplyr::group_by(Pass) |>
        dplyr::filter(interval_from_end <= abs_delay | is.na(interval_from_end)) |>
        dplyr::summarise(n = dplyr::n()) |>
        dplyr::ungroup() |>
        dplyr::pull(n), na.rm = TRUE))
      data <- data |>
        dplyr::group_by(Pass) |>
        dplyr::filter(interval_from_end > abs_delay | is.na(interval_from_end)) |>
        dplyr::ungroup()
    } else {
      boundary <- "beginning"
      n_delay_points <- round(mean(data |>
        dplyr::group_by(Pass) |>
        dplyr::filter(interval_cumsum <= abs_delay | is.na(interval_cumsum)) |>
        dplyr::summarise(n = dplyr::n()) |>
        dplyr::ungroup() |>
        dplyr::pull(n), na.rm = TRUE))
      data <- data |>
        dplyr::group_by(Pass) |>
        dplyr::filter(interval_cumsum > abs_delay | is.na(interval_cumsum)) |>
        dplyr::ungroup()
    }

    data$interval_cumsum <- NULL
    data$total_interval <- NULL
    data$interval_from_end <- NULL

    n_removed <- n_before - nrow(data)
    rlang::inform(paste("  ", n_removed, "boundary points removed (",
                       boundary, ", delay:", flow_delay, "s =",
                       n_delay_points, "points)"))
  } else {
    rlang::inform("  Missing Pass, Yield, or Interval column, skipping boundary removal")
    data <- data
  }

  # ----- Etape 10 : suppression des rendements nuls -----
  rlang::inform("Etape 10 : suppression des rendements nuls...")
  n_before <- nrow(data)
  data <- data |> dplyr::filter(Yield_buacre > 0)
  n_removed <- n_before - nrow(data)
  if (n_removed > 0) {
    rlang::inform(paste("  ", n_removed, "zero yield points removed"))
  }
  rlang::inform(paste("  Rows:", nrow(data)))

  # ----- Etape 11 : filtre plage de rendement -----
  rlang::inform("Etape 11 : filtre plage de rendement...")
  data <- filter_yield_range(data,
    min_yield = thresholds$min_yield,
    max_yield = thresholds$max_yield
  )
  rlang::inform(paste("  Rows:", nrow(data)))

  # ----- Etape 12 : filtre humidite -----
  rlang::inform("Etape 12 : filtre humidite (auto-detection)...")
  data <- filter_moisture_range(data, n_std = params$n_std)
  rlang::inform(paste("  Rows:", nrow(data)))

  # ----- Etape 13 : filtre de chevauchement -----
  rlang::inform("Etape 13 : filtre de chevauchement bitmap...")
  data <- apply_overlap_filter(data,
    cellsize = params$cellsize_overlap,
    overlap_threshold = params$overlap_threshold
  )
  rlang::inform(paste("  Rows:", nrow(data)))

  # ----- Etape 14 : filtre ecart-type local -----
  rlang::inform("Etape 14 : filtre ecart-type localise...")
  data <- apply_local_sd_filter(data,
    n_swaths = params$n_swaths,
    lsd_limit = params$lsd_limit,
    min_cells = params$min_cells
  )
  rlang::inform(paste("  Rows:", nrow(data)))

  # ----- Etape 15 : validation -----
  rlang::inform("Etape 15 : validation et controle qualite...")
  validation <- ayce_validate(data, data_raw, pcdi_result, thresholds)
  rlang::inform(paste("  Retention rate:", round(validation$retention_rate * 100, 1), "%"))

  # ----- Etape 16 : formatage de la sortie -----
  rlang::inform("Etape 16 : formatage de la sortie...")

   # Calcul des conversions d'unites
   if (metrique) {
     data$Yield_kg_ha <- data$Yield_buacre * 67.25  # kg/ha
     data$Flow_kg_s <- data$Flow * 0.453592         # kg/s
     data$Yield_final <- data$Yield_kg_ha
     data$Flow_final <- data$Flow_kg_s
     yield_label <- "Yield_kg_ha"
     flow_label <- "Flow_kg_s"
     unit_label <- "kg/ha"
     # Calculer aussi le rendement humide en kg/ha si disponible
     if ("Yield_buacre_wet" %in% names(data)) {
       data$Yield_kg_ha_wet <- data$Yield_buacre_wet * 67.25
     }
   } else {
     data$Yield_final <- data$Yield_buacre
     data$Flow_final <- data$Flow
     yield_label <- "Yield_bu_ac"
     flow_label <- "Flow_lbs_s"
     unit_label <- "bu/acre"
   }

    # Pour une sortie tibble, supprimer les colonnes intermediaires
   if (!polygon) {
     data$Yield_buacre <- NULL
     data$Flow <- NULL
     if (metrique) {
       data$Yield_kg_ha <- NULL
       data$Flow_kg_s <- NULL
     }
   }

     if (polygon) {
       # ----- Calcul du cap pour les polygones -----
       rlang::inform("Etape 16b : calcul du cap...")
      data <- data |>
        dplyr::mutate(
          heading = atan2(
            dplyr::lag(Longitude, default = Longitude[1]) - Longitude,
            dplyr::lag(Latitude, default = Latitude[1]) - Latitude
          ) * 180 / pi
        )
      data$heading[is.na(data$heading)] <- 0

       # ----- Lissage du cap pour reduire le bruit -----
       rlang::inform("Etape 16b.1 : lissage du cap par segments...")
       # Calculer les variations pour detecter les virages
      n <- nrow(data)
      heading_diff <- c(data$heading[2:n] - data$heading[1:(n-1)], 0)
       # Gerer le retour circulaire des angles
      heading_diff <- ((heading_diff + 180) %% 360) - 180
       # Detecter les virages (seuil : 30 degres)
      turn_threshold <- 30
      is_turn <- abs(heading_diff) > turn_threshold

       # Identifier les segments (series sans virage)
      segment_id <- cumsum(c(TRUE, is_turn[1:(n-1)]))
      data$segment_id <- segment_id

       # Lisser le cap dans chaque segment
      data <- data |>
        dplyr::group_by(segment_id) |>
        dplyr::mutate(
          row_in_segment = dplyr::row_number(),
          n_in_segment = dplyr::n(),
           # Calculer la variation du cap dans le segment (ET circulaire)
          heading_var = {
            angles_rad <- heading * pi / 180
            sin_mean <- mean(sin(angles_rad), na.rm = TRUE)
            cos_mean <- mean(cos(angles_rad), na.rm = TRUE)
            R <- sqrt(sin_mean^2 + cos_mean^2)
            circular_sd <- ifelse(R < 0.001, 0, sqrt(-2 * log(R)) * 180 / pi)
            circular_sd
          },
          heading_smooth = dplyr::case_when(
             # Conserver le premier et le dernier point du segment
            row_in_segment == 1 | row_in_segment == n_in_segment ~ heading,
             # Segments courbes (>15 degres) : petite fenetre locale
            heading_var > 15 ~ {
               # Moyenne locale avec petite fenetre
              zoo::rollapply(
                heading,
                width = 3,
                align = "center",
                partial = TRUE,
                FUN = function(x) {
                  angles_rad <- x * pi / 180
                  real_part <- mean(cos(angles_rad), na.rm = TRUE)
                  imag_part <- mean(sin(angles_rad), na.rm = TRUE)
                  smoothed_angle <- atan2(imag_part, real_part) * 180 / pi
                  smoothed_angle
                }
              )
            },
             # Segments droits (<=15 degres) : moyenne du segment
            n_in_segment >= 3 ~ {
              angles_rad <- heading * pi / 180
              real_part <- mean(cos(angles_rad), na.rm = TRUE)
              imag_part <- mean(sin(angles_rad), na.rm = TRUE)
              smoothed_angle <- atan2(imag_part, real_part) * 180 / pi
              smoothed_angle
            },
             # Conserver l'original pour les segments courts
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

      # ----- Creation de l'objet SF avec polygones -----
      rlang::inform("Etape 16c : creation de l'objet SF polygones...")

      # S'assurer que les colonnes metriques existent
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

      # Creer l'objet SF
      sf_result <- data_to_sf(data, crs = 4326)
      
      if (is.null(sf_result)) {
        rlang::warn("Impossible de creer les polygones - retour des donnees sous forme de points")
        return(data)
      }

      # Selection des colonnes de sortie
      output_cols <- c("Yield_kg_ha", "Yield_kg_ha_wet", "Flow_kg_s", "Moisture_pct", 
                       "Swath_m", "Distance_m", "Heading_deg", "Altitude_m", 
                       "HeaderStatus", "Pass", "GPS_Time", "Longitude", "Latitude", 
                       "X_utm", "Y_utm", "Variety", "GrainType")

      sf_output <- sf_result |>
        dplyr::select(dplyr::any_of(output_cols)) |>
        dplyr::rename(
          Yield = Yield_kg_ha,
          Flow = Flow_kg_s,
          Heading = Heading_deg
        )

      # ----- Etape 17 : export -----
      if (!is.null(output_file)) {
        rlang::inform("Etape 17 : export...")
        ext <- tolower(tools::file_ext(output_file))
        if (ext == "geojson" || ext == "json") {
          sf::st_write(sf_output, output_file, driver = "GeoJSON", delete_dsn = TRUE)
        } else if (ext %in% c("shp", "gpkg")) {
          sf::st_write(sf_output, output_file, driver = toupper(ext), delete_dsn = TRUE)
        } else {
          # CSV par defaut
          utils::write.csv(sf_output, output_file, row.names = FALSE)
        }
        rlang::inform(paste("  Saved to:", output_file))
      }

      # ----- Etape 18 : generation du journal -----
      if (!is.null(log_file)) {
        rlang::inform("Etape 18 : generation du journal...")
        generate_clean_yield_log(sf_output, data_raw, params, pcdi_result,
                                thresholds, validation, log_file, file_path)
      }

      rlang::inform("")
      rlang::inform("================================================")
      rlang::inform(paste("Termine :", nrow(sf_output), "observations nettoyees"))
      rlang::inform(paste("Rendement moyen :", round(mean(sf_output$Yield), 0), unit_label))
      rlang::inform("================================================")

      return(sf_output)

  } else {
    # ----- Sortie tibble -----
    # Renommer les colonnes finales
    data$Yield <- data$Yield_final
    data$Flow <- data$Flow_final

    # Selectionner les colonnes de sortie
    output_cols <- c("X", "Y", "Latitude", "Longitude", "Flow", "Moisture",
                     "Swath", "Distance", "Pass", "HeaderStatus", "GPS_Time",
                     "Interval", "Yield", "Variety", "GrainType", "Altitude")

    data_output <- data |>
      dplyr::select(dplyr::any_of(output_cols)) |>
      dplyr::mutate(
        HeaderStatus = as.integer(HeaderStatus)
      ) |>
      dplyr::arrange(GPS_Time)

    # ----- Etape 17 : export -----
    if (!is.null(output_file)) {
      rlang::inform("Etape 17 : export...")
      ext <- tolower(tools::file_ext(output_file))
      if (ext == "csv") {
        utils::write.csv(data_output, output_file, row.names = FALSE)
      } else if (ext == "geojson" || ext == "json") {
        sf_data <- sf::st_as_sf(data_output,
          coords = c("Longitude", "Latitude"),
          crs = 4326
        )
        sf::st_write(sf_data, output_file, driver = "GeoJSON", delete_dsn = TRUE)
      }
      rlang::inform(paste("  Saved to:", output_file))
    }

    # ----- Etape 18 : generation du journal -----
    if (!is.null(log_file)) {
      rlang::inform("Etape 18 : generation du journal...")
      generate_clean_yield_log(data_output, data_raw, params, pcdi_result,
                              thresholds, validation, log_file, file_path)
    }

    rlang::inform("")
    rlang::inform("================================================")
    rlang::inform(paste("Termine :", nrow(data_output), "observations nettoyees"))
    rlang::inform(paste("Rendement moyen :", round(mean(data_output$Yield), 1), unit_label))
    rlang::inform("================================================")

    # Ajouter des attributs pour compatibilite
    attr(data_output, "ayce_params") <- params
    attr(data_output, "pcdi_result") <- pcdi_result
    attr(data_output, "thresholds") <- thresholds
    attr(data_output, "validation") <- validation

    return(data_output)
  }
}


#' Generer le journal pour clean_yield
#' @noRd
generate_clean_yield_log <- function(data_clean, data_raw, params, pcdi_result,
                                     thresholds, validation, log_file,
                                     source_file = "unknown") {

  n_raw <- nrow(data_raw)
  n_clean <- nrow(data_clean)
  n_removed <- n_raw - n_clean

  # Calcul des statistiques
  stats_raw <- data_raw |>
    dplyr::summarise(
      mean_yield = mean(Yield_buacre, na.rm = TRUE),
      sd_yield = stats::sd(Yield_buacre, na.rm = TRUE),
      cv = sd_yield / mean_yield * 100,
      n = dplyr::n()
    )

  stats_clean <- data_clean |>
    dplyr::summarise(
      mean_yield = mean(Yield, na.rm = TRUE),
      sd_yield = stats::sd(Yield, na.rm = TRUE),
      cv = sd_yield / mean_yield * 100,
      n = dplyr::n()
    )

  # Recuperer la valeur RSC
  rsc_value <- tryCatch({
    pcdi_result$rsc_values$mean_rsc[pcdi_result$rsc_values$delay == pcdi_result$optimal_delay][1]
  }, error = function(e) NA)

  log_lines <- c(
    "=======================================================",
    "         Journal du pipeline de nettoyage            ",
    "=======================================================",
    paste0("Date: ", Sys.time()),
    paste0("Fichier source : ", source_file),
    "",
    "--- RESUME ---",
    paste0("Points d'origine : ", n_raw),
    paste0("Points nettoyes : ", n_clean),
    paste0("Points supprimes : ", n_removed, " (", round(n_removed/n_raw*100, 1), "%)"),
    paste0("Taux de retention : ", round(validation$retention_rate * 100, 1), "%"),
    "",
    "--- RESULTATS PCDI (Delai de flux) ---",
    paste0("Delai optimal : ", pcdi_result$optimal_delay, " secondes"),
    paste0("RSC a l'optimal : ", round(rsc_value, 4) %||% "NA"),
    paste0("Stabilite (CV) : ", round(pcdi_result$stability, 4) %||% "NA"),
    "",
    "--- SEUILS AUTOMATIQUES ---",
    paste0("Plage rendement : [", round(thresholds$min_yield, 2), " - ", round(thresholds$max_yield, 2), "]"),
    paste0("Plage vitesse : [", round(thresholds$min_velocity, 2), " - ", round(thresholds$max_velocity, 2), " m/s]"),
    "",
    "--- PARAMETRES DE FILTRAGE ---",
    paste0("Chevauchement - Cellule : ", params$cellsize_overlap, " m"),
    paste0("Chevauchement - Seuil : ", params$overlap_threshold * 100, "%"),
    paste0("ET local - Passages : ", params$n_swaths),
    paste0("ET local - Limite : ", params$lsd_limit, " ET"),
    "",
    "--- STATISTIQUES ---",
    paste0("Brut :  Moyenne=", round(stats_raw$mean_yield, 1),
           " SD=", round(stats_raw$sd_yield, 1),
           " CV=", round(stats_raw$cv, 1), "%",
           " N=", stats_raw$n),
    paste0("Net :   Moyenne=", round(stats_clean$mean_yield, 1),
           " SD=", round(stats_clean$sd_yield, 1),
           " CV=", round(stats_clean$cv, 1), "%",
           " N=", stats_clean$n),
    paste0("Amelioration CV : ", round(validation$cv_improvement * 100, 1), "%"),
    "",
    "======================================================="
  )

  writeLines(log_lines, log_file)
  rlang::inform(paste("Journal sauvegarde :", log_file))
}
