#' Convertir le flux de grain en rendement (boisseaux/acre)
#'
#' Convertit le flux brut (LBS/SEC) en rendement (boisseaux/acre)
#' avec la formule :
#' Rendement (bu/acre) = (Flow × Interval × 43560) / (lbs_per_bushel × Swath_ft × Distance_ft)
#'
#' Ou :
#'   - Flow = flux de grain en lbs/sec
#'   - Interval = intervalle de temps en secondes
#'   - Swath_ft = largeur de coupe en pieds (Swath_in / 12)
#'   - Distance_ft = distance parcourue en pieds (Distance_in / 12)
#'   - 43560 = pieds^2 par acre
#'   - lbs_per_bushel = lbs par boisseau (selon la culture)
#'
#' Facteurs de conversion standard :
#'   - Mais : 56 lbs/bu a 15.5% humidite
#'   - Soja : 60 lbs/bu a 13% humidite
#'   - Ble/cereales : 60 lbs/bu
#'
#' @param data Tibble avec Flow, Interval, Swath, Distance
#' @param lbs_per_bushel LBS par boisseau. Si NULL, auto-detection via GrainType.
#'   Defaut 56 pour le mais. Utiliser 60 pour soja et cereales.
#' @param sqft_per_acre Pieds carres par acre (defaut 43560)
#' @param inches_per_foot Pouces par pied (defaut 12)
#' @return Donnees avec colonne Yield_buacre
#' @noRd
#' @examples
#' \dontrun{
#' # Auto-detection selon la culture
#' data <- convert_flow_to_yield(data)
#'
#' # Explicite pour le mais
#' data <- convert_flow_to_yield(data, lbs_per_bushel = 56)
#'
#' # Pour soja/cereales
#' data <- convert_flow_to_yield(data, lbs_per_bushel = 60)
#' }
convert_flow_to_yield <- function(data, lbs_per_bushel = NULL,
                                   sqft_per_acre = 43560,
                                   inches_per_foot = 12) {

  if (!all(c("Flow", "Interval", "Swath", "Distance") %in% names(data))) {
    rlang::warn("Colonnes Flow, Interval, Swath, Distance requises")
    return(data)
  }

  # Auto-detecter lbs_per_bushel via GrainType si absent
  if (is.null(lbs_per_bushel)) {
    lbs_per_bushel <- get_lbs_per_bushel(data)
  }

  # Calculer Width_ft et Distance_ft
  # Swath_ft = Swath / 12 (pouces -> pieds)
  # Distance_ft = Distance / 12 (pouces -> pieds)
  # Distance est la distance parcourue durant l'intervalle

  # Retirer les lignes avec zeros (division par zero ou Inf)
  data <- data |>
    dplyr::filter(.data$Distance > 0, .data$Swath > 0, .data$Flow > 0)

  data <- data |>
    dplyr::mutate(
      Width_ft = .data$Swath / inches_per_foot,
      Distance_ft = .data$Distance / inches_per_foot,
      # Rendement = (Flow × Interval × 43560) / (lbs_per_bushel × Width_ft × Distance_ft)
      Yield_buacre = (.data$Flow * .data$Interval * sqft_per_acre) /
                     (lbs_per_bushel * .data$Width_ft * .data$Distance_ft)
    )

  # Supprimer les colonnes temporaires
  data <- data |>
    dplyr::select(-Width_ft, -Distance_ft)

  # Calculer la moyenne en excluant les Inf
  finite_yield <- data$Yield_buacre[is.finite(data$Yield_buacre)]
  mean_yield <- if (length(finite_yield) > 0) mean(finite_yield, na.rm = TRUE) else NA

  rlang::inform(paste("Yield calculé:", round(mean_yield, 1),
                      "bu/acre (lbs/bu =", lbs_per_bushel, ")"))

  return(data)
}


#' Obtenir les lbs par boisseau selon la culture
#'
#' Retourne le facteur standard selon la culture.
#'
#' @param data Tibble avec GrainType ou Grain_Type
#' @return LBS par boisseau (56 pour mais, 60 pour soja/cereales)
#' @noRd
get_lbs_per_bushel <- function(data) {
  # Verifier la colonne GrainType ou Grain_Type
  grain_col <- if ("GrainType" %in% names(data)) {
    "GrainType"
  } else if ("Grain_Type" %in% names(data)) {
    "Grain_Type"
  } else {
    NULL
  }

  if (!is.null(grain_col)) {
    grain <- tolower(unique(data[[grain_col]]))

    # Detecter le soja
    if (any(grepl("soja|soy", grain))) {
      return(60)
    }

    # Detecter ble/cereales
    if (any(grepl("blé|wheat|wheat|cereal|avoine|barley|orge", grain))) {
      return(60)
    }

    # Defaut : mais (56 lbs/bu)
    rlang::inform(paste("GrainType non reconnu, utilisation 56 lbs/boisseau (maïs par défaut)"))
    return(56)
  }

  # Defaut si pas de GrainType
  rlang::inform("Pas de colonne GrainType, utilisation 56 lbs/boisseau (maïs par défaut)")
  return(56)
}


#' Alternative : conversion avec formule simplifiee
#'
#' Rendement = Flow × 9335 / (Swath × Vitesse)
#' Formule simplifiee quand on utilise la vitesse directement.
#'
#' @param data Tibble avec Flow, Swath, Velocity_ft_s
#' @return Donnees avec colonne Yield_buacre
#' @noRd
convert_flow_simple <- function(data) {
  if (!all(c("Flow", "Swath", "Distance") %in% names(data))) {
    rlang::warn("Colonnes Flow, Swath, Distance requises")
    return(data)
  }

  # Utiliser Distance (pouces) et Interval (secondes) pour la vitesse
  data <- data |>
    dplyr::mutate(
      Velocity_ft_s = .data$Distance / .data$Interval / 12,  # ft/s
      Yield_buacre = .data$Flow * 43560 / (56 * .data$Swath / 12 * .data$Velocity_ft_s)
    )

  return(data)
}


#' Executer AYCE avec conversion correcte du rendement
#'
#' Pipeline AYCE complet qui retourne le rendement en boisseaux/acre.
#'
#' @param file_path Chemin du fichier d'entree
#' @param output_file Chemin du CSV de sortie
#' @param log_file Chemin du journal
#' @param params Parametres AYCE
#' @return Donnees nettoyees avec rendement en boisseaux/acre
#' @noRd
#' @examples
#' \dontrun{
#' cleaned <- ayce_with_yield_conversion("data.txt", "output.csv", "log.txt")
#' }
ayce_with_yield_conversion <- function(file_path, output_file = NULL,
                                        log_file = NULL, params = NULL) {

  rlang::inform("================================================")
  rlang::inform("   AYCE with Yield Conversion                  ")
  rlang::inform("   (LBS/SEC → Bushels/acre)                   ")
  rlang::inform("================================================")

  # Parametres par defaut
  default_params <- list(
    delay_range = 0:20,
    n_iterations = 10,
    noise_level = 0.05,
    yscale = 1.5,
    v_scale = 1.5,
    minv_abs = 0.5,
    miny_abs = 0,
    cellsize_overlap = 0.3,       # Cellule 30cm (standard USDA)
    overlap_threshold = 0.5,      # 50% max chevauchement
    n_swaths = 5,
    lsd_limit = 3,
    lbs_per_bushel = 56,          # Mais
    n_std = 3                     # Nombre d'ET pour auto-detection
  )

  params <- modifyList(default_params, params %||% list())

  # Etape 1 : lecture des donnees
  rlang::inform("Step 1: Loading data...")
  data <- read_yield_data(file_path)
  data_raw <- data
  rlang::inform(paste("  -", nrow(data), "rows loaded"))
  rlang::inform(paste("  Flow range:", min(data$Flow), "-", max(data$Flow), "LBS/sec"))

  # Etape 2 : conversion UTM
  rlang::inform("Step 2: Converting to UTM...")
  data <- latlon_to_utm(data)

  # Etape 3 : calcul de la vitesse
  rlang::inform("Step 3: Calculating velocity...")
  data <- data |>
    dplyr::mutate(
      velocity = sqrt((X - dplyr::lag(X))^2 + (Y - dplyr::lag(Y))^2) / Interval
    )

  # Etape 4 : PCDI
  rlang::inform("Step 4: PCDI - Flow Delay Optimization...")
  pcdi_result <- apply_pcdi(data, params$delay_range, params$n_iterations, params$noise_level)
  rlang::inform(paste("  Optimal delay:", pcdi_result$optimal_delay, "seconds"))

  # Etape 5 : seuils automatiques
  rlang::inform("Step 5: Calculating auto thresholds...")
  thresholds <- calculate_auto_thresholds(
    data,
    yllim = 0.05, yulim = 0.95, yscale = params$yscale,
    vllim = 0.02, vulim = 0.98, vscale = params$v_scale,
    minv_abs = params$minv_abs, miny_abs = params$miny_abs,
    gbuffer = 100
  )
  rlang::inform(paste("  Yield range:", round(thresholds$min_yield, 1), "-", round(thresholds$max_yield, 1)))

  # Etape 6 : filtre vitesse
  rlang::inform("Step 6: Velocity filter...")
  data <- filter_velocity(data, thresholds$min_velocity, thresholds$max_velocity)
  rlang::inform(paste("  Rows:", nrow(data)))

  # Etape 7 : correction du delai de flux
  if (pcdi_result$optimal_delay > 0) {
    rlang::inform(paste("Step 7: Flow delay correction (", pcdi_result$optimal_delay, "s)..."))
    data <- apply_flow_delay(data, delay = pcdi_result$optimal_delay)
    rlang::inform(paste("  Rows:", nrow(data)))
  }

  # Etape 8 : filtre plage de rendement
  rlang::inform("Step 8: Yield range filter...")
  data <- filter_yield_range(data, thresholds$min_yield, thresholds$max_yield)
  rlang::inform(paste("  Rows:", nrow(data)))

  # Etape 9 : filtre humidite (auto-detection par ET)
  rlang::inform("Step 9: Moisture filter...")
  data <- filter_moisture_range(data, n_std = params$n_std)
  rlang::inform(paste("  Rows:", nrow(data)))

  # Etape 10 : filtre de chevauchement
  rlang::inform("Step 10: Overlap filter...")
  data <- apply_overlap_filter(data, params$cellsize_overlap, params$overlap_threshold)
  rlang::inform(paste("  Rows:", nrow(data)))

  # Etape 11 : filtre ecart-type local
  rlang::inform("Step 11: Local SD filter...")
  data <- apply_local_sd_filter(data, params$n_swaths, params$lsd_limit)
  rlang::inform(paste("  Rows:", nrow(data)))

  # Etape 12 : conversion en boisseaux/acre
  rlang::inform("Step 12: Converting to Bushels/acre...")
  data <- convert_flow_to_yield(data, lbs_per_bushel = params$lbs_per_bushel)

  rlang::inform(paste("  Flow range:", round(min(data$Flow), 1), "-", round(max(data$Flow), 1), "bu/acre"))
  rlang::inform(paste("  Flow mean:", round(mean(data$Flow), 1), "bu/acre"))

  # Etape 13 : formatage de la sortie
  rlang::inform("Step 13: Formatting output...")
  output_cols <- c("X", "Y", "Latitude", "Longitude", "Flow", "Moisture",
                   "Swath", "Pass", "GPS_Time", "HeaderStatus", "Altitude", "velocity")

  data_output <- data |>
    dplyr::select(dplyr::any_of(output_cols)) |>
    dplyr::mutate(
      HeaderStatus = as.integer(HeaderStatus),
      velocity = NULL  # Remove velocity column
    ) |>
    dplyr::arrange(GPS_Time)

  # Etape 14 : export
  if (!is.null(output_file)) {
    rlang::inform("Step 14: Exporting...")
    export_cleaned_data(data_output, output_file, format = "csv")
  }

  # Etape 15 : journal
  if (!is.null(log_file)) {
    rlang::inform("Step 15: Generating log...")
    generate_yield_log(data_output, data_raw, params, pcdi_result, thresholds, log_file)
  }

  rlang::inform("")
  rlang::inform("================================================")
  rlang::inform(paste("Complete:", nrow(data_output), "rows at",
                      round(mean(data_output$Flow), 1), "bu/acre"))
  rlang::inform("================================================")

  return(data_output)
}


#' Generer le journal pour la conversion de rendement
#' @noRd
generate_yield_log <- function(data_clean, data_raw, params, pcdi_result,
                               thresholds, log_file) {

  n_raw <- nrow(data_raw)
  n_clean <- nrow(data_clean)

  # Convertir le flux brut pour la comparaison
  data_raw_vel <- data_raw |>
    dplyr::mutate(
      velocity = sqrt((X - dplyr::lag(X))^2 + (Y - dplyr::lag(Y))^2) / Interval
    ) |>
    convert_flow_to_yield(lbs_per_bushel = params$lbs_per_bushel)

  stats_raw <- data.frame(
    mean = mean(data_raw_vel$Flow, na.rm = TRUE),
    sd = sd(data_raw_vel$Flow, na.rm = TRUE),
    cv = sd(data_raw_vel$Flow, na.rm = TRUE) / mean(data_raw_vel$Flow, na.rm = TRUE) * 100,
    n = n_raw
  )

  stats_clean <- data.frame(
    mean = mean(data_clean$Flow, na.rm = TRUE),
    sd = sd(data_clean$Flow, na.rm = TRUE),
    cv = sd(data_clean$Flow, na.rm = TRUE) / mean(data_clean$Flow, na.rm = TRUE) * 100,
    n = n_clean
  )

  log_lines <- c(
    "=======================================================",
    "    AYCE with Yield Conversion - Log File             ",
    "=======================================================",
    paste0("Date: ", Sys.time()),
    "",
    "--- SUMMARY ---",
    paste0("Original points: ", n_raw),
    paste0("Cleaned points: ", n_clean),
    paste0("Removed: ", n_raw - n_clean, " (", round((n_raw - n_clean)/n_raw*100, 1), "%)"),
    "",
    "--- CONVERSION ---",
    paste0("Formula: Flow_buacre = Flow_lbssec × 43560 / (56 × Swath_ft × Velocity)"),
    paste0("Conversion factor: 1 bushel = 56 lbs (corn)"),
    "",
    "--- PCDI ---",
    paste0("Optimal delay: ", pcdi_result$optimal_delay, " seconds"),
    paste0("RSC: ", round(pcdi_result$rsc_values$mean_rsc[pcdi_result$rsc_values$delay == pcdi_result$optimal_delay], 4)),
    "",
    "--- STATISTICS (Bushels/acre) ---",
    paste0("Raw:    Mean=", round(stats_raw$mean, 1),
           " SD=", round(stats_raw$sd, 1),
           " CV=", round(stats_raw$cv, 1), "% N=", stats_raw$n),
    paste0("Clean:  Mean=", round(stats_clean$mean, 1),
           " SD=", round(stats_clean$sd, 1),
           " CV=", round(stats_clean$cv, 1), "% N=", stats_clean$n),
    "",
    "======================================================="
  )

  writeLines(log_lines, log_file)
}
