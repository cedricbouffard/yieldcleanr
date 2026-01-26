#' Generate cleaning log
#'
#' Cette fonction génère un log détaillé des opérations de nettoyage.
#'
#' @param data_cleaned Cleaned data tibble
#' @param data_raw Original raw data
#' @param params List of cleaning parameters used
#' @param log_file Path to output log file
#' @return Invisible TRUE if successful
#' @noRd
generate_cleaning_log <- function(data_cleaned, data_raw, params = NULL,
                                   log_file = "cleaning_log.txt") {
  n_raw <- nrow(data_raw)
  n_clean <- nrow(data_cleaned)
  n_removed <- n_raw - n_clean

  # Calcul des statistiques
  stats_raw <- data_raw |>
    dplyr::summarise(
      mean_yield = mean(Flow, na.rm = TRUE),
      sd_yield = stats::sd(Flow, na.rm = TRUE),
      cv = (sd_yield / mean_yield) * 100,
      n = dplyr::n(),
      min_yield = min(Flow, na.rm = TRUE),
      max_yield = max(Flow, na.rm = TRUE)
    )

  stats_clean <- data_cleaned |>
    dplyr::summarise(
      mean_yield = mean(Flow, na.rm = TRUE),
      sd_yield = stats::sd(Flow, na.rm = TRUE),
      cv = (sd_yield / mean_yield) * 100,
      n = dplyr::n(),
      min_yield = min(Flow, na.rm = TRUE),
      max_yield = max(Flow, na.rm = TRUE)
    )

  # Construction du log
  log_lines <- c(
    "=======================================================",
    "              YIELDCLEANR - CLEANING LOG               ",
    "=======================================================",
    paste0("Date: ", Sys.time()),
    paste0("Original points: ", n_raw),
    paste0("Cleaned points: ", n_clean),
    paste0("Points removed: ", n_removed, " (", round(n_removed/n_raw*100, 1), "%)"),
    "",
    "--- PARAMETERS USED ---",
    paste0("  Flow Delay Estimate: ", params$flow_delay %||% "NA"),
    paste0("  Moisture Delay Estimate: ", params$moisture_delay %||% "NA"),
    paste0("  Yield Range: ", params$yield_range[1], " - ", params$yield_range[2]),
    paste0("  Overlap Filter - Cellsize: ", params$overlap_cellsize %||% "NA"),
    paste0("  Overlap Filter - Limit: ", params$overlap_limit %||% "NA"),
    paste0("  Local STD Filter - Swath: ", params$std_swath %||% "NA"),
    paste0("  Local STD Filter - Limit: ", params$std_limit %||% "NA"),
    paste0("  Header Status Filter: ", params$header_status %||% "NA"),
    paste0("  GPS Status Filter: ", params$min_gps_status %||% "NA"),
    paste0("  Max DOP: ", params$max_dop %||% "NA"),
    paste0("  Velocity Range: ", params$min_velocity %||% "NA", " - ", params$max_velocity %||% "NA"),
    "",
    "--- STATISTICS ---",
    paste0("Raw:    Mean=", round(stats_raw$mean_yield, 2),
           " SD=", round(stats_raw$sd_yield, 2),
           " CV=", round(stats_raw$cv, 1),
           " N=", stats_raw$n,
           " Range=", round(stats_raw$min_yield, 0), "-", round(stats_raw$max_yield, 0)),
    paste0("Clean:  Mean=", round(stats_clean$mean_yield, 2),
           " SD=", round(stats_clean$sd_yield, 2),
           " CV=", round(stats_clean$cv, 1),
           " N=", stats_clean$n,
           " Range=", round(stats_clean$min_yield, 0), "-", round(stats_clean$max_yield, 0)),
    "",
    "======================================================="
  )

  # Écrire le fichier
  writeLines(log_lines, log_file)

  rlang::inform(paste("Log file saved:", log_file))

  return(invisible(TRUE))
}


#' Add flags to data
#'
#' Cette fonction ajoute des colonnes de drapeaux (flags) pour indiquer
#' les modifications apportées à chaque point de données.
#'
#' @param data The cleaned data tibble
#' @param cleaning_log Log entries or parameters used
#' @return Data with flag columns added
#' @noRd
add_flags <- function(data, cleaning_log = NULL) {
  # Initialiser les drapeaux
  data <- data |>
    dplyr::mutate(
      flag_header = 0,
      flag_velocity = 0,
      flag_yield_range = 0,
      flag_moisture = 0,
      flag_gps = 0,
      flag_delay = 0,
      flag_overlap = 0,
      flag_std = 0
    )

  # Si un log de nettoyage est fourni, analyser les flags
  if (!is.null(cleaning_log) && is.list(cleaning_log)) {
    # Appliquer les drapeaux basés sur les opérations effectuées
    if (!is.null(cleaning_log$header_filtered)) {
      # HeaderStatus: 1 = harvesting (active), 33 = header down (active)
      # Flag si header n'est pas actif (ni 1 ni 33)
      data <- data |>
        dplyr::mutate(flag_header = dplyr::if_else(!HeaderStatus %in% c(1, 33), 1, 0))
    }
    # Ajouter d'autres drapeaux selon les besoins
  }

  return(data)
}


#' Export cleaned data
#'
#' Cette fonction exporte les données nettoyées vers un fichier CSV.
#'
#' @param data Cleaned data tibble
#' @param file_path Path to output file
#' @param format Output format: "csv" or "shp"
#' @param utm_zone UTM zone for shapefile export
#' @return Invisible TRUE if successful
#' @noRd
export_cleaned_data <- function(data, file_path, format = "csv", utm_zone = NULL) {
  if (format == "csv") {
    readr::write_csv(data, file_path)
    rlang::inform(paste("Data exported to:", file_path))
  } else if (format == "shp") {
    if (is.null(utm_zone)) {
      rlang::abort("UTM zone required for shapefile export")
    }
    data_sf <- data |>
      sf::st_as_sf(
        coords = c("X", "Y"),
        crs = paste0("EPSG:", 326 + utm_zone),
        remove = FALSE
      )
    sf::st_write(data_sf, file_path, driver = "ESRI Shapefile", delete_layer = TRUE)
    rlang::inform(paste("Shapefile exported to:", file_path))
  }

  return(invisible(TRUE))
}
