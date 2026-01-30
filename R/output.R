#' Generer un journal de nettoyage
#'
#' Cette fonction génère un log détaillé des opérations de nettoyage.
#'
#' @param data_cleaned Tibble nettoye
#' @param data_raw Donnees brutes d'origine
#' @param params Liste des parametres utilises
#' @param log_file Chemin du journal de sortie
#' @return TRUE invisible si succes
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

  # Construction du journal
  log_lines <- c(
    "=======================================================",
    "              YIELDCLEANR - JOURNAL NETTOYAGE          ",
    "=======================================================",
    paste0("Date: ", Sys.time()),
    paste0("Points d'origine : ", n_raw),
    paste0("Points nettoyes : ", n_clean),
    paste0("Points supprimes : ", n_removed, " (", round(n_removed/n_raw*100, 1), "%)"),
    "",
    "--- PARAMETRES UTILISES ---",
    paste0("  Estimation delai flux : ", params$flow_delay %||% "NA"),
    paste0("  Estimation delai humidite : ", params$moisture_delay %||% "NA"),
    paste0("  Plage de rendement : ", params$yield_range[1], " - ", params$yield_range[2]),
    paste0("  Filtre chevauchement - Cellule : ", params$overlap_cellsize %||% "NA"),
    paste0("  Filtre chevauchement - Limite : ", params$overlap_limit %||% "NA"),
    paste0("  Filtre ET local - Passage : ", params$std_swath %||% "NA"),
    paste0("  Filtre ET local - Limite : ", params$std_limit %||% "NA"),
    paste0("  Filtre header : ", params$header_status %||% "NA"),
    paste0("  Filtre GPS : ", params$min_gps_status %||% "NA"),
    paste0("  DOP max : ", params$max_dop %||% "NA"),
    paste0("  Plage vitesse : ", params$min_velocity %||% "NA", " - ", params$max_velocity %||% "NA"),
    "",
    "--- STATISTIQUES ---",
    paste0("Brut :  Moyenne=", round(stats_raw$mean_yield, 2),
           " SD=", round(stats_raw$sd_yield, 2),
           " CV=", round(stats_raw$cv, 1),
           " N=", stats_raw$n,
           " Range=", round(stats_raw$min_yield, 0), "-", round(stats_raw$max_yield, 0)),
    paste0("Net :   Moyenne=", round(stats_clean$mean_yield, 2),
           " SD=", round(stats_clean$sd_yield, 2),
           " CV=", round(stats_clean$cv, 1),
           " N=", stats_clean$n,
           " Range=", round(stats_clean$min_yield, 0), "-", round(stats_clean$max_yield, 0)),
    "",
    "======================================================="
  )

  # Ecrire le fichier
  writeLines(log_lines, log_file)

  rlang::inform(paste("Journal sauvegarde :", log_file))

  return(invisible(TRUE))
}


#' Ajouter des drapeaux aux donnees
#'
#' Cette fonction ajoute des colonnes de drapeaux (flags) pour indiquer
#' les modifications apportées à chaque point de données.
#'
#' @param data Tibble de donnees nettoyees
#' @param cleaning_log Entrees de journal ou parametres utilises
#' @return Donnees avec colonnes de drapeaux
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

  # Si un journal est fourni, analyser les drapeaux
  if (!is.null(cleaning_log) && is.list(cleaning_log)) {
    # Appliquer les drapeaux selon les operations effectuees
    if (!is.null(cleaning_log$header_filtered)) {
      # HeaderStatus : 0 = header haut mais actif, 1 = harvesting, 33 = header bas (actif)
      # Drapeau si header inactif (ni 0 ni 1 ni 33)
      data <- data |>
        dplyr::mutate(flag_header = dplyr::if_else(!HeaderStatus %in% c(0, 1, 33), 1, 0))
    }
    # Ajouter d'autres drapeaux selon les besoins
  }

  return(data)
}


#' Exporter les donnees nettoyees
#'
#' Cette fonction exporte les données nettoyées vers un fichier CSV.
#'
#' @param data Tibble de donnees nettoyees
#' @param file_path Chemin du fichier de sortie
#' @param format Format de sortie : "csv" ou "shp"
#' @param utm_zone Zone UTM pour export shapefile
#' @return TRUE invisible si succes
#' @noRd
export_cleaned_data <- function(data, file_path, format = "csv", utm_zone = NULL) {
  if (format == "csv") {
    readr::write_csv(data, file_path)
    rlang::inform(paste("Donnees exportees vers :", file_path))
  } else if (format == "shp") {
    if (is.null(utm_zone)) {
      rlang::abort("Zone UTM requise pour l'export shapefile")
    }
    data_sf <- data |>
      sf::st_as_sf(
        coords = c("X", "Y"),
        crs = paste0("EPSG:", 326 + utm_zone),
        remove = FALSE
      )
    sf::st_write(data_sf, file_path, driver = "ESRI Shapefile", delete_layer = TRUE)
    rlang::inform(paste("Shapefile exporte vers :", file_path))
  }

  return(invisible(TRUE))
}
