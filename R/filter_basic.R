#' Filter by header status
#'
#' Cette fonction filtre les données pour ne garder que les points où la
#' moissonneuse est en position de travail (header abaissé ou actif).
#' Header Status: 1 = harvesting (actif), 33 = header down (abaissé).
#' Les deux valeurs indiquent une récolte active.
#'
#' @param data A tibble with yield data
#' @param header_values Values indicating active harvesting (default c(1, 33))
#' @return Filtered tibble with header active data
#' @noRd
#' @examples
#' # Create sample data with mixed header status
#' data <- tibble::tibble(
#'   Flow = c(1.53, 3.7, 7.56, 10.36, 15.48),
#'   HeaderStatus = c(1, 33, 33, 0, 33)  # 1=harvesting, 33=header down, 0=header up
#' )
#'
#' # Filter to keep only active harvesting
#' data_filtered <- filter_header_status(data)
#' print(data_filtered)
filter_header_status <- function(data, header_values = c(1, 33)) {
  if (!"HeaderStatus" %in% names(data)) {
    rlang::warn("Colonne HeaderStatus non trouvée, saut du filtrage")
    return(data)
  }

  n_before <- nrow(data)

  data <- data |>
    dplyr::filter(HeaderStatus %in% header_values)

  n_removed <- n_before - nrow(data)

  # Log du filtrage
  if (n_removed > 0) {
    rlang::inform(paste(
      "Header Status filter:", n_removed, "points éliminés",
      "(header non actif, valeurs acceptées:", paste(header_values, collapse = ", "), ")"
    ))
  }

  return(data)
}


#' Filter by GPS status
#'
#' Cette fonction filtre les données selon la qualité du signal GPS.
#'
#' @param data A tibble with yield data
#' @param min_gps_status Minimum GPS status value (default 4 = good)
#' @return Filtered tibble with valid GPS data
#' @noRd
filter_gps_status <- function(data, min_gps_status = 4) {
  if (!"GPSStatus" %in% names(data)) {
    rlang::warn("Colonne GPSStatus non trouvée, saut du filtrage")
    return(data)
  }

  n_before <- nrow(data)

  # Conserver les lignes avec GPSStatus NA (données de qualité inconnue)
  data <- data |>
    dplyr::filter(is.na(GPSStatus) | GPSStatus >= min_gps_status)

  n_removed <- n_before - nrow(data)

  if (n_removed > 0) {
    rlang::inform(paste(
      "GPS Status filter:", n_removed, "points éliminés",
      "(GPS status <", min_gps_status, ")"
    ))
  }

  return(data)
}


#' Filter by DOP (Dilution of Precision)
#'
#' Cette fonction élimine les points avec un DOP trop élevé
#' (mauvaise précision GPS).
#'
#' @param data A tibble with yield data
#' @param max_dop Maximum acceptable DOP value (default 10)
#' @return Filtered tibble with valid DOP
#' @noRd
filter_dop <- function(data, max_dop = 10) {
  if (!"DOP" %in% names(data)) {
    rlang::warn("Colonne DOP non trouvée, saut du filtrage")
    return(data)
  }

  n_before <- nrow(data)

  # Conserver les lignes avec DOP NA
  data <- data |>
    dplyr::filter(is.na(DOP) | DOP <= max_dop)

  n_removed <- n_before - nrow(data)

  if (n_removed > 0) {
    rlang::inform(paste(
      "DOP filter:", n_removed, "points éliminés",
      "(DOP >", max_dop, ")"
    ))
  }

  return(data)
}


#' Filter by velocity range
#'
#' Cette fonction filtre les points selon la vitesse de déplacement.
#' La vélocité est calculée comme la distance euclidienne entre points
#' consécutifs divisée par l'intervalle de temps.
#'
#' @param data A tibble with yield data
#' @param min_velocity Minimum velocity in m/s (default 0.5)
#' @param max_velocity Maximum velocity in m/s (default 10)
#' @return Filtered tibble with valid velocities
#' @noRd
filter_velocity <- function(data, min_velocity = 0.5, max_velocity = 10) {
  if (!"X" %in% names(data) || !"Y" %in% names(data)) {
    rlang::warn("Coordonnées X,Y non trouvées, calcul de vélocité ignoré")
    return(data)
  }

  # Calcul de la vitesse basée sur la distance euclidienne
  data <- data |>
    dplyr::arrange(.row_id) |>
    dplyr::mutate(
      dist_eucl = sqrt((X - dplyr::lag(X))^2 + (Y - dplyr::lag(Y))^2),
      velocity = dist_eucl / Interval
    )

  n_before <- nrow(data)

  data <- data |>
    dplyr::filter(velocity >= min_velocity, velocity <= max_velocity)

  n_removed <- n_before - nrow(data)

  if (n_removed > 0) {
    rlang::inform(paste(
      "Velocity filter:", n_removed, "points éliminés",
      "(vitesse hors plage:", min_velocity, "-", max_velocity, ")"
    ))
  }

  # Suppression des colonnes temporaires
  data <- data |> dplyr::select(-dist_eucl, -velocity)

  return(data)
}


#' Filter by coordinate bounds
#'
#' Cette fonction filtre les points selon les limites géographiques
#' du champ (Easting/Northing ou Lat/Lon).
#'
#' @param data A tibble with yield data
#' @param bounds A list with min/max for x and y coordinates
#' @param coord_type Type of coordinates: "utm" or "latlon"
#' @return Filtered tibble within bounds
#' @noRd
filter_bounds <- function(data, bounds = NULL, coord_type = "latlon") {
  if (is.null(bounds)) {
    rlang::warn("Bounds non fournis, saut du filtrage")
    return(data)
  }

  n_before <- nrow(data)

  if (coord_type == "utm") {
    data <- data |>
      dplyr::filter(X >= bounds$min_x, X <= bounds$max_x,
                    Y >= bounds$min_y, Y <= bounds$max_y)
  } else {
    data <- data |>
      dplyr::filter(Longitude >= bounds$min_x, Longitude <= bounds$max_x,
                    Latitude >= bounds$min_y, Latitude <= bounds$max_y)
  }

  n_removed <- n_before - nrow(data)

  if (n_removed > 0) {
    rlang::inform(paste(
      "Bounds filter:", n_removed, "points éliminés",
      "(hors limites du champ)"
    ))
  }

  return(data)
}


#' Filter by yield range
#'
#' Cette fonction filtre les points selon la plage de rendement valide.
#' Peut utiliser des valeurs explicites ou l'auto-détection basée sur l'écart-type.
#'
#' @param data A tibble with yield data
#' @param min_yield Minimum acceptable yield. If NULL, auto-calculated as mean - n_std * sd
#' @param max_yield Maximum acceptable yield. If NULL, auto-calculated as mean + n_std * sd
#' @param yield_column Name of the yield column (default "Yield_buacre")
#' @param n_std Number of standard deviations for auto-detection (default 3)
#' @return Filtered tibble within yield range
#' @noRd
#' @examples
#' # Create sample data with yield values
#' data <- tibble::tibble(
#'   Yield_buacre = c(50, 100, 150, 300, 180),
#'   Flow = c(1.53, 3.7, 7.56, 10.36, 15.48)
#' )
#'
#' # Valeurs explicites
#' data_filtered <- filter_yield_range(data, min_yield = 50, max_yield = 200)
#'
#' # Auto-détection basée sur l'écart-type (mean ± 3*sd)
#' data_filtered <- filter_yield_range(data)
#'
#' # Auto-détection avec plage plus large (mean ± 4*sd)
#' data_filtered <- filter_yield_range(data, n_std = 4)
filter_yield_range <- function(data, min_yield = NULL, max_yield = NULL,
                                yield_column = "Yield_buacre", n_std = 3) {

  if (!yield_column %in% names(data)) {
    rlang::warn(paste("Colonne", yield_column, "non trouvée, saut du filtrage"))
    return(data)
  }

  # Auto-calcul si nécessaire
  if (is.null(min_yield) || is.null(max_yield)) {
    yield_vals <- data[[yield_column]]
    yield_vals <- yield_vals[is.finite(yield_vals)]

    mean_yield <- mean(yield_vals, na.rm = TRUE)
    sd_yield <- stats::sd(yield_vals, na.rm = TRUE)

    min_yield_calc <- mean_yield - n_std * sd_yield
    max_yield_calc <- mean_yield + n_std * sd_yield

    # Utiliser les valeurs calculées ou celles fournies
    min_yield <- if (is.null(min_yield)) min_yield_calc else min_yield
    max_yield <- if (is.null(max_yield)) max_yield_calc else max_yield

    rlang::inform(paste("Yield auto-range:", round(min_yield, 1), "-", round(max_yield, 1),
                       "(mean ±", n_std, "SD =", round(mean_yield, 1), "±", round(sd_yield, 1), ")"))
  }

  n_before <- nrow(data)

  # Filter out non-finite values first
  data <- data |>
    dplyr::filter(is.finite(.data[[yield_column]])) |>
    dplyr::filter(.data[[yield_column]] >= min_yield, .data[[yield_column]] <= max_yield)

  n_removed <- n_before - nrow(data)

  if (n_removed > 0) {
    rlang::inform(paste(
      "Yield range filter:", n_removed, "points éliminés",
      "(rendement hors plage:", round(min_yield, 1), "-", round(max_yield, 1), ")"
    ))
  }

  return(data)
}


#' Filter by moisture range
#'
#' Cette fonction filtre les points selon la plage d'humidité valide.
#' Peut utiliser des valeurs explicites ou l'auto-détection basée sur l'écart-type.
#'
#' @param data A tibble with yield data
#' @param min_moisture Minimum acceptable moisture. If NULL, auto-calculated as mean - n_std * sd
#' @param max_moisture Maximum acceptable moisture. If NULL, auto-calculated as mean + n_std * sd
#' @param n_std Number of standard deviations for auto-detection (default 3)
#' @return Filtered tibble within moisture range
#' @noRd
#' @examples
#' # Valeurs explicites
#' data_clean <- filter_moisture_range(data, min_moisture = 8, max_moisture = 15)
#'
#' # Auto-détection basée sur l'écart-type (mean ± 3*sd)
#' data_clean <- filter_moisture_range(data)
#'
#' # Auto-détection avec plage plus large (mean ± 4*sd)
#' data_clean <- filter_moisture_range(data, n_std = 4)
filter_moisture_range <- function(data, min_moisture = NULL, max_moisture = NULL, n_std = 3) {

  if (!"Moisture" %in% names(data)) {
    rlang::warn("Colonne Moisture non trouvée, saut du filtrage")
    return(data)
  }

  # Auto-calcul si nécessaire
  if (is.null(min_moisture) || is.null(max_moisture)) {
    mean_moisture <- mean(data$Moisture, na.rm = TRUE)
    sd_moisture <- stats::sd(data$Moisture, na.rm = TRUE)

    min_moisture_calc <- mean_moisture - n_std * sd_moisture
    max_moisture_calc <- mean_moisture + n_std * sd_moisture

    # Utiliser les valeurs calculées ou celles fournies
    min_moisture <- if (is.null(min_moisture)) min_moisture_calc else min_moisture
    max_moisture <- if (is.null(max_moisture)) max_moisture_calc else max_moisture

    rlang::inform(paste("Moisture auto-range:", round(min_moisture, 1), "-", round(max_moisture, 1),
                       "(mean ±", n_std, "SD =", round(mean_moisture, 1), "±", round(sd_moisture, 1), ")"))
  }

  n_before <- nrow(data)

  data <- data |>
    dplyr::filter(Moisture >= min_moisture, Moisture <= max_moisture)

  n_removed <- n_before - nrow(data)

  if (n_removed > 0) {
    rlang::inform(paste(
      "Moisture range filter:", n_removed, "points éliminés",
      "(humidité hors plage:", round(min_moisture, 1), "-", round(max_moisture, 1), ")"
    ))
  }

  return(data)
}
