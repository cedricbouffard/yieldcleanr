#' Filtrer selon le statut du header
#'
#' Cette fonction filtre les données pour ne garder que les points où la
#' moissonneuse est en position de travail (header abaissé ou actif).
#' Header Status: 1 = harvesting (actif), 33 = header down (abaissé).
#' Les deux valeurs indiquent une récolte active.
#'
#' @param data Tibble avec donnees de rendement
#' @param header_values Valeurs indiquant une recolte active (defaut c(1, 33))
#' @return Tibble filtre avec header actif
#' @noRd
#' @examples
#' # Creer des donnees d'exemple avec header mixte
#' data <- tibble::tibble(
#'   Flow = c(1.53, 3.7, 7.56, 10.36, 15.48),
#'   HeaderStatus = c(1, 33, 33, 0, 33)  # 1=actif, 33=header bas, 0=header haut
#' )
#'
#' # Filtrer pour ne garder que la recolte active
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


#' Filtrer selon le statut GPS
#'
#' Cette fonction filtre les données selon la qualité du signal GPS.
#'
#' @param data Tibble avec donnees de rendement
#' @param min_gps_status Statut GPS minimal (defaut 4 = bon)
#' @return Tibble filtre avec GPS valide
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


#' Filtrer selon le DOP (Dilution of Precision)
#'
#' Cette fonction élimine les points avec un DOP trop élevé
#' (mauvaise précision GPS).
#'
#' @param data Tibble avec donnees de rendement
#' @param max_dop Valeur maximale acceptable du DOP (defaut 10)
#' @return Tibble filtre avec DOP valide
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


#' Filtrer selon la plage de vitesse
#'
#' Cette fonction filtre les points selon la vitesse de déplacement.
#' La vélocité est calculée comme la distance euclidienne entre points
#' consécutifs divisée par l'intervalle de temps.
#'
#' @param data Tibble avec donnees de rendement
#' @param min_velocity Vitesse minimale en m/s (defaut 0.5)
#' @param max_velocity Vitesse maximale en m/s (defaut 10)
#' @return Tibble filtre avec vitesses valides
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


#' Filtrer selon les limites geographiques
#'
#' Cette fonction filtre les points selon les limites géographiques
#' du champ (Easting/Northing ou Lat/Lon).
#'
#' @param data Tibble avec donnees de rendement
#' @param bounds Liste avec min/max des coordonnees x et y
#' @param coord_type Type de coordonnees : "utm" ou "latlon"
#' @return Tibble filtre dans les limites
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


#' Filtrer selon la plage de rendement
#'
#' Cette fonction filtre les points selon la plage de rendement valide.
#' Peut utiliser des valeurs explicites ou l'auto-détection basée sur l'écart-type.
#'
#' @param data Tibble avec donnees de rendement
#' @param min_yield Rendement minimal acceptable. Si NULL, calcule automatiquement.
#' @param max_yield Rendement maximal acceptable. Si NULL, calcule automatiquement.
#' @param yield_column Nom de la colonne de rendement (defaut "Yield_buacre")
#' @param n_std Nombre d'ecarts-types pour auto-detection (defaut 3)
#' @return Tibble filtre dans la plage de rendement
#' @noRd
#' @examples
#' # Creer des donnees d'exemple avec rendements
#' data <- tibble::tibble(
#'   Yield_buacre = c(50, 100, 150, 300, 180),
#'   Flow = c(1.53, 3.7, 7.56, 10.36, 15.48)
#' )
#'
#' # Valeurs explicites
#' data_filtered <- filter_yield_range(data, min_yield = 50, max_yield = 200)
#'
#' # Auto-detection basee sur l'ecart-type (moyenne ± 3*ET)
#' data_filtered <- filter_yield_range(data)
#'
#' # Auto-detection avec plage plus large (moyenne ± 4*ET)
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

  # Filtrer d'abord les valeurs non finies
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


#' Filtrer selon la plage d'humidite
#'
#' Cette fonction filtre les points selon la plage d'humidité valide.
#' Peut utiliser des valeurs explicites ou l'auto-détection basée sur l'écart-type.
#'
#' @param data Tibble avec donnees de rendement
#' @param min_moisture Humidite minimale acceptable. Si NULL, calcule automatiquement.
#' @param max_moisture Humidite maximale acceptable. Si NULL, calcule automatiquement.
#' @param n_std Nombre d'ecarts-types pour auto-detection (defaut 3)
#' @return Tibble filtre dans la plage d'humidite
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
