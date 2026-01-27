#' Appliquer la correction de delai de flux
#'
#' Cette fonction compense le délai entre le moment où le grain passe
#' sous le capteur de flux et le moment où la position GPS est enregistrée.
#' Le flux de grain est décalé dans le temps pour correspondre à la position.
#'
#' @param data Tibble avec donnees de rendement
#' @param delay Nombre d'observations a decaler (positif = vers l'avant)
#' @param direction Direction du decalage : "forward" ou "backward"
#' @return Tibble avec valeurs de flux corrigees
#' @noRd
#' @examples
#' # Creer des donnees d'exemple
#' data <- tibble::tibble(
#'   Flow = c(1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5),
#'   Longitude = 1:7,
#'   Latitude = 1:7
#' )
#'
#' # Appliquer la correction de delai de flux
#' data_corrected <- apply_flow_delay(data, delay = 1)
#' print(data_corrected)
apply_flow_delay <- function(data, delay = 2, direction = "forward") {
  n_before <- nrow(data)

  if (delay == 0) {
    rlang::inform("Flow delay = 0, pas de correction appliquée")
    data <- data |> dplyr::mutate(Flow_raw = Flow, Flow = Flow)
    return(data)
  }

  # Sauvegarde de la valeur originale
  data <- data |> dplyr::mutate(Flow_raw = Flow)

  if (delay >= 0) {
    if (direction == "forward") {
      # Delai positif : flux mesure APRES la position
      # Deplacer le flux vers l'arriere pour aligner la position
      # Utiliser lead() pour decaler dans le jeu de donnees
      data <- data |>
        dplyr::mutate(
          Flow = dplyr::lead(Flow, n = delay, default = NA_real_)
        )
    } else {
      # Decaler le flux vers l'arriere (delai positif en backward)
      data <- data |>
        dplyr::mutate(
          Flow = dplyr::lag(Flow, n = delay, default = NA_real_)
        )
    }
  } else {
    # Delai negatif : flux mesure AVANT la position
    abs_delay <- abs(delay)
    if (direction == "forward") {
      # Delai negatif : decaler le flux vers l'avant
      data <- data |>
        dplyr::mutate(
          Flow = dplyr::lag(Flow, n = abs_delay, default = NA_real_)
        )
    } else {
      # Delai negatif en backward = decaler vers l'avant
      data <- data |>
        dplyr::mutate(
          Flow = dplyr::lag(Flow, n = abs_delay, default = NA_real_)
        )
    }
  }

  # Compter les NA crees
  n_na <- sum(is.na(data$Flow))
  data <- data |> dplyr::filter(!is.na(Flow))

  rlang::inform(paste(
    "Flow delay correction:", delay, "seconds,",
    n_na, "points éliminés (valeurs NA)"
  ))

  return(data)
}


#' Appliquer la correction de delai d'humidite
#'
#' Cette fonction compense le délai entre la mesure d'humidité et la position GPS.
#'
#' @param data Tibble avec donnees de rendement
#' @param delay Nombre d'observations a decaler
#' @param direction Direction du decalage
#' @return Tibble avec valeurs d'humidite corrigees
#' @noRd
apply_moisture_delay <- function(data, delay = 15, direction = "forward") {
  n_before <- nrow(data)

  if (delay == 0) {
    rlang::inform("Moisture delay = 0, pas de correction appliquée")
    data <- data |> dplyr::mutate(Moisture_raw = Moisture)
    return(data)
  }

  # Sauvegarde de la valeur originale
  data <- data |> dplyr::mutate(Moisture_raw = Moisture)

  if (direction == "forward") {
    data <- data |>
      dplyr::mutate(
        Moisture = dplyr::lag(Moisture, n = delay, default = NA_real_)
      )
  } else {
    data <- data |>
      dplyr::mutate(
        Moisture = dplyr::lead(Moisture, n = delay, default = NA_real_)
      )
  }

  n_na <- sum(is.na(data$Moisture))
  data <- data |> dplyr::filter(!is.na(Moisture))

  rlang::inform(paste(
    "Moisture delay correction:", delay, "observations décalées,",
    n_na, "points éliminés"
  ))

  return(data)
}


#' Calculer les plages automatiques pour le filtrage
#'
#' Cette fonction calcule automatiquement les plages de valeurs valides
#' basées sur les distributions de Yield, Coordinates et Velocity.
#'
#' @param data Tibble avec donnees de rendement
#' @param yield_quantiles Quantiles pour la plage de rendement (c(bas, haut))
#' @param coord_quantiles Quantiles pour la plage de coordonnees
#' @param velocity_quantiles Quantiles pour la plage de vitesse
#' @return Liste avec plages calculees
#' @noRd
#' @examples
#' # Creer des donnees d'exemple
#' data <- tibble::tibble(
#'   Flow = c(10, 50, 100, 150, 200, 250, 300, 350, 400, 450),
#'   X = c(435000, 435010, 435020, 435030, 435040,
#'         435050, 435060, 435070, 435080, 435090),
#'   Y = c(5262000, 5262010, 5262020, 5262030, 5262040,
#'         5262050, 5262060, 5262070, 5262080, 5262090),
#'   Distance = c(10, 14, 14, 14, 14, 14, 14, 14, 14, 14),
#'   Interval = c(2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L)
#' )
#'
#' # Calculer les plages automatiques
#' ranges <- calculate_auto_ranges(data)
#' print(ranges)
calculate_auto_ranges <- function(data,
                                   yield_quantiles = c(0.01, 0.99),
                                   coord_quantiles = c(0.001, 0.999),
                                   velocity_quantiles = c(0.01, 0.99)) {

  ranges <- list()

  # Plage de rendement
  if ("Flow" %in% names(data)) {
    ranges$yield <- c(
      min = quantile(data$Flow, yield_quantiles[1], na.rm = TRUE),
      max = quantile(data$Flow, yield_quantiles[2], na.rm = TRUE)
    )
    rlang::inform(paste(
      "Auto ranges complete: Yield :",
      round(ranges$yield[1], 0), "-", round(ranges$yield[2], 0)
    ))
  }

  # Plages de coordonnees (si UTM disponible)
  if ("X" %in% names(data) && "Y" %in% names(data)) {
    ranges$easting <- c(
      min = quantile(data$X, coord_quantiles[1], na.rm = TRUE),
      max = quantile(data$X, coord_quantiles[2], na.rm = TRUE)
    )
    ranges$northing <- c(
      min = quantile(data$Y, coord_quantiles[1], na.rm = TRUE),
      max = quantile(data$Y, coord_quantiles[2], na.rm = TRUE)
    )
    rlang::inform(paste(
      "Auto ranges complete: Easting:", round(ranges$easting[1]), "-", round(ranges$easting[2])
    ))
    rlang::inform(paste(
      "Auto ranges complete: Northing:", round(ranges$northing[1]), "-", round(ranges$northing[2])
    ))
  }

  # Plage de vitesse
  if ("Distance" %in% names(data) && "Interval" %in% names(data)) {
    data_temp <- data |>
      dplyr::mutate(velocity = Distance / Interval)
    ranges$velocity <- c(
      min = quantile(data_temp$velocity, velocity_quantiles[1], na.rm = TRUE),
      max = quantile(data_temp$velocity, velocity_quantiles[2], na.rm = TRUE)
    )
    rlang::inform(paste(
      "Auto ranges complete: Velocity:", round(ranges$velocity[1], 2), "-", round(ranges$velocity[2], 2)
    ))
  }

  return(ranges)
}
