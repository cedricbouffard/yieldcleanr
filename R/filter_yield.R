#' Appliquer la correction de delai de flux
#'
#' Cette fonction compense le delai entre le moment ou le grain passe
#' sous le capteur de flux et le moment ou la position GPS est enregistree.
#' Le flux de grain est decale dans le temps pour correspondre a la position.
#'
#' @param data Tibble avec donnees de rendement
#' @param delay Nombre d'observations a decaler (positif = vers l'avant)
#' @param direction Direction du decalage : "forward" ou "backward"
 #' @return Tibble avec valeurs de flux corrigees
 #' @noRd
 #' @keywords internal
   apply_flow_delay <- function(data, delay = 2, direction = "forward", value_col = "Flow") {
   n_before <- nrow(data)

   if (delay == 0) {
     rlang::inform(paste(value_col, "delay = 0, pas de correction appliquee"))
     return(data)
   }

   if (!value_col %in% names(data)) {
     rlang::warn(paste("Colonne", value_col, "non trouvee pour correction de delai"))
     return(data)
   }

   # Sauvegarde de la valeur originale
   raw_col <- paste0(value_col, "_raw")
   data[[raw_col]] <- data[[value_col]]

   if (delay >= 0) {
     # Delai positif : Flow mesuree APRES la position GPS
     # Flow doit shifter en AVANT pour aligner avec position ulterieure
     # lead() deplace valeurs vers indices superieurs = apparait plus tard
     data[[value_col]] <- dplyr::lead(data[[value_col]], n = delay, default = NA_real_)
   } else {
     # Delai negatif : Flow mesuree AVANT la position GPS
     # Flow doit shifter en ARRIERE pour aligner avec position anterieure
     # lag() deplace valeurs vers indices inferieurs = apparait plus tot
     abs_delay <- abs(delay)
     data[[value_col]] <- dplyr::lag(data[[value_col]], n = abs_delay, default = NA_real_)
   }

   # Compter les NA crees
   n_na <- sum(is.na(data[[value_col]]))
   # NE PLUS SUPPRIMER les points - conserver toutes les lignes
   # Les NA sont conserves (pour interpolation ulterieure si necessaire)
   
   rlang::inform(paste(
     value_col, "delay correction:", delay, "secondes,", n_na, "valeurs NA creees (points conserves)"
   ))

   return(data)
 }


#' Interpoler les valeurs NA apres decalage de delai
#'
#' Remplit les valeurs NA creees par le decalage avec une interpolation lineaire
#'
#' @param data Tibble avec donnees de rendement
#' @param value_col Colonne a interpoler
#' @return Donnees avec valeurs interpolees
#' @noRd
interpolate_na <- function(data, value_col = "Flow") {
  if (!value_col %in% names(data)) {
    rlang::warn(paste("Colonne", value_col, "non trouvee pour interpolation"))
    return(data)
  }

  n_na_before <- sum(is.na(data[[value_col]]))
  if (n_na_before == 0) {
    return(data)
  }

  # Trouver les indices non-NA
  valid_idx <- which(!is.na(data[[value_col]]))
  if (length(valid_idx) < 2) {
    rlang::warn("Pas assez de valeurs valides pour interpolation")
    return(data)
  }

  # Interpolation lineaire simple - utiliser approx
  # Creer un vecteur pour les resultats
  result <- rep(NA_real_, nrow(data))
  valid_vals <- data[[value_col]][valid_idx]

  # Interpolation seulement pour les NA entre des valeurs valides
  na_idx <- which(is.na(data[[value_col]]))

  for (i in na_idx) {
    # Trouver les valeurs valides avant et apres
    before <- valid_idx[valid_idx < i]
    after <- valid_idx[valid_idx > i]

    if (length(before) > 0 && length(after) > 0) {
      idx_before <- max(before)
      idx_after <- min(after)
      val_before <- data[[value_col]][idx_before]
      val_after <- data[[value_col]][idx_after]

      # Interpolation lineaire
      result[i] <- val_before + (val_after - val_before) * (i - idx_before) / (idx_after - idx_before)
    }
  }

  data[[value_col]] <- ifelse(is.na(data[[value_col]]), result, data[[value_col]])

  n_na_after <- sum(is.na(data[[value_col]]))
  rlang::inform(paste("  Interpolation:", n_na_before, "->", n_na_after, "NA pour", value_col))

  return(data)
}


#' Appliquer la correction de delai d'humidite
#'
#' Cette fonction compense le delai entre la mesure d'humidite et la position GPS.
#'
#' @param data Tibble avec donnees de rendement
#' @param delay Nombre d'observations a decaler
#' @param direction Direction du decalage
 #' @return Tibble avec valeurs d'humidite corrigees
 #' @export
 apply_moisture_delay <- function(data, delay = 15, direction = "forward") {
  n_before <- nrow(data)

  if (delay == 0) {
    rlang::inform("Moisture delay = 0, pas de correction appliquee")
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
    "Moisture delay correction:", delay, "observations decalees,",
    n_na, "points elimines"
  ))

  return(data)
}


#' Calculer les plages automatiques pour le filtrage
#'
#' Cette fonction calcule automatiquement les plages de valeurs valides
#' basees sur les distributions de Yield, Coordinates et Velocity.
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
