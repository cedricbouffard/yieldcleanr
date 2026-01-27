#' Convertir les unites de flux de rendement
#'
#' Convertit le flux brut (LBS/sec) en boisseaux/sec en supposant
#' une densite standard de 56 lbs/boisseau pour le mais.
#'
#' @param data Tibble avec colonne Flow
#' @param density LBS par boisseau (defaut 56 pour le mais)
#' @param scale_factor Facteur d'echelle optionnel (ex. 1000 pour affichage)
#' @return Donnees avec Flow converti
#' @noRd
#' @examples
#' \dontrun{
#' data <- convert_flow_units(data, density = 56, scale_factor = 100)
#' }
convert_flow_units <- function(data, density = 56, scale_factor = 1) {
  if (!"Flow" %in% names(data)) {
    rlang::warn("Colonne Flow non trouvée")
    return(data)
  }

  data <- data |>
    dplyr::mutate(
      Flow = Flow / density * scale_factor
    )

  rlang::inform(paste("Flow converted:", density, "lbs/bu ×", scale_factor, "scale"))

  return(data)
}


#' Appliquer la conversion de rendement pour l'affichage
#'
#' Convertit le flux brut en rendement (boisseaux/acre)
#' en tenant compte de la vitesse et de la largeur de coupe.
#'
#' @param data Tibble avec Flow, velocity (optionnel), Swath
#' @param density LBS par boisseau
#' @return Donnees avec Flow converti en boisseaux/acre
#' @noRd
convert_to_yield <- function(data, density = 56) {
  if (!"Flow" %in% names(data)) {
    rlang::warn("Colonne Flow non trouvée")
    return(data)
  }

  # Facteurs de conversion
  lbs_per_bu <- density  # 56 pour le mais
  sqft_per_acre <- 43560
  sqm_per_sqft <- 0.0929
  inch_per_m <- 39.37

  # Convertir la largeur de coupe en metres
  if ("Swath" %in% names(data)) {
    swath_m <- data$Swath / inch_per_m

    # Calculer la vitesse si absente
    if (!"velocity" %in% names(data) && all(c("X", "Y", "Interval") %in% names(data))) {
      data <- data |>
        dplyr::mutate(
          velocity = sqrt((X - dplyr::lag(X))^2 + (Y - dplyr::lag(Y))^2) / Interval
        )
    }

    # Convertir en boisseaux/acre
    # Flow (lbs/sec) / lbs_per_bu = boisseaux/sec
    # boisseaux/sec / (velocity * swath_m) = boisseaux/sec/m2
    # boisseaux/sec/m2 * sqm_per_sqft * sqft_per_acre = boisseaux/acre/sec
    # Final : Flow * sqm_per_sqft * sqft_per_acre / (lbs_per_bu * velocity * swath_m)

    if ("velocity" %in% names(data)) {
      data <- data |>
        dplyr::mutate(
          Flow_yield = Flow * sqm_per_sqft * sqft_per_acre /
                        (lbs_per_bu * velocity * swath_m)
        ) |>
        dplyr::select(-velocity)
    } else {
      # Conversion simplifiee
      data <- data |>
        dplyr::mutate(
          Flow_yield = Flow / lbs_per_bu * 100  # Simplified scaling
        )
    }
  } else {
    # Conversion simple
    data <- data |>
      dplyr::mutate(
        Flow_yield = Flow / lbs_per_bu
      )
  }

  return(data)
}
