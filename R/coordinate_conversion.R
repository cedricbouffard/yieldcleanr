#' Convertir Latitude/Longitude en coordonnees UTM
#'
#' Cette fonction convertit les coordonnées géographiques (WGS84)
#' en coordonnées UTM (Eastings/Northings).
#'
#' @param data Tibble avec colonnes Latitude et Longitude
#' @param zone Zone UTM (auto-detectee si NULL)
#' @param datum Datum a utiliser (defaut "WGS84")
 #' @return Tibble avec colonnes X (Easting) et Y (Northing)
 #' @export
 #' @examples
 #' # Creer des donnees d'exemple
 #' data <- tibble::tibble(
 #'   Latitude = c(47.506122, 47.506136, 47.506152),
 #'   Longitude = c(-69.856661, -69.856681, -69.856701),
 #'   Flow = c(1.53, 3.7, 7.56)
 #' )
 #'
 #' # Convertir en UTM
 #' data_utm <- latlon_to_utm(data)
 #' print(data_utm)
 latlon_to_utm <- function(data, zone = NULL, datum = "WGS84") {
  if (!all(c("Latitude", "Longitude") %in% names(data))) {
    rlang::abort("Les colonnes Latitude et Longitude sont requises")
  }

  # Detection automatique de la zone UTM
  if (is.null(zone)) {
    zone <- floor((data$Longitude[1] + 180) / 6) + 1
    rlang::inform(paste("Zone UTM détectée:", zone))
  }

  # Utiliser la conversion directe avec sf
  # EPSG:4326 = WGS84, EPSG:32619 = UTM zone 19N
  utm_epsg <- 32600 + zone  # Zone 19N = 32619

  result <- tryCatch({
    # Creer un objet sf avec WGS84
    sf_data <- sf::st_as_sf(
      data,
      coords = c("Longitude", "Latitude"),
      crs = 4326,
      remove = FALSE
    )

    # Transformer vers UTM
    utm_crs <- sf::st_crs(paste0("EPSG:", utm_epsg))
    sf_utm <- sf::st_transform(sf_data, utm_crs)

    # Extraire les coordonnees
    coords <- sf::st_coordinates(sf_utm)

    # Ajouter les colonnes
    data$X <- coords[, 1]
    data$Y <- coords[, 2]

    data
  }, error = function(e) {
    rlang::warn(paste("Erreur conversion sf, utilisation formule mathématique:", e$message))

    # Formule de conversion UTM approximative
    # Source : Army Corps of Engineers - Formules Transverse Mercator
    data <- data |>
      dplyr::mutate(
        # Parametres UTM
        zone_cm = (zone - 1) * 6 - 180 + 3,  # Longitude du centre de zone
        k0 = 0.9996,  # Facteur d'echelle

        # Conversion
        lat_rad = Latitude * pi / 180,
        lon_rad = Longitude * pi / 180,
        lon_origin_rad = zone_cm * pi / 180,

        # Parametres de l'ellipsoide WGS84
        a = 6378137,
        e = 0.081819191,
        e_prime_sq = (e^2) / (1 - e^2),

        # Calculs intermediaires
        n = a / sqrt(1 - e^2 * sin(lat_rad)^2),
        t = tan(lat_rad)^2,
        c = e_prime_sq * cos(lat_rad)^2,
        A = cos(lat_rad) * (lon_rad - lon_origin_rad),

        # Coordonnees UTM (Easting)
        X = k0 * n * (A + (1 - t + c) * A^3 / 6) + 500000,

        # Coordonnees UTM (Northing) - formule complete
        M0 = 1 - e^2/4 - 3*e^4/64 - 5*e^6/256,
        M1 = 3*e^2/8 + 3*e^4/32 + 45*e^6/1024,
        M2 = 15*e^4/256 + 45*e^6/1024,
        M3 = 35*e^6/3072,
        M = a * (M0*lat_rad - M1*sin(2*lat_rad) + M2*sin(4*lat_rad) - M3*sin(6*lat_rad)),
        Y = k0 * (M + n * tan(lat_rad) * (A^2/2 + (5-t+9*c+4*c^2)*A^4/24))
      ) |>
      # Hemisphere nord : pas de false northing
      dplyr::mutate(Y = ifelse(Y < 0, Y + 10000000, Y)) |>
      dplyr::select(-zone_cm, -k0, -lat_rad, -lon_rad, -lon_origin_rad,
                    -a, -e, -e_prime_sq, -n, -t, -c, -A,
                    -M0, -M1, -M2, -M3, -M)

    data
  })

  return(result)
}


#' Convertir UTM en Latitude/Longitude
#'
#' Cette fonction convertit les coordonnées UTM en Latitude/Longitude (WGS84).
#'
#' @param data Tibble avec colonnes X et Y
#' @param zone Numero de zone UTM
#' @param hemisphere "N" pour nord, "S" pour sud
#' @return Tibble avec colonnes Latitude et Longitude ajoutees
#' @noRd
#' @examples
#' # Creer des donnees d'exemple en UTM
#' data <- tibble::tibble(
#'   X = c(435000, 435050, 435100),
#'   Y = c(5262000, 5262050, 5262100),
#'   Flow = c(1.53, 3.7, 7.56)
#' )
#'
#' # Convertir en lat/lon
#' data_ll <- utm_to_latlon(data, zone = 19, hemisphere = "N")
#' print(data_ll)
utm_to_latlon <- function(data, zone, hemisphere = "N") {
  if (!all(c("X", "Y") %in% names(data))) {
    rlang::abort("Les colonnes X et Y sont requises")
  }

  # Creer des points sf avec le bon code EPSG
  # EPSG:32601-32660 = UTM hemisphere nord zones 1-60
  # EPSG:32701-32760 = UTM hemisphere sud zones 1-60
  utm_epsg <- ifelse(hemisphere == "N",
                    32600 + zone,   # Zone 19N = 32619
                    32700 + zone)   # Zone 19S = 32719

  data_sf <- data |>
    sf::st_as_sf(
      coords = c("X", "Y"),
      crs = paste0("EPSG:", utm_epsg),
      remove = FALSE
    ) |>
    sf::st_transform(crs = 4326)  # WGS84

  coords <- sf::st_coordinates(data_sf)

  data <- data |>
    dplyr::mutate(
      Longitude = coords[, 1],
      Latitude = coords[, 2]
    )

  return(data)
}


#' Calculer la distance entre points consecutifs
#'
#' Cette fonction calcule la distance euclidienne entre points consécutifs.
#'
#' @param data Tibble avec colonnes X, Y
#' @return Tibble avec colonne distance
#' @noRd
calculate_distances <- function(data) {
  if (!all(c("X", "Y") %in% names(data))) {
    rlang::abort("Les colonnes X et Y sont requises")
  }

  data <- data |>
    dplyr::arrange(.row_id) |>
    dplyr::mutate(
      dist_prev = sqrt((X - dplyr::lag(X))^2 + (Y - dplyr::lag(Y))^2)
    ) |>
    dplyr::mutate(distance_calc = dplyr::if_else(is.na(dist_prev), 0, dist_prev)) |>
    dplyr::select(-dist_prev)

  return(data)
}
