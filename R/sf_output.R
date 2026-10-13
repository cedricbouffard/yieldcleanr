#' Pipeline AYCE complet avec sortie SF
#'
#' Cette fonction execute le pipeline AYCE complet et retourne un objet SF
#' avec des polygones rectangles orientes et toutes les mesures en metrique.
#' Enveloppe de clean_yield() avec sortie metrique + polygones.
#'
#' @param file_path Chemin du fichier d'entree
#' @param data Tibble ou data frame de donnees de rendement (alternative a file_path)
#' @param output_file Chemin optionnel pour GeoJSON
#' @param log_file Chemin optionnel pour le journal
#' @param geometry_type "polygon" ou "point" (compatibilite)
#' @param params Liste des parametres AYCE
 #' @return Objet SF avec donnees nettoyees
 #' @export
 #' @examples
 #' \dontrun{
 #' sf_result <- ayce_sf(
 #'   file_path = "data.txt",
 #'   geometry_type = "polygon"
 #' )
 #' plot(sf_result["Yield"])
 #' }
 ayce_sf <- function(file_path = NULL, data = NULL, output_file = NULL, log_file = NULL,
                    geometry_type = c("polygon", "point"),
                    params = NULL) {

   geometry_type <- match.arg(geometry_type)

   if (geometry_type == "point") {
     # Sortie points (tibble sans geometrie pour compatibilite)
     clean_yield(file_path = file_path,
                 data = data,
                 metrique = TRUE,
                 polygon = FALSE,
                 params = params,
                 output_file = output_file,
                 log_file = log_file)
   } else {
     # Sortie polygones (objet SF)
     clean_yield(file_path = file_path,
                 data = data,
                 metrique = TRUE,
                 polygon = TRUE,
                 params = params,
                 output_file = output_file,
                 log_file = log_file)
   }
 }


#' Convertir les donnees nettoyees en objet SF avec polygones
#'
#' Cette fonction convertit les données nettoyées en objet SF avec des polygones
#' représentant la zone récoltée (rectangles orientés selon la direction de déplacement).
#'
#' @param data Tibble nettoye avec Latitude, Longitude, Distance, Swath
#' @param crs Systeme de reference (defaut 4326 pour WGS84)
#' @return Objet SF avec geometries POLYGON
#' @noRd
#' @examples
#' \dontrun{
#' sf_data <- data_to_sf(data)
#' plot(sf_data["Yield"])
#' }
data_to_sf <- function(data, crs = 4326) {

  if (!all(c("Latitude", "Longitude", "Distance", "Swath") %in% names(data))) {
    rlang::warn("Colonnes Latitude, Longitude, Distance, Swath requises")
    return(NULL)
  }

  if (nrow(data) == 0) {
    rlang::warn("Aucune donnee a convertir en SF")
    return(NULL)
  }

  rlang::inform("Creation d'un objet SF avec polygones...")

  # Fonction helper pour creer un rectangle oriente - bien ferme
  create_rectangle <- function(lon, lat, heading_deg, width_m, length_m) {
    # Valeurs minimales pour eviter les polygones degeneres
    width_m <- max(width_m, 0.1)  # Minimum 10 cm
    length_m <- max(length_m, 0.1)  # Minimum 10 cm
    
    lat_rad <- lat * pi / 180
    lon_per_m <- 1 / (111320 * cos(lat_rad))
    lat_per_m <- 1 / 110540

    heading_rad <- heading_deg * pi / 180
    half_width <- width_m / 2
    half_length <- length_m / 2

    dx_forward <- sin(heading_rad) * half_length
    dy_forward <- cos(heading_rad) * half_length
    dx_perp <- cos(heading_rad) * half_width
    dy_perp <- -sin(heading_rad) * half_width

    # Creer 5 points (le point 5 = point 1 pour fermer)
    x <- c(
      lon + (dx_forward + dx_perp) * lon_per_m,    # front-right
      lon + (dx_forward - dx_perp) * lon_per_m,    # front-left
      lon + (-dx_forward - dx_perp) * lon_per_m,   # back-left
      lon + (-dx_forward + dx_perp) * lon_per_m,   # back-right
      lon + (dx_forward + dx_perp) * lon_per_m     # back to front-right (CLOSE)
    )

    y <- c(
      lat + (dy_forward + dy_perp) * lat_per_m,
      lat + (dy_forward - dy_perp) * lat_per_m,
      lat + (-dy_forward - dy_perp) * lat_per_m,
      lat + (-dy_forward + dy_perp) * lat_per_m,
      lat + (dy_forward + dy_perp) * lat_per_m
    )

    # Retourner une matrice 5x2 (polygone ferme)
    coords <- matrix(c(x, y), nrow = 5, ncol = 2, byrow = FALSE)
    
    # Verifier qu'il n'y a pas de points dupliques consecutifs
    # Si deux points sont identiques, deplacer legerement le deuxieme
    for (i in 2:5) {
      if (all(abs(coords[i, ] - coords[i-1, ]) < 1e-10)) {
        coords[i, ] <- coords[i, ] + c(1e-8, 1e-8)  # Deplacement minuscule
      }
    }
    
    return(coords)
  }

  # Utiliser le heading du capteur si disponible, sinon calculer
  if ("Heading" %in% names(data) && !all(is.na(data$Heading))) {
    # Utiliser le heading du capteur (deja en degres, 0=Nord, sens horaire)
    data$heading <- data$Heading
    rlang::inform("Utilisation du Heading du capteur")
  } else if (!"heading" %in% names(data)) {
    # Calculer le heading a partir des coordonnees
    # Direction vers le point suivant (ou on va)
    data <- data |>
      dplyr::mutate(
        heading = atan2(
          dplyr::lead(Longitude, default = Longitude[dplyr::n()]) - Longitude,
          dplyr::lead(Latitude, default = Latitude[dplyr::n()]) - Latitude
        ) * 180 / pi
      )
    # Normaliser a 0-360
    data$heading <- (data$heading + 360) %% 360
    data$heading[is.na(data$heading)] <- 0
    rlang::inform("Heading calcule a partir des coordonnees")
  }

   # S'assurer que les colonnes metriques existent
   # Si Swath_m ou Distance_m existent deja, les utiliser directement
   # Sinon, detecter les unites de Swath et Distance
   
   if (!"Swath_m" %in% names(data)) {
     mean_swath <- mean(data$Swath, na.rm = TRUE)
     # Detection des unites Swath:
     # - En metres: 0.5-15m (recolteuses maraicheres: 1-3m, moissonneuses: 6-15m)
     # - En pieds: 20-50 pieds = 6-15m
     # - En pouces: > 100 pouces (rarement utilise)
     
     if (mean_swath > 50) {
       # > 50: probablement pieds ou pouces
       if (mean_swath > 200) {
         # > 200: probablement pouces
         rlang::inform(paste("Swath detecte en pouces (moyenne:", round(mean_swath, 1), "in) - conversion en metres"))
         data$Swath_m <- data$Swath * 0.0254
       } else {
         # 50-200: probablement pieds
         rlang::inform(paste("Swath detecte en pieds (moyenne:", round(mean_swath, 1), "ft) - conversion en metres"))
         data$Swath_m <- data$Swath * 0.3048
       }
     } else {
       # <= 50: probablement metres (inclut petites recolteuses maraicheres 1-3m)
       rlang::inform(paste("Swath detecte en metres (moyenne:", round(mean_swath, 2), "m)"))
       data$Swath_m <- data$Swath
     }
   }

   if (!"Distance_m" %in% names(data)) {
     mean_dist <- mean(data$Distance, na.rm = TRUE)
     # Distance typique entre points: 0.3-5m
     # - En metres: 0.3-5m
     # - En pieds: > 10 pieds
     # - En pouces: > 50 pouces (rarement utilise)
     
     if (mean_dist > 50) {
       # > 50: probablement pouces
       rlang::inform(paste("Distance detectee en pouces (moyenne:", round(mean_dist, 1), "in) - conversion en metres"))
       data$Distance_m <- data$Distance * 0.0254
     } else if (mean_dist > 15) {
       # 15-50: probablement pieds
       rlang::inform(paste("Distance detectee en pieds (moyenne:", round(mean_dist, 1), "ft) - conversion en metres"))
       data$Distance_m <- data$Distance * 0.3048
     } else {
       # <= 15: probablement metres
       rlang::inform(paste("Distance detectee en metres (moyenne:", round(mean_dist, 2), "m)"))
       data$Distance_m <- data$Distance
     }
   }

# Creer Yield_kg_ha si n'existe pas
      if (!"Yield_kg_ha" %in% names(data)) {
        if ("Flow" %in% names(data) && !all(is.na(data$Flow)) && mean(data$Flow, na.rm = TRUE) > 100) {
          # Flow existe avec valeurs > 100, utiliser comme rendement humide (kg/ha)
          data$Yield_kg_ha <- data$Flow
          rlang::inform(paste("Yield_kg_ha cree a partir de Flow (moyenne:", round(mean(data$Yield_kg_ha, na.rm = TRUE), 0), "kg/ha)"))
        }
      }
     # Creer Yield_kg_ha_wet si n'existe pas
     if (!"Yield_kg_ha_wet" %in% names(data) && "Yield_kg_ha" %in% names(data)) {
       data$Yield_kg_ha_wet <- data$Yield_kg_ha
     }
    if (!"Flow_kg_s" %in% names(data) && "Flow" %in% names(data)) {
      data$Flow_kg_s <- data$Flow * 0.453592
    }

  # Filtrer les lignes avec des valeurs manquantes necessaires pour les polygones
  required_cols <- c("Longitude", "Latitude", "heading", "Swath_m", "Distance_m")
  valid_rows <- tryCatch({
    complete_cases <- complete.cases(data[, required_cols, drop = FALSE])
    as.logical(complete_cases)
  }, error = function(e) {
    rep(TRUE, nrow(data))
  })

  if (!all(valid_rows)) {
    n_invalid <- sum(!valid_rows)
    rlang::warn(paste(n_invalid, "lignes avec valeurs manquantes exclues de la creation des polygones"))
    data <- data[valid_rows, , drop = FALSE]
  }

  if (nrow(data) == 0) {
    rlang::warn("Aucune donnee valide pour creer les polygones")
    return(NULL)
  }

  # Creer les geometries des polygones
  rlang::inform("Creation des geometries des polygones...")
  polygons_list <- list()
  valid_indices <- c()
  
  for (i in seq_len(nrow(data))) {
    coords <- create_rectangle(
      data$Longitude[i],
      data$Latitude[i],
      data$heading[i],
      data$Swath_m[i],
      data$Distance_m[i]
    )
    
    # Essayer de creer le polygone, ignorer en cas d'erreur
    tryCatch({
      poly <- sf::st_polygon(list(coords))
      # Verifier que le polygone est valide
      if (sf::st_is_valid(poly)) {
        polygons_list[[length(polygons_list) + 1]] <- poly
        valid_indices <- c(valid_indices, i)
      } else {
        rlang::warn(paste("Polygone", i, "invalide - ignore"))
      }
    }, error = function(e) {
      rlang::warn(paste("Erreur creation polygone", i, ":", conditionMessage(e)))
    })
  }
  
  # Filtrer les donnees pour ne garder que les polygones valides
  if (length(valid_indices) < nrow(data)) {
    rlang::inform(paste(length(valid_indices), "polygones valides sur", nrow(data)))
    data <- data[valid_indices, ]
  }

   # Creer l'objet SF
   # S'assurer que toutes les colonnes ont la meme longueur que les polygones
   n_poly <- length(polygons_list)
   
   sf_data <- sf::st_sf(
     geometry = sf::st_sfc(polygons_list, crs = crs),
      # Colonnes metriques (principales)
     Flow_kg_s = if ("Flow_kg_s" %in% names(data)) data$Flow_kg_s else rep(NA_real_, n_poly),
     Yield_kg_ha = if ("Yield_kg_ha" %in% names(data)) data$Yield_kg_ha else rep(NA_real_, n_poly),
     Yield_kg_ha_wet = if ("Yield_kg_ha_wet" %in% names(data)) data$Yield_kg_ha_wet else rep(NA_real_, n_poly),
     Moisture_pct = if ("Moisture" %in% names(data)) data$Moisture else rep(NA_real_, n_poly),
     Swath_m = if ("Swath_m" %in% names(data)) data$Swath_m else rep(NA_real_, n_poly),
     Distance_m = if ("Distance_m" %in% names(data)) data$Distance_m else rep(NA_real_, n_poly),
     Heading_deg = if ("heading" %in% names(data)) data$heading else rep(NA_real_, n_poly),
     Altitude_m = if ("Altitude" %in% names(data)) data$Altitude * 0.3048 else rep(NA_real_, n_poly),
      # Colonnes imperiales (secondaires)
      Flow_lbs_s = if ("Flow" %in% names(data)) data$Flow else rep(NA_real_, n_poly),
     Swath_in = if ("Swath" %in% names(data)) data$Swath else rep(NA_real_, n_poly),
     Distance_in = if ("Distance" %in% names(data)) data$Distance else rep(NA_real_, n_poly),
     Altitude_ft = if ("Altitude" %in% names(data)) data$Altitude else rep(NA_real_, n_poly),
      # Colonnes de metadonnees
     GPS_Time = if ("GPS_Time" %in% names(data)) data$GPS_Time else rep(NA_real_, n_poly),
     HeaderStatus = if ("HeaderStatus" %in% names(data)) data$HeaderStatus else rep(NA_integer_, n_poly),
     Pass = if ("Pass" %in% names(data)) data$Pass else rep(NA_integer_, n_poly),
     Longitude = if ("Longitude" %in% names(data)) data$Longitude else rep(NA_real_, n_poly),
     Latitude = if ("Latitude" %in% names(data)) data$Latitude else rep(NA_real_, n_poly),
     X_utm = if ("X" %in% names(data)) data$X else rep(NA_real_, n_poly),
     Y_utm = if ("Y" %in% names(data)) data$Y else rep(NA_real_, n_poly),
     orig_row_id = if ("orig_row_id" %in% names(data)) data$orig_row_id else seq_len(n_poly),
     Variety = if ("Variety" %in% names(data)) data$Variety else rep(NA_character_, n_poly),
     GrainType = if ("GrainType" %in% names(data)) data$GrainType else rep(NA_character_, n_poly)
   )

  rlang::inform(paste("Objet SF cree :", nrow(sf_data), "polygones"))

  return(sf_data)
}


#' Convertir les donnees nettoyees en points SF
#'
#' Cette fonction crée un objet SF avec des points (centroïdes) au lieu de polygones.
#'
#' @param data Tibble nettoye
#' @param crs Systeme de reference
#' @return Objet SF avec geometries POINT
#' @noRd
#' @examples
#' \dontrun{
#' sf_points <- data_to_sf_points(data)
#' plot(sf_points["Yield"])
#' }
data_to_sf_points <- function(data, crs = 4326) {

  if (!all(c("Latitude", "Longitude") %in% names(data))) {
    rlang::warn("Colonnes Latitude, Longitude requises")
    return(NULL)
  }

  rlang::inform("Creation d'un objet SF avec points...")

    # Creer Yield_kg_ha si n'existe pas
     if (!"Yield_kg_ha" %in% names(data)) {
       if ("Flow" %in% names(data) && mean(data$Flow, na.rm = TRUE) > 100) {
         # Flow existe avec valeurs > 100, utiliser comme rendement humide (kg/ha)
         data$Yield_kg_ha <- data$Flow
         rlang::inform(paste("Yield_kg_ha cree a partir de Flow (moyenne:", round(mean(data$Yield_kg_ha, na.rm = TRUE), 0), "kg/ha)"))
       }
     }
     # Creer Yield_kg_ha_wet si n'existe pas
     if (!"Yield_kg_ha_wet" %in% names(data) && "Yield_kg_ha" %in% names(data)) {
       data$Yield_kg_ha_wet <- data$Yield_kg_ha
     }
    if ("Flow" %in% names(data)) {
      data$Flow_kg_s <- data$Flow * 0.453592
    }
  data$Swath_m <- data$Swath * 0.0254
  data$Distance_m <- data$Distance * 0.0254
  data$Altitude_m <- data$Altitude * 0.3048

   # Creer l'objet SF avec points
   sf_data <- sf::st_as_sf(
     data,
     coords = c("Longitude", "Latitude"),
     crs = crs,
     agr = "identity"
   ) |>
     dplyr::mutate(
       # Yield_kg_ha deja calcule ci-dessus avec detection d'unites
       Flow_kg_s = Flow * 0.453592,
       Swath_m = Swath * 0.0254,
       Distance_m = Distance * 0.0254,
       Altitude_m = Altitude * 0.3048
     )

  rlang::inform(paste("Objet SF points cree :", nrow(sf_data), "points"))

  return(sf_data)
}


#' Tracer les donnees de rendement SF
#'
#' Fonction utilitaire pour visualiser les donnees de rendement SF.
#'
#' @param sf_data Objet SF issu de clean_yield ou ayce_sf
#' @param column Colonne a tracer (defaut "Yield")
#' @param ... Arguments additionnels passes a plot()
#' @return Objet plot
#' @noRd
#' @examples
#' \dontrun{
#' clean_yield("data.txt", polygon = TRUE) |> plot_yield()
#' }
plot_yield <- function(sf_data, column = "Yield", ...) {
  if (!inherits(sf_data, "sf")) {
    rlang::abort("sf_data must be an SF object")
  }

  if (column == "Yield") {
     # Toujours utiliser kg/ha
     column <- "Yield_kg_ha"
     title <- "Rendement (kg/ha)"
   } else {
    title <- column
  }

  col <- grDevices::hcl.colors(100, "YlOrRd", rev = TRUE)

  plot(sf_data[column],
       main = title,
       pal = col,
       ...)
}


#' Statistiques de synthese pour les donnees SF
#'
#' Genere un resume statistique des donnees de rendement.
#'
#' @param sf_data Objet SF issu de clean_yield
#' @return Data frame avec statistiques
#' @noRd
#' @examples
#' \dontrun{
#' clean_yield("data.txt", polygon = TRUE) |> yield_summary()
#' }
yield_summary <- function(sf_data) {
  if (!inherits(sf_data, "sf")) {
    rlang::abort("sf_data must be an SF object")
  }

  # Toujours utiliser kg/ha
   yield_col <- "Yield_kg_ha"

  dplyr::summarise(sf_data,
    n = dplyr::n(),
    Yield_mean = mean(!!dplyr::sym(yield_col), na.rm = TRUE),
    Yield_sd = sd(!!dplyr::sym(yield_col), na.rm = TRUE),
    Yield_min = min(!!dplyr::sym(yield_col), na.rm = TRUE),
    Yield_max = max(!!dplyr::sym(yield_col), na.rm = TRUE),
    Area_ha = sum(sf::st_area(geometry)) / 10000,  # Conversion m2 -> ha
    Total_mass_kg = sum(!!dplyr::sym(yield_col) * sf::st_area(geometry) / 10000)
  )
}
