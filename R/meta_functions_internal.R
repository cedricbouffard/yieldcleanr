# =============================================================================
# Fonctions internes pour les métafonctions
# =============================================================================
# Ce fichier contient les fonctions internes utilisées par les méta-fonctions
# exportées dans meta_functions.R
# =============================================================================

#' @noRd
.export_csv_internal <- function(data, file, params) {
  # Convertir sf en tibble si nécessaire
  if (inherits(data, "sf")) {
    data <- sf::st_drop_geometry(data)
  }

  write.csv(data, file, row.names = FALSE)
}

#' @noRd
.export_geojson_internal <- function(data, file, params) {
  # Convertir en sf si nécessaire
  if (!inherits(data, "sf")) {
    if (all(c("Longitude", "Latitude") %in% names(data))) {
      data <- sf::st_as_sf(data, coords = c("Longitude", "Latitude"), crs = 4326)
    } else if (all(c("X", "Y") %in% names(data))) {
      data <- sf::st_as_sf(data, coords = c("X", "Y"), crs = params$crs %||% 4326)
    } else {
      rlang::abort("Coordonnées non trouvées pour l'export GeoJSON")
    }
  }

  sf::st_write(data, file, driver = "GeoJSON", delete_dsn = TRUE)
}

#' @noRd
.export_shapefile_internal <- function(data, file, params) {
  if (!inherits(data, "sf")) {
    rlang::abort("Les données doivent être au format sf pour l'export Shapefile")
  }

  sf::st_write(data, file, driver = "ESRI Shapefile", delete_layer = TRUE)
}

#' @noRd
.export_geopackage_internal <- function(data, file, params) {
  if (!inherits(data, "sf")) {
    rlang::abort("Les données doivent être au format sf pour l'export GeoPackage")
  }

  layer_name <- params$layer %||% "yield_data"
  sf::st_write(data, file, layer = layer_name, driver = "GPKG", delete_layer = TRUE)
}

#' @noRd
.export_raster_internal <- function(data, file, params) {
  if (!requireNamespace("terra", quietly = TRUE)) {
    rlang::abort("Package 'terra' requis pour l'export raster")
  }

  # Convertir en sf si nécessaire
  if (!inherits(data, "sf")) {
    if (all(c("Longitude", "Latitude") %in% names(data))) {
      data <- sf::st_as_sf(data, coords = c("Longitude", "Latitude"), crs = 4326)
    } else if (all(c("X", "Y") %in% names(data))) {
      data <- sf::st_as_sf(data, coords = c("X", "Y"), crs = params$crs %||% 4326)
    } else {
      rlang::abort("Coordonnées non trouvées pour l'export raster")
    }
  }

  # Déterminer la colonne de rendement
  yield_col <- NULL
  for (col in c("Yield_kg_ha", "Yield", "Flow")) {
    if (col %in% names(data)) {
      yield_col <- col
      break
    }
  }

  if (is.null(yield_col)) {
    rlang::abort("Colonne de rendement non trouvée pour l'export raster")
  }

  # Paramètres de rasterisation
  resolution <- params$resolution %||% 5
  
  # Créer le raster
  terra::terraOptions(progress = 0)
  r <- terra::rast(data, resolution = resolution)
  r <- terra::rasterize(data, r, field = yield_col, fun = mean)
  
  # Sauvegarder
  terra::writeRaster(r, file, overwrite = TRUE)
}
