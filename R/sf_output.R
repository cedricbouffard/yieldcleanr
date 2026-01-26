#' Complete AYCE pipeline with SF output
#'
#' Cette fonction exécute le pipeline AYCE complet et retourne un objet SF
#' avec des polygones rectangles orientés et toutes les mesures en métrique.
#' Wrapper around clean_yield() with metric + polygon output.
#'
#' @param file_path Path to input file
#' @param output_file Optional path to save GeoJSON
#' @param log_file Optional path to save log
#' @param geometry_type "polygon" or "point" (kept for compatibility)
#' @param params AYCE parameters list
#' @return SF object with cleaned data
#' @noRd
#' @examples
#' \dontrun{
#' sf_result <- ayce_sf(
#'   file_path = "data.txt",
#'   geometry_type = "polygon"
#' )
#' plot(sf_result["Yield"])
#' }
ayce_sf <- function(file_path, output_file = NULL, log_file = NULL,
                    geometry_type = c("polygon", "point"),
                    params = NULL) {

  geometry_type <- match.arg(geometry_type)

  if (geometry_type == "point") {
    # Point output (tibble without geometry for compatibility)
    clean_yield(file_path = file_path,
                metrique = TRUE,
                polygon = FALSE,
                params = params,
                output_file = output_file,
                log_file = log_file)
  } else {
    # Polygon output (SF object)
    clean_yield(file_path = file_path,
                metrique = TRUE,
                polygon = TRUE,
                params = params,
                output_file = output_file,
                log_file = log_file)
  }
}


#' Convert cleaned data to SF object with polygon geometries
#'
#' Cette fonction convertit les données nettoyées en objet SF avec des polygones
#' représentant la zone récoltée (rectangles orientés selon la direction de déplacement).
#'
#' @param data Cleaned data tibble with Latitude, Longitude, Distance, Swath columns
#' @param crs Coordinate reference system (default 4326 for WGS84)
#' @return SF object with POLYGON geometries
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

  rlang::inform("Creating SF object with polygon geometries...")

  # Helper function to create oriented rectangle polygon - PROPERLY CLOSED
  create_rectangle <- function(lon, lat, heading_deg, width_m, length_m) {
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

    # Create 5 points (point 5 = point 1 to close the polygon)
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

    # Return matrix with 5 rows (closed polygon)
    matrix(c(x, y), nrow = 5, ncol = 2, byrow = FALSE)
  }

  # Calculate heading from coordinates if not present
  if (!"heading" %in% names(data)) {
    data <- data |>
      dplyr::mutate(
        heading = atan2(
          dplyr::lag(Longitude, default = Longitude[1]) - Longitude,
          dplyr::lag(Latitude, default = Latitude[1]) - Latitude
        ) * 180 / pi
      )
    data$heading[is.na(data$heading)] <- 0
  }

  # Ensure metric columns exist
  if (!"Swath_m" %in% names(data)) {
    data$Swath_m <- data$Swath * 0.0254
  }
  if (!"Distance_m" %in% names(data)) {
    data$Distance_m <- data$Distance * 0.0254
  }
  if (!"Yield_kg_ha" %in% names(data)) {
    data$Yield_kg_ha <- data$Yield_buacre * 67.25
  }
  if (!"Flow_kg_s" %in% names(data)) {
    data$Flow_kg_s <- data$Flow * 0.453592
  }

  # Create polygon geometries
  rlang::inform("Creating polygon geometries...")
  polygons_list <- list()

  for (i in 1:nrow(data)) {
    coords <- create_rectangle(
      data$Longitude[i],
      data$Latitude[i],
      data$heading[i],
      data$Swath_m[i],
      data$Distance_m[i]
    )
    polygons_list[[i]] <- sf::st_polygon(list(coords))
  }

   # Create SF object
   sf_data <- sf::st_sf(
     geometry = sf::st_sfc(polygons_list, crs = crs),
     # Metric columns (primary)
     Flow_kg_s = data$Flow_kg_s,
     Yield_kg_ha = data$Yield_kg_ha,
     Moisture_pct = data$Moisture,
     Swath_m = data$Swath_m,
     Distance_m = data$Distance_m,
     Heading_deg = data$heading,
     Altitude_m = data$Altitude * 0.3048,
     # Imperial columns (secondary)
     Flow_lbs_s = data$Flow,
     Yield_bu_ac = data$Yield_buacre,
     Swath_in = data$Swath,
     Distance_in = data$Distance,
     Altitude_ft = data$Altitude,
     # Metadata columns
     GPS_Time = data$GPS_Time,
     HeaderStatus = data$HeaderStatus,
     Pass = data$Pass,
     Longitude = data$Longitude,
     Latitude = data$Latitude,
     X_utm = data$X,
     Y_utm = data$Y,
     orig_row_id = if ("orig_row_id" %in% names(data)) data$orig_row_id else seq_len(nrow(data)),
     Variety = if ("Variety" %in% names(data)) data$Variety else NA_character_,
     GrainType = if ("GrainType" %in% names(data)) data$GrainType else NA_character_
   )

  rlang::inform(paste("SF object created:", nrow(sf_data), "polygons"))

  return(sf_data)
}


#' Convert cleaned data to SF points
#'
#' Cette fonction crée un objet SF avec des points (centroïdes) au lieu de polygones.
#'
#' @param data Cleaned data tibble
#' @param crs Coordinate reference system
#' @return SF object with POINT geometries
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

  rlang::inform("Creating SF object with point geometries...")

  # Calculate yield in kg/ha
  data$Yield_kg_ha <- data$Yield_buacre * 67.25
  data$Flow_kg_s <- data$Flow * 0.453592
  data$Swath_m <- data$Swath * 0.0254
  data$Distance_m <- data$Distance * 0.0254
  data$Altitude_m <- data$Altitude * 0.3048

  # Create SF object with points
  sf_data <- sf::st_as_sf(
    data,
    coords = c("Longitude", "Latitude"),
    crs = crs,
    agr = "identity"
  ) |>
    dplyr::mutate(
      Yield_kg_ha = Yield_buacre * 67.25,
      Flow_kg_s = Flow * 0.453592,
      Swath_m = Swath * 0.0254,
      Distance_m = Distance * 0.0254,
      Altitude_m = Altitude * 0.3048
    )

  rlang::inform(paste("SF points object created:", nrow(sf_data), "points"))

  return(sf_data)
}


#' Plot SF yield data
#'
#' Fonction utilitaire pour visualiser les données de rendement SF.
#'
#' @param sf_data SF object from clean_yield or ayce_sf
#' @param column Column to plot (default "Yield")
#' @param ... Additional arguments passed to plot()
#' @return Plot object
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
    # Use the appropriate yield column based on units
    if ("Yield_kg_ha" %in% names(sf_data)) {
      column <- "Yield_kg_ha"
      title <- "Yield (kg/ha)"
    } else {
      column <- "Yield_bu_ac"
      title <- "Yield (bu/acre)"
    }
  } else {
    title <- column
  }

  col <- grDevices::hcl.colors(100, "YlOrRd", rev = TRUE)

  plot(sf_data[column],
       main = title,
       pal = col,
       ...)
}


#' Summary statistics for SF yield data
#'
#' Génère un résumé statistique des données de rendement.
#'
#' @param sf_data SF object from clean_yield
#' @return Data frame with statistics
#' @noRd
#' @examples
#' \dontrun{
#' clean_yield("data.txt", polygon = TRUE) |> yield_summary()
#' }
yield_summary <- function(sf_data) {
  if (!inherits(sf_data, "sf")) {
    rlang::abort("sf_data must be an SF object")
  }

  # Use appropriate yield column
  yield_col <- if ("Yield_kg_ha" %in% names(sf_data)) "Yield_kg_ha" else "Yield_bu_ac"

  dplyr::summarise(sf_data,
    n = dplyr::n(),
    Yield_mean = mean(!!dplyr::sym(yield_col), na.rm = TRUE),
    Yield_sd = sd(!!dplyr::sym(yield_col), na.rm = TRUE),
    Yield_min = min(!!dplyr::sym(yield_col), na.rm = TRUE),
    Yield_max = max(!!dplyr::sym(yield_col), na.rm = TRUE),
    Area_ha = sum(sf::st_area(geometry)) / 10000,  # Convert m² to ha
    Total_mass_kg = sum(!!dplyr::sym(yield_col) * sf::st_area(geometry) / 10000)
  )
}
