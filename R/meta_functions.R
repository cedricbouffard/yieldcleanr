#' Méta-fonction de filtrage unifiée
#'
#' Cette fonction unifiée permet d'appliquer un ou plusieurs filtres sur les données
#' de rendement. Elle remplace les fonctions individuelles filter_header_status(),
#' filter_gps_status(), filter_velocity(), filter_yield_range(), filter_moisture_range(),
#' filter_dop() et filter_bounds().
#'
#' @param data Tibble avec données de rendement
#' @param type Type de filtre à appliquer. Peut être un vecteur pour appliquer
#'   plusieurs filtres en séquence. Options: "header", "gps", "dop", "velocity",
#'   "yield", "moisture", "bounds", "all"
#' @param ... Paramètres spécifiques au type de filtre:
#'   - header: header_values (défaut: c(1, 33))
#'   - gps: min_gps_status (défaut: 4)
#'   - dop: max_dop (défaut: 10)
#'   - velocity: min_velocity, max_velocity (défaut: auto-calculé)
#'   - yield: min_yield, max_yield, n_std (défaut: auto-calculé avec 3 SD)
#'   - moisture: min_moisture, max_moisture, n_std (défaut: auto-calculé avec 3 SD)
#'   - bounds: bounds (liste avec min_x, max_x, min_y, max_y), coord_type ("utm" ou "latlon")
#' @return Tibble filtré selon les critères spécifiés
#' @export
#' @examples
#' \dontrun{
#' # Filtrer uniquement le header
#' data_filtered <- filter_data(data, type = "header")
#'
#' # Filtrer header et GPS
#' data_filtered <- filter_data(data, type = c("header", "gps"))
#'
#' # Appliquer tous les filtres disponibles
#' data_filtered <- filter_data(data, type = "all")
#'
#' # Filtrer avec paramètres personnalisés
#' data_filtered <- filter_data(data, type = "velocity",
#'                              min_velocity = 1.0, max_velocity = 5.0)
#' }
filter_data <- function(data, type = "all", ...) {
  params <- list(...)

  # Si type est "all", appliquer tous les filtres dans l'ordre logique
  if (identical(type, "all")) {
    type <- c("header", "gps", "dop", "velocity", "yield", "moisture")
  }

  # S'assurer que type est un vecteur
  if (!is.character(type)) {
    rlang::abort("Le paramètre 'type' doit être une chaîne de caractères ou un vecteur")
  }

  # Ordre logique des filtres (indépendamment de l'ordre demandé)
  filter_order <- c("header", "gps", "dop", "velocity", "yield", "moisture", "bounds")
  type <- filter_order[filter_order %in% type]

  n_initial <- nrow(data)
  filter_log <- list()

  for (filter_type in type) {
    n_before <- nrow(data)

    data <- switch(filter_type,
      header = .filter_header_internal(data,
        header_values = params$header_values %||% c(1, 33)
      ),
      gps = .filter_gps_internal(data,
        min_gps_status = params$min_gps_status %||% 4
      ),
      dop = .filter_dop_internal(data,
        max_dop = params$max_dop %||% 10
      ),
      velocity = .filter_velocity_internal(data,
        min_velocity = params$min_velocity,
        max_velocity = params$max_velocity
      ),
      yield = .filter_yield_internal(data,
        min_yield = params$min_yield,
        max_yield = params$max_yield,
        n_std = params$n_std %||% 3
      ),
      moisture = .filter_moisture_internal(data,
        min_moisture = params$min_moisture,
        max_moisture = params$max_moisture,
        n_std = params$n_std %||% 3
      ),
      bounds = .filter_bounds_internal(data,
        bounds = params$bounds,
        coord_type = params$coord_type %||% "utm"
      ),
      {
        rlang::warn(paste("Type de filtre inconnu:", filter_type))
        data
      }
    )

    n_removed <- n_before - nrow(data)
    if (n_removed > 0) {
      filter_log[[filter_type]] <- n_removed
    }
  }

  # Résumé des filtres appliqués
  n_final <- nrow(data)
  if (n_final < n_initial) {
    rlang::inform(paste(
      "Filtres appliqués:", n_initial - n_final, "points éliminés au total"
    ))
    if (length(filter_log) > 0) {
      for (ftype in names(filter_log)) {
        rlang::inform(paste0("  - ", ftype, ": ", filter_log[[ftype]], " points"))
      }
    }
  }

  return(data)
}

# Fonctions internes (non exportées)

#' @noRd
.filter_header_internal <- function(data, header_values = c(0, 1, 33)) {
  if (!"HeaderStatus" %in% names(data)) {
    return(data)
  }

  n_before <- nrow(data)
  data <- data |>
    dplyr::filter(HeaderStatus %in% header_values | is.na(HeaderStatus))
  n_removed <- n_before - nrow(data)

  if (n_removed > 0) {
    rlang::inform(paste("Filtre header:", n_removed, "points éliminés"))
  }
  return(data)
}

#' @noRd
.filter_gps_internal <- function(data, min_gps_status = 4) {
  if (!"GPSStatus" %in% names(data)) {
    return(data)
  }

  n_before <- nrow(data)
  data$GPSStatus <- suppressWarnings(as.numeric(data$GPSStatus))
  data <- data |>
    dplyr::filter(is.na(GPSStatus) | GPSStatus >= min_gps_status)
  n_removed <- n_before - nrow(data)

  if (n_removed > 0) {
    rlang::inform(paste("Filtre GPS:", n_removed, "points éliminés"))
  }
  return(data)
}

#' @noRd
.filter_dop_internal <- function(data, max_dop = 10) {
  if (!"DOP" %in% names(data)) {
    return(data)
  }

  n_before <- nrow(data)
  data <- data |>
    dplyr::filter(is.na(DOP) | DOP <= max_dop)
  n_removed <- n_before - nrow(data)

  if (n_removed > 0) {
    rlang::inform(paste("Filtre DOP:", n_removed, "points éliminés"))
  }
  return(data)
}

#' @noRd
.filter_velocity_internal <- function(data, min_velocity = NULL, max_velocity = NULL) {
  if (!all(c("X", "Y") %in% names(data))) {
    rlang::warn("Coordonnées X,Y non trouvées - saut du filtre vitesse")
    return(data)
  }

  # Check if Interval column exists
  has_interval <- "Interval" %in% names(data)
  
  # Calcul automatique des seuils si non fournis
  if (is.null(min_velocity) || is.null(max_velocity)) {
    if (has_interval) {
      data_temp <- data |>
        dplyr::mutate(
          velocity = sqrt((X - dplyr::lag(X))^2 + (Y - dplyr::lag(Y))^2) / Interval
        )
    } else {
      data_temp <- data |>
        dplyr::mutate(
          velocity = sqrt((X - dplyr::lag(X))^2 + (Y - dplyr::lag(Y))^2)
        )
    }
    data_temp$velocity[is.na(data_temp$velocity)] <- 0

    vel_values <- data_temp$velocity[is.finite(data_temp$velocity)]
    q05 <- quantile(vel_values, 0.05, na.rm = TRUE)
    q95 <- quantile(vel_values, 0.95, na.rm = TRUE)
    iqr <- q95 - q05

    if (is.null(min_velocity)) min_velocity <- max(0.5, q05 - 1.5 * iqr)
    if (is.null(max_velocity)) max_velocity <- q95 + 1.5 * iqr
  }

  n_before <- nrow(data)
  if (has_interval) {
    data <- data |>
      dplyr::mutate(
        velocity = sqrt((X - dplyr::lag(X))^2 + (Y - dplyr::lag(Y))^2) / Interval
      )
  } else {
    data <- data |>
      dplyr::mutate(
        velocity = sqrt((X - dplyr::lag(X))^2 + (Y - dplyr::lag(Y))^2)
      )
  }
  data$velocity[is.na(data$velocity)] <- 0

  data <- data |>
    dplyr::filter(velocity >= min_velocity & velocity <= max_velocity) |>
    dplyr::select(-velocity)

  n_removed <- n_before - nrow(data)
  if (n_removed > 0) {
    rlang::inform(paste(
      "Filtre vitesse:", n_removed, "points éliminés",
      "(plage:", round(min_velocity, 2), "-", round(max_velocity, 2), ")"
    ))
  }
  return(data)
}

#' @noRd
.filter_yield_internal <- function(data, min_yield = NULL, max_yield = NULL, n_std = 3) {
  # Détecter la colonne de rendement
  yield_col <- NULL
  for (col in c("Yield_kg_ha", "Yield", "Flow")) {
    if (col %in% names(data)) {
      yield_col <- col
      break
    }
  }

  if (is.null(yield_col)) {
    rlang::warn("Colonne de rendement non trouvée - saut du filtre")
    return(data)
  }

  # Calcul automatique des seuils si non fournis
  if (is.null(min_yield) || is.null(max_yield)) {
    yield_vals <- data[[yield_col]]
    yield_vals <- yield_vals[is.finite(yield_vals)]

    mean_yield <- mean(yield_vals, na.rm = TRUE)
    sd_yield <- stats::sd(yield_vals, na.rm = TRUE)

    if (is.null(min_yield)) min_yield <- max(0, mean_yield - n_std * sd_yield)
    if (is.null(max_yield)) max_yield <- mean_yield + n_std * sd_yield
  }

  n_before <- nrow(data)
  data <- data |>
    dplyr::filter(is.finite(.data[[yield_col]])) |>
    dplyr::filter(.data[[yield_col]] >= min_yield & .data[[yield_col]] <= max_yield)

  n_removed <- n_before - nrow(data)
  if (n_removed > 0) {
    rlang::inform(paste(
      "Filtre rendement:", n_removed, "points éliminés",
      "(plage:", round(min_yield, 1), "-", round(max_yield, 1), ")"
    ))
  }
  return(data)
}

#' @noRd
.filter_moisture_internal <- function(data, min_moisture = NULL, max_moisture = NULL, n_std = 3) {
  if (!"Moisture" %in% names(data)) {
    return(data)
  }

  # Calcul automatique des seuils si non fournis
  if (is.null(min_moisture) || is.null(max_moisture)) {
    mean_moisture <- mean(data$Moisture, na.rm = TRUE)
    sd_moisture <- stats::sd(data$Moisture, na.rm = TRUE)

    if (is.null(min_moisture)) min_moisture <- mean_moisture - n_std * sd_moisture
    if (is.null(max_moisture)) max_moisture <- mean_moisture + n_std * sd_moisture
  }

  n_before <- nrow(data)
  data <- data |>
    dplyr::filter(Moisture >= min_moisture & Moisture <= max_moisture)

  n_removed <- n_before - nrow(data)
  if (n_removed > 0) {
    rlang::inform(paste(
      "Filtre humidité:", n_removed, "points éliminés",
      "(plage:", round(min_moisture, 1), "-", round(max_moisture, 1), ")"
    ))
  }
  return(data)
}

#' @noRd
.filter_bounds_internal <- function(data, bounds = NULL, coord_type = "utm") {
  if (is.null(bounds)) {
    return(data)
  }

  n_before <- nrow(data)

  if (coord_type == "utm") {
    data <- data |>
      dplyr::filter(
        X >= bounds$min_x, X <= bounds$max_x,
        Y >= bounds$min_y, Y <= bounds$max_y
      )
  } else {
    data <- data |>
      dplyr::filter(
        Longitude >= bounds$min_x, Longitude <= bounds$max_x,
        Latitude >= bounds$min_y, Latitude <= bounds$max_y
      )
  }

  n_removed <- n_before - nrow(data)
  if (n_removed > 0) {
    rlang::inform(paste("Filtre limites:", n_removed, "points éliminés"))
  }
  return(data)
}


#' Méta-fonction de détection d'anomalies
#'
#' Cette fonction unifiée détecte et filtre différents types d'anomalies dans les
#' données de rendement : chevauchements, écarts-types locaux, changements brusques
#' de vitesse, anomalies de direction et points hors champ.
#'
#' @param data Tibble avec données de rendement
#' @param type Type d'anomalie à détecter. Peut être un vecteur. Options:
#'   "overlap" (chevauchement), "local_sd" (écart-type local), "velocity_jump"
#'   (changement de vitesse), "heading" (direction), "position" (position),
#'   "all" (toutes)
#' @param action Action à effectuer: "filter" (filtrer les anomalies), "detect"
#'   (marquer sans filtrer), ou "report" (rapport uniquement). Défaut: "filter"
#' @param ... Paramètres spécifiques au type d'anomalie:
#'   - overlap: cellsize (défaut: 0.3), max_pass (défaut: 50)
#'   - local_sd: n_swaths (défaut: 5), lsd_limit (défaut: 2.4), min_cells (défaut: 3)
#'   - velocity_jump: max_acceleration (défaut: 5), max_deceleration (défaut: -8)
#'   - heading: max_heading_change (défaut: 60)
#'   - position: gbuffer (défaut: 100)
#' @return Selon l'action: data filtré, data avec colonnes de marquage, ou rapport
#' @export
#' @examples
#' \dontrun{
#' # Détecter et filtrer toutes les anomalies
#' data_clean <- detect_anomalies(data, type = "all")
#'
#' # Détecter uniquement les chevauchements avec paramètres personnalisés
#' data_clean <- detect_anomalies(data, type = "overlap",
#'                                cellsize = 0.5, max_pass = 30)
#'
#' # Marquer sans filtrer
#' data_marked <- detect_anomalies(data, type = c("overlap", "local_sd"),
#'                                 action = "detect")
#' }
detect_anomalies <- function(data, type = "all", action = "filter", ...) {
  params <- list(...)

  if (identical(type, "all")) {
    type <- c("overlap", "local_sd", "velocity_jump", "heading", "position")
  }

  if (!is.character(type)) {
    rlang::abort("Le paramètre 'type' doit être une chaîne de caractères ou un vecteur")
  }

  n_initial <- nrow(data)
  anomaly_log <- list()

  for (anomaly_type in type) {
    n_before <- nrow(data)

    result <- switch(anomaly_type,
      overlap = .detect_overlap_internal(data,
        cellsize = params$cellsize %||% 0.3,
        max_pass = params$max_pass %||% 50,
        action = action
      ),
      local_sd = .detect_local_sd_internal(data,
        n_swaths = params$n_swaths %||% 5,
        lsd_limit = params$lsd_limit %||% 2.4,
        min_cells = params$min_cells %||% 3,
        action = action
      ),
      velocity_jump = .detect_velocity_jump_internal(data,
        max_acceleration = params$max_acceleration %||% 5,
        max_deceleration = params$max_deceleration %||% -8,
        action = action
      ),
      heading = .detect_heading_internal(data,
        max_heading_change = params$max_heading_change %||% 60,
        action = action
      ),
      position = .detect_position_internal(data,
        gbuffer = params$gbuffer %||% 100,
        action = action
      ),
      {
        rlang::warn(paste("Type d'anomalie inconnu:", anomaly_type))
        list(data = data, count = 0)
      }
    )

    data <- result$data
    if (result$count > 0) {
      anomaly_log[[anomaly_type]] <- result$count
    }
  }

  # Résumé
  if (action == "filter" && length(anomaly_log) > 0) {
    n_final <- nrow(data)
    rlang::inform(paste(
      "Détection d'anomalies:", n_initial - n_final, "points éliminés au total"
    ))
    for (atype in names(anomaly_log)) {
      rlang::inform(paste0("  - ", atype, ": ", anomaly_log[[atype]], " points"))
    }
  }

  return(data)
}

#' @noRd
.detect_overlap_internal <- function(data, cellsize = 0.3, max_pass = 50, action = "filter") {
  if (!all(c("X", "Y") %in% names(data))) {
    return(list(data = data, count = 0))
  }

  # Créer un ID de cellule basé sur la grille
  data <- data |>
    dplyr::mutate(
      .cell_id_x = floor(X / cellsize),
      .cell_id_y = floor(Y / cellsize),
      .cell_id = paste(.cell_id_x, .cell_id_y, sep = "_")
    )

  # Compter le nombre de points par cellule
  cell_counts <- data |>
    dplyr::count(.cell_id, name = ".cell_count")

  # Joindre les comptes
  data <- data |>
    dplyr::left_join(cell_counts, by = ".cell_id") |>
    dplyr::mutate(.is_overlap = .cell_count > max_pass)

  n_anomalies <- sum(data$.is_overlap, na.rm = TRUE)

  if (action == "filter") {
    data <- data |>
      dplyr::filter(!.is_overlap) |>
      dplyr::select(-.cell_id_x, -.cell_id_y, -.cell_id, -.cell_count, -.is_overlap)
  } else if (action == "detect") {
    data <- data |>
      dplyr::rename(overlap_flag = .is_overlap) |>
      dplyr::select(-.cell_id_x, -.cell_id_y, -.cell_id, -.cell_count)
  } else {
    data <- data |>
      dplyr::select(-.cell_id_x, -.cell_id_y, -.cell_id, -.cell_count, -.is_overlap)
  }

  return(list(data = data, count = n_anomalies))
}

#' @noRd
.detect_local_sd_internal <- function(data, n_swaths = 5, lsd_limit = 2.4, min_cells = 3, action = "filter") {
  if (!all(c("X", "Y", "Swath") %in% names(data))) {
    return(list(data = data, count = 0))
  }

  # Calculer la taille de cellule en mètres
  swath_m <- mean(data$Swath, na.rm = TRUE) * 0.0254
  cellsize <- n_swaths * swath_m

  # Créer des cellules spatiales
  data <- data |>
    dplyr::mutate(
      .cell_x = floor(X / cellsize),
      .cell_y = floor(Y / cellsize),
      .cell_id = paste(.cell_x, .cell_y, sep = "_")
    )

  # Calculer les statistiques locales
  # Remove NA values before grouping to avoid issues
  data_clean <- data |>
    dplyr::filter(!is.na(X) & !is.na(Y) & !is.na(Flow))

  cell_stats <- data_clean |>
    dplyr::group_by(.cell_id) |>
    dplyr::summarise(
      local_median = stats::median(Flow, na.rm = TRUE),
      local_mad = stats::mad(Flow, na.rm = TRUE),
      local_mean = mean(Flow, na.rm = TRUE),
      local_sd = stats::sd(Flow, na.rm = TRUE),
      n = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::filter(n >= min_cells)

  # Ensure cell_stats has data - if no cells meet min_cells, use all data
  if (nrow(cell_stats) == 0) {
    cell_stats <- data_clean |>
      dplyr::summarise(
        local_median = stats::median(Flow, na.rm = TRUE),
        local_mad = stats::mad(Flow, na.rm = TRUE),
        local_mean = mean(Flow, na.rm = TRUE),
        local_sd = stats::sd(Flow, na.rm = TRUE),
        n = dplyr::n(),
        .groups = "drop"
      )
  }

  # Statistiques globales pour les cellules avec peu de points
  global_median <- stats::median(data$Flow, na.rm = TRUE)
  global_mad <- stats::mad(data$Flow, na.rm = TRUE)
  global_mean <- mean(data$Flow, na.rm = TRUE)
  global_sd <- stats::sd(data$Flow, na.rm = TRUE)

  # Joindre et identifier les outliers
  # Use MAD for outlier detection (more robust), fall back to SD if MAD is 0
  data <- data |>
    dplyr::left_join(cell_stats, by = ".cell_id") |>
    dplyr::mutate(
      local_median = dplyr::coalesce(local_median, global_median),
      local_mad = dplyr::coalesce(local_mad, global_mad),
      local_mean = dplyr::coalesce(local_mean, global_mean),
      local_sd = dplyr::coalesce(local_sd, global_sd),
      # Use MAD if available and non-zero, otherwise use SD
      upper_limit = ifelse(local_mad > 0, local_median + lsd_limit * local_mad * 1.4826,
                           local_mean + lsd_limit * local_sd),
      lower_limit = ifelse(local_mad > 0, local_median - lsd_limit * local_mad * 1.4826,
                           local_mean - lsd_limit * local_sd),
      .is_outlier = Flow > upper_limit | Flow < lower_limit
    )

  n_anomalies <- sum(data$.is_outlier, na.rm = TRUE)

  if (action == "filter") {
    data <- data |>
      dplyr::filter(!.is_outlier) |>
      dplyr::select(-.cell_x, -.cell_y, -.cell_id, -local_median, -local_mad, -local_mean, -local_sd, -n, -upper_limit, -lower_limit, -.is_outlier)
  } else if (action == "detect") {
    data <- data |>
      dplyr::rename(local_sd_outlier = .is_outlier) |>
      dplyr::select(-.cell_x, -.cell_y, -.cell_id, -local_median, -local_mad, -local_mean, -local_sd, -n, -upper_limit, -lower_limit)
  } else {
    data <- data |>
      dplyr::select(-.cell_x, -.cell_y, -.cell_id, -local_median, -local_mad, -local_mean, -local_sd, -n, -upper_limit, -lower_limit, -.is_outlier)
  }

  return(list(data = data, count = n_anomalies))
}

#' @noRd
.detect_velocity_jump_internal <- function(data, max_acceleration = 5, max_deceleration = -8, action = "filter") {
  if (!all(c("X", "Y", "Interval") %in% names(data))) {
    return(list(data = data, count = 0))
  }

  if (!"GPS_Time" %in% names(data)) {
    data <- data |> dplyr::mutate(GPS_Time = dplyr::row_number())
  }

  data_calc <- data |>
    dplyr::arrange(GPS_Time) |>
    dplyr::mutate(
      .dist_next = sqrt((dplyr::lead(X) - X)^2 + (dplyr::lead(Y) - Y)^2),
      .dist_prev = sqrt((X - dplyr::lag(X))^2 + (Y - dplyr::lag(Y))^2),
      .vel_next = .dist_next / dplyr::coalesce(dplyr::lead(Interval), 1),
      .vel_prev = .dist_prev / dplyr::coalesce(dplyr::lag(Interval), 1),
      .acceleration = (.vel_next - .vel_prev) / dplyr::coalesce(Interval, 1),
      .is_jump = !is.na(.acceleration) &
        (.acceleration > max_acceleration | .acceleration < max_deceleration)
    )

  n_anomalies <- sum(data_calc$.is_jump, na.rm = TRUE)

  if (action == "filter") {
    data <- data_calc |>
      dplyr::filter(!.is_jump) |>
      dplyr::select(-.dist_next, -.dist_prev, -.vel_next, -.vel_prev, -.acceleration, -.is_jump)
  } else if (action == "detect") {
    data <- data_calc |>
      dplyr::rename(velocity_jump = .is_jump) |>
      dplyr::select(-.dist_next, -.dist_prev, -.vel_next, -.vel_prev, -.acceleration)
  } else {
    data <- data_calc |>
      dplyr::select(-.dist_next, -.dist_prev, -.vel_next, -.vel_prev, -.acceleration, -.is_jump)
  }

  return(list(data = data, count = n_anomalies))
}

#' @noRd
.detect_heading_internal <- function(data, max_heading_change = 60, action = "filter") {
  if (!all(c("X", "Y") %in% names(data))) {
    return(list(data = data, count = 0))
  }

  if (!"GPS_Time" %in% names(data)) {
    data <- data |> dplyr::mutate(GPS_Time = dplyr::row_number())
  }

  data_calc <- data |>
    dplyr::arrange(GPS_Time) |>
    dplyr::mutate(
      .dx_next = dplyr::lead(X) - X,
      .dy_next = dplyr::lead(Y) - Y,
      .dx_prev = X - dplyr::lag(X),
      .dy_prev = Y - dplyr::lag(Y),
      .heading_next = (atan2(.dx_next, .dy_next) * 180 / pi) %% 360,
      .heading_prev = (atan2(.dx_prev, .dy_prev) * 180 / pi) %% 360,
      .heading_change = pmin(
        abs(.heading_next - .heading_prev),
        360 - abs(.heading_next - .heading_prev)
      ),
      .is_anomaly = !is.na(.heading_change) & .heading_change > max_heading_change
    )

  n_anomalies <- sum(data_calc$.is_anomaly, na.rm = TRUE)

  if (action == "filter") {
    data <- data_calc |>
      dplyr::filter(!.is_anomaly) |>
      dplyr::select(-.dx_next, -.dy_next, -.dx_prev, -.dy_prev,
                    -.heading_next, -.heading_prev, -.heading_change, -.is_anomaly)
  } else if (action == "detect") {
    data <- data_calc |>
      dplyr::rename(heading_anomaly = .is_anomaly) |>
      dplyr::select(-.dx_next, -.dy_next, -.dx_prev, -.dy_prev,
                    -.heading_next, -.heading_prev, -.heading_change)
  } else {
    data <- data_calc |>
      dplyr::select(-.dx_next, -.dy_next, -.dx_prev, -.dy_prev,
                    -.heading_next, -.heading_prev, -.heading_change, -.is_anomaly)
  }

  return(list(data = data, count = n_anomalies))
}

#' @noRd
.detect_position_internal <- function(data, gbuffer = 100, action = "filter") {
  if (!all(c("X", "Y") %in% names(data))) {
    return(list(data = data, count = 0))
  }

  # Calculer les limites avec buffer
  x_range <- range(data$X, na.rm = TRUE)
  y_range <- range(data$Y, na.rm = TRUE)

  x_min <- x_range[1] + gbuffer
  x_max <- x_range[2] - gbuffer
  y_min <- y_range[1] + gbuffer
  y_max <- y_range[2] - gbuffer

  # Vérifier que les limites sont valides
  if (x_min >= x_max || y_min >= y_max) {
    rlang::warn("Buffer trop grand par rapport a l'etendue des donnees - saut du filtre position")
    # Still create position_outlier column when action="detect"
    if (action == "detect") {
      data <- data |> dplyr::mutate(position_outlier = FALSE)
    }
    return(list(data = data, count = 0))
  }

  data <- data |>
    dplyr::mutate(.is_outlier = X < x_min | X > x_max | Y < y_min | Y > y_max)

  n_anomalies <- sum(data$.is_outlier, na.rm = TRUE)

  if (action == "filter") {
    data <- data |>
      dplyr::filter(!.is_outlier) |>
      dplyr::select(-.is_outlier)
  } else if (action == "detect") {
    data <- data |>
      dplyr::rename(position_outlier = .is_outlier)
  } else {
    data <- data |>
      dplyr::select(-.is_outlier)
  }

  return(list(data = data, count = n_anomalies))
}


#' Méta-fonction d'optimisation des délais
#'
#' Cette fonction optimise les délais temporels entre les capteurs (flux et humidité)
#' en utilisant la méthode de delay adjustment (Delay Adjustment).
#'
#' @param data Tibble avec données de rendement
#' @param type Type de délai à optimiser: "flow" (flux), "moisture" (humidité),
#'   ou "both" (les deux). Défaut: "both"
#' @param method Méthode d'optimisation. Défaut: "delay_adjustment"
#' @param delay_range Plage de délais à tester en secondes. Défaut: -25:25
#' @param n_iterations Nombre d'itérations pour la stabilité. Défaut: 10
#' @param noise_level Niveau de bruit ajouté. Défaut: 0.03
#' @param apply_correction Si TRUE, applique la correction de délai aux données.
#'   Défaut: TRUE
#' @return Liste contenant:
#'   - data: Données corrigées (si apply_correction = TRUE)
#'   - delays: Délai(s) optimal(aux) trouvé(s)
#'   - delay_adjustment_results: Résultats détaillés de l'optimisation
#' @export
#' @examples
#' \dontrun{
#' # Optimiser les deux délais et appliquer les corrections
#' result <- optimize_delays(data, type = "both")
#' data_corrected <- result$data
#'
#' # Optimiser uniquement le délai de flux sans appliquer
#' result <- optimize_delays(data, type = "flow", apply_correction = FALSE)
#' print(result$delays$flow)
#' }
optimize_delays <- function(data, type = "both", method = "delay_adjustment",
                            delay_range = -25:25, n_iterations = 10,
                            noise_level = 0.03, apply_correction = TRUE) {

  if (!method %in% c("delay_adjustment")) {
    rlang::abort(paste("Méthode non supportée:", method))
  }

  types_to_process <- switch(type,
    "flow" = "flow",
    "moisture" = "moisture",
    "both" = c("flow", "moisture"),
    rlang::abort("Type doit être 'flow', 'moisture' ou 'both'")
  )

  delays <- list()
  delay_adjustment_results <- list()

  for (t in types_to_process) {
    value_col <- switch(t,
      "flow" = "Flow",
      "moisture" = "Moisture"
    )

    if (!value_col %in% names(data)) {
      rlang::warn(paste("Colonne", value_col, "non trouvée - saut de l'optimisation"))
      next
    }

    rlang::inform(paste("Optimisation du délai de", t, "..."))

    # Utiliser la fonction delay adjustment existante
    delay_adjustment_result <- apply_delay_adjustment(
      data,
      delay_range = delay_range,
      n_iterations = n_iterations,
      noise_level = noise_level,
      value_col = value_col
    )

    delays[[t]] <- delay_adjustment_result$optimal_delay
    delay_adjustment_results[[t]] <- delay_adjustment_result

    rlang::inform(paste("  Délai optimal", t, ":", delay_adjustment_result$optimal_delay, "secondes"))

    # Appliquer la correction si demandé
    if (apply_correction && isTRUE(delay_adjustment_result$optimal_delay != 0)) {
      data <- .apply_delay_correction_internal(data, delay_adjustment_result$optimal_delay, value_col)
    }
  }

  result <- list(
    delays = delays,
    delay_adjustment_results = delay_adjustment_results
  )

  if (apply_correction) {
    result$data <- data
  }

  return(result)
}

#' @noRd
.apply_delay_correction_internal <- function(data, delay, value_col) {
  if (delay == 0) return(data)

  n <- nrow(data)

  if (delay > 0) {
    # Valeur en retard : utiliser lead() pour avancer
    data[[value_col]] <- dplyr::lead(data[[value_col]], n = delay, default = NA)
  } else {
    # Valeur en avance : utiliser lag() pour reculer
    data[[value_col]] <- dplyr::lag(data[[value_col]], n = abs(delay), default = NA)
  }

  # Interpoler les NA créés par le décalage
  if (sum(is.na(data[[value_col]])) > 0) {
    data <- .interpolate_na_internal(data, value_col)
  }

  return(data)
}

#' @noRd
.interpolate_na_internal <- function(data, value_col) {
  values <- data[[value_col]]
  na_indices <- which(is.na(values))

  if (length(na_indices) == 0) return(data)

  for (idx in na_indices) {
    # Trouver la première valeur non-NA avant et après
    before <- suppressWarnings(max(which(!is.na(values[1:idx]))))
    after <- suppressWarnings(min(which(!is.na(values[idx:length(values)])))) + idx - 1

    if (!is.finite(before) || !is.finite(after)) {
      # Pas de valeurs pour interpoler - utiliser la moyenne globale
      data[[value_col]][idx] <- mean(values, na.rm = TRUE)
    } else if (before == after) {
      data[[value_col]][idx] <- values[before]
    } else {
      # Interpolation linéaire
      ratio <- (idx - before) / (after - before)
      data[[value_col]][idx] <- values[before] + ratio * (values[after] - values[before])
    }
  }

  return(data)
}


#' Méta-fonction de calcul des seuils
#'
#' Cette fonction calcule automatiquement les seuils pour le rendement, la vitesse,
#' la position et l'humidité en utilisant des méthodes statistiques robustes.
#'
#' @param data Tibble avec données de rendement
#' @param type Type de seuil à calculer. Options: "yield", "velocity", "position",
#'   "moisture", "all". Défaut: "all"
#' @param ... Paramètres de calcul:
#'   - yield: yllim (quantile bas, défaut 0.10), yulim (quantile haut, défaut 0.90),
#'           yscale (multiplicateur IQR, défaut 1.1), min_yield_abs (défaut 0)
#'   - velocity: vllim (quantile bas, défaut 0.05), vulim (quantile haut, défaut 0.95),
#'              vscale (multiplicateur IQR, défaut 1.1), min_velocity_abs (défaut 0.5)
#'   - position: gbuffer (marge en mètres, défaut 100)
#'   - moisture: n_std (nombre d'écarts-types, défaut 3)
#' @return Liste avec les seuils calculés pour chaque type demandé
#' @export
#' @examples
#' \dontrun{
#' # Calculer tous les seuils
#' thresholds <- calculate_thresholds(data)
#'
#' # Calculer uniquement les seuils de rendement personnalisés
#' thresholds <- calculate_thresholds(data, type = "yield",
#'                                   yllim = 0.05, yulim = 0.95, yscale = 1.5)
#' }
calculate_thresholds <- function(data, type = "all", ...) {
  params <- list(...)
  result <- list()

  if (identical(type, "all")) {
    type <- c("yield", "velocity", "position", "moisture")
  }

  for (t in type) {
    result[[t]] <- switch(t,
      yield = .calculate_yield_thresholds_internal(data, params),
      velocity = .calculate_velocity_thresholds_internal(data, params),
      position = .calculate_position_thresholds_internal(data, params),
      moisture = .calculate_moisture_thresholds_internal(data, params),
      {
        rlang::warn(paste("Type de seuil inconnu:", t))
        NULL
      }
    )
  }

  return(result)
}

#' @noRd
.calculate_yield_thresholds_internal <- function(data, params) {
  # Détecter la colonne de rendement
  yield_col <- NULL
  for (col in c("Yield_kg_ha", "Yield", "Flow")) {
    if (col %in% names(data)) {
      yield_col <- col
      break
    }
  }

  if (is.null(yield_col)) {
    rlang::warn("Colonne de rendement non trouvée pour le calcul des seuils")
    return(list(min_yield = 0, max_yield = Inf))
  }

  yield_vals <- data[[yield_col]]
  yield_vals <- yield_vals[is.finite(yield_vals) & yield_vals > 0]

  if (length(yield_vals) == 0) {
    return(list(min_yield = 0, max_yield = Inf))
  }

  yllim <- params$yllim %||% 0.10
  yulim <- params$yulim %||% 0.90
  yscale <- params$yscale %||% 1.1
  min_yield_abs <- params$min_yield_abs %||% 0

  q_low <- quantile(yield_vals, yllim, na.rm = TRUE)
  q_high <- quantile(yield_vals, yulim, na.rm = TRUE)
  iqr <- q_high - q_low

  min_yield <- max(min_yield_abs, q_low - yscale * iqr)
  max_yield <- q_high + yscale * iqr

  return(list(
    min_yield = min_yield,
    max_yield = max_yield,
    mean_yield = mean(yield_vals, na.rm = TRUE),
    sd_yield = stats::sd(yield_vals, na.rm = TRUE)
  ))
}

#' @noRd
.calculate_velocity_thresholds_internal <- function(data, params) {
  if (!all(c("X", "Y") %in% names(data))) {
    rlang::warn("Coordonnées X,Y non trouvées pour le calcul des seuils de vitesse")
    return(list(min_velocity = 0.5, max_velocity = 10))
  }

  # Calculate velocity - use Interval if available, otherwise assume 1 second intervals
  if ("Interval" %in% names(data)) {
    data_temp <- data |>
      dplyr::mutate(
        velocity = sqrt((X - dplyr::lag(X))^2 + (Y - dplyr::lag(Y))^2) / Interval
      )
  } else {
    data_temp <- data |>
      dplyr::mutate(
        velocity = sqrt((X - dplyr::lag(X))^2 + (Y - dplyr::lag(Y))^2)
      )
  }
  vel_vals <- data_temp$velocity[is.finite(data_temp$velocity)]

  if (length(vel_vals) == 0) {
    return(list(min_velocity = 0.5, max_velocity = 10))
  }

  vllim <- params$vllim %||% 0.05
  vulim <- params$vulim %||% 0.95
  vscale <- params$vscale %||% 1.1
  min_velocity_abs <- params$min_velocity_abs %||% 0.5

  q_low <- quantile(vel_vals, vllim, na.rm = TRUE)
  q_high <- quantile(vel_vals, vulim, na.rm = TRUE)
  iqr <- q_high - q_low

  min_velocity <- max(min_velocity_abs, q_low - vscale * iqr)
  max_velocity <- q_high + vscale * iqr

  return(list(
    min_velocity = min_velocity,
    max_velocity = max_velocity,
    mean_velocity = mean(vel_vals, na.rm = TRUE),
    sd_velocity = stats::sd(vel_vals, na.rm = TRUE)
  ))
}

#' @noRd
.calculate_position_thresholds_internal <- function(data, params) {
  if (!all(c("X", "Y") %in% names(data))) {
    return(list(buffer = params$gbuffer %||% 100))
  }

  gbuffer <- params$gbuffer %||% 100

  x_range <- range(data$X, na.rm = TRUE)
  y_range <- range(data$Y, na.rm = TRUE)

  return(list(
    min_x = x_range[1] + gbuffer,
    max_x = x_range[2] - gbuffer,
    min_y = y_range[1] + gbuffer,
    max_y = y_range[2] - gbuffer,
    buffer = gbuffer
  ))
}

#' @noRd
.calculate_moisture_thresholds_internal <- function(data, params) {
  if (!"Moisture" %in% names(data)) {
    return(NULL)
  }

  n_std <- params$n_std %||% 3
  moisture_vals <- data$Moisture[is.finite(data$Moisture)]

  if (length(moisture_vals) == 0) {
    return(NULL)
  }

  mean_moisture <- mean(moisture_vals, na.rm = TRUE)
  sd_moisture <- stats::sd(moisture_vals, na.rm = TRUE)

  return(list(
    min_moisture = mean_moisture - n_std * sd_moisture,
    max_moisture = mean_moisture + n_std * sd_moisture,
    mean_moisture = mean_moisture,
    sd_moisture = sd_moisture
  ))
}


#' Méta-fonction de conversion des coordonnées
#'
#' Convertit les coordonnées entre différents systèmes (Lat/Lon, UTM).
#'
#' @param data Tibble avec données de rendement
#' @param from Système de coordonnées source: "latlon" ou "utm"
#' @param to Système de coordonnées cible: "latlon" ou "utm"
#' @param zone Zone UTM (optionnel, auto-détectée si non fournie)
#' @return Tibble avec coordonnées converties
#' @export
#' @examples
#' \dontrun{
#' # Convertir Lat/Lon vers UTM
#' data_utm <- convert_coordinates(data, from = "latlon", to = "utm")
#'
#' # Convertir UTM vers Lat/Lon
#' data_latlon <- convert_coordinates(data, from = "utm", to = "latlon")
#' }
convert_coordinates <- function(data, from = "latlon", to = "utm", zone = NULL) {
  if (from == to) {
    return(data)
  }

  if (from == "latlon" && to == "utm") {
    return(latlon_to_utm(data, zone = zone))
  } else if (from == "utm" && to == "latlon") {
    return(.utm_to_latlon_internal(data, zone = zone))
  } else {
    rlang::abort(paste("Conversion non supportée:", from, "->", to))
  }
}

#' @noRd
.utm_to_latlon_internal <- function(data, zone = NULL) {
  if (!all(c("X", "Y") %in% names(data))) {
    rlang::abort("Colonnes X et Y requises pour la conversion UTM -> Lat/Lon")
  }

  # Détecter la zone si non fournie
  if (is.null(zone)) {
    # Utiliser la première zone trouvée dans les données
    if ("UTM_Zone" %in% names(data)) {
      zone <- data$UTM_Zone[1]
    } else {
      # Estimer la zone à partir des coordonnées
      zone <- floor((mean(data$Longitude, na.rm = TRUE) + 180) / 6) + 1
    }
  }

  # Créer un objet sf pour la conversion
  coords_sf <- sf::st_as_sf(data, coords = c("X", "Y"), crs = sf::st_crs(paste0("EPSG:326", zone)))

  # Convertir vers WGS84
  coords_latlon <- sf::st_transform(coords_sf, crs = 4326)

  # Extraire les coordonnées
  coords_matrix <- sf::st_coordinates(coords_latlon)
  data$Longitude <- coords_matrix[, 1]
  data$Latitude <- coords_matrix[, 2]

  return(data)
}


#' Méta-fonction de conversion des unités de rendement
#'
#' Convertit les unités de rendement entre différents systèmes (kg/ha, bu/acre, etc.)
#' et calcule le rendement à partir du flux.
#'
#' @param data Tibble avec données de rendement
#' @param from Unité source: "flow_lbs_s", "kg_ha", "bu_acre"
#' @param to Unité cible: "kg_ha", "bu_acre", "t_ha"
#' @param crop_type Type de culture pour la conversion (maize, soybean, wheat, etc.)
#' @param moisture_std Humidité standard pour la conversion (défaut selon culture)
#' @return Tibble avec rendement converti
#' @export
#' @examples
#' \dontrun{
#' # Convertir flux (lbs/s) vers kg/ha
#' data_yield <- convert_yield_units(data, from = "flow_lbs_s", to = "kg_ha")
#'
#' # Convertir kg/ha vers bu/acre pour du maïs
#' data_imperial <- convert_yield_units(data, from = "kg_ha", to = "bu_acre",
#'                                     crop_type = "maize")
#' }
convert_yield_units <- function(data, from = "flow_lbs_s", to = "kg_ha",
                                crop_type = NULL, moisture_std = NULL) {

  # Détecter le type de culture si non fourni
  if (is.null(crop_type) && "GrainType" %in% names(data)) {
    crop_type <- tolower(data$GrainType[1])
  }

  # Déterminer l'humidité standard selon la culture
  if (is.null(moisture_std)) {
    if (is.null(crop_type)) {
      moisture_std <- 13  # default
    } else {
      moisture_std <- switch(crop_type[1],
        "maize" = 15.5,
        "corn" = 15.5,
        "soybean" = 13,
        "wheat" = 13.5,
        "barley" = 14.8,
        "canola" = 10,
        13 # default
      )
    }
  }

  # Conversion selon le cas
  if (from == "flow_lbs_s" && to == "kg_ha") {
    data <- .convert_flow_to_yield_internal(data, moisture_std)
  } else if (from == "kg_ha" && to == "bu_acre") {
    data <- .convert_kgha_to_buacre_internal(data, crop_type)
  } else if (from == "bu_acre" && to == "kg_ha") {
    data <- .convert_buacre_to_kgha_internal(data, crop_type)
  } else if (from == "kg_ha" && to == "t_ha") {
    if ("Yield_kg_ha" %in% names(data)) {
      data$Yield_t_ha <- data$Yield_kg_ha / 1000
    }
  } else {
    rlang::warn(paste("Conversion non implémentée:", from, "->", to))
  }

  return(data)
}

#' @noRd
.convert_flow_to_yield_internal <- function(data, moisture_std = 13) {
  if (!"Flow" %in% names(data)) {
    rlang::warn("Colonne Flow non trouvée")
    return(data)
  }

  # Vérifier les colonnes requises
  required_cols <- c("Swath", "Distance", "Interval")
  missing_cols <- setdiff(required_cols, names(data))

  if (length(missing_cols) > 0) {
    rlang::warn(paste("Colonnes manquantes:", paste(missing_cols, collapse = ", ")))
    return(data)
  }

  # Conversion lbs/s -> kg/s
  flow_kg_s <- data$Flow * 0.453592

  # Conversion des unités de distance
  swath_m <- ifelse(mean(data$Swath, na.rm = TRUE) > 10,
                    data$Swath * 0.3048,  # pieds -> mètres
                    data$Swath * 0.0254)  # pouces -> mètres

  distance_m <- ifelse(mean(data$Distance, na.rm = TRUE) > 1,
                       data$Distance * 0.3048,  # pieds -> mètres
                       data$Distance * 0.0254)  # pouces -> mètres

  # Calcul du rendement humide en kg/ha
  # Rendement = Flux (kg/s) / (Swath (m) * Distance (m) / Interval (s)) * 10000 m²/ha
  data$Yield_kg_ha_wet <- (flow_kg_s * data$Interval * 10000) / (swath_m * distance_m)

  # Conversion vers rendement sec si humidité disponible
  if ("Moisture" %in% names(data)) {
    moisture_factor <- 100 - moisture_std
    data$Yield_kg_ha <- data$Yield_kg_ha_wet * (100 - data$Moisture) / moisture_factor
  } else {
    data$Yield_kg_ha <- data$Yield_kg_ha_wet
  }

  return(data)
}

#' @noRd
.convert_kgha_to_buacre_internal <- function(data, crop_type = "maize") {
  if (!"Yield_kg_ha" %in% names(data)) {
    rlang::warn("Colonne Yield_kg_ha non trouvée")
    return(data)
  }

  # Facteurs de conversion kg/ha -> bu/acre
  conversion_factor <- switch(crop_type,
    "maize" = 0.0159,    # 1 kg/ha = 0.0159 bu/acre (maïs @ 15.5%)
    "corn" = 0.0159,
    "soybean" = 0.0149,  # 1 kg/ha = 0.0149 bu/acre (soja @ 13%)
    "wheat" = 0.0149,    # 1 kg/ha = 0.0149 bu/acre (blé @ 13.5%)
    0.015 # défaut
  )

  data$Yield_bu_acre <- data$Yield_kg_ha * conversion_factor
  return(data)
}

#' @noRd
.convert_buacre_to_kgha_internal <- function(data, crop_type = "maize") {
  if (!"Yield_bu_acre" %in% names(data)) {
    rlang::warn("Colonne Yield_bu_acre non trouvée")
    return(data)
  }

  # Facteurs de conversion bu/acre -> kg/ha
  conversion_factor <- switch(crop_type,
    "maize" = 62.77,     # 1 bu/acre = 62.77 kg/ha (maïs @ 15.5%)
    "corn" = 62.77,
    "soybean" = 67.25,   # 1 bu/acre = 67.25 kg/ha (soja @ 13%)
    "wheat" = 67.25,     # 1 bu/acre = 67.25 kg/ha (blé @ 13.5%)
    65 # défaut
  )

  data$Yield_kg_ha <- data$Yield_bu_acre * conversion_factor
  return(data)
}


#' Méta-fonction d'anonymisation des données
#'
#' Anonymise les données de rendement en supprimant les informations sensibles
#' et/ou en transformant les coordonnées géographiques.
#'
#' @param data Tibble avec données de rendement
#' @param type Type d'anonymisation: "coordinates" (décalage coordonnées),
#'   "attributes" (suppression attributs sensibles), "full" (les deux).
#'   Défaut: "full"
#' @param method Méthode d'anonymisation des coordonnées: "translation"
#'   (décalage aléatoire), "rotation" (rotation), "noise" (bruit).
#'   Défaut: "translation"
#' @param preserve_structure Si TRUE, conserve la structure spatiale relative.
#'   Défaut: TRUE
#' @param ... Paramètres supplémentaires pour l'anonymisation
#' @return Données anonymisées
#' @export
#' @examples
#' \dontrun{
#' # Anonymisation complète
#' data_anon <- anonymize_data(data, type = "full")
#'
#' # Anonymiser uniquement les coordonnées avec méthode de rotation
#' data_anon <- anonymize_data(data, type = "coordinates", method = "rotation")
#'
#' # Anonymiser uniquement les attributs sensibles
#' data_anon <- anonymize_data(data, type = "attributes")
#' }
anonymize_data <- function(data, type = "full", method = "translation",
                           preserve_structure = TRUE, ...) {
  params <- list(...)

  if (type %in% c("full", "coordinates")) {
    data <- .anonymize_coordinates_internal(data, method, preserve_structure, params)
  }

  if (type %in% c("full", "attributes")) {
    data <- .anonymize_attributes_internal(data, params)
  }

  return(data)
}

#' @noRd
.anonymize_coordinates_internal <- function(data, method = "translation",
                                            preserve_structure = TRUE, params) {
  if (!all(c("X", "Y") %in% names(data))) {
    rlang::warn("Coordonnées X,Y non trouvées - saut de l'anonymisation spatiale")
    return(data)
  }

  # Générer une transformation aléatoire
  set.seed(params$seed %||% sample(1:10000, 1))

  if (method == "translation") {
    # Décalage aléatoire
    dx <- runif(1, -10000, 10000)
    dy <- runif(1, -10000, 10000)

    data$X <- data$X + dx
    data$Y <- data$Y + dy

    if ("Longitude" %in% names(data) && "Latitude" %in% names(data)) {
      # Convertir le décalage UTM en décalage Lat/Lon approximatif
      data$Longitude <- data$Longitude + dx / 111320
      data$Latitude <- data$Latitude + dy / 111320
    }

    attr(data, "anonymization") <- list(method = "translation", dx = dx, dy = dy)

  } else if (method == "rotation") {
    # Rotation autour du centroïde
    angle <- runif(1, 0, 2 * pi)
    centroid_x <- mean(data$X, na.rm = TRUE)
    centroid_y <- mean(data$Y, na.rm = TRUE)

    # Translation au centre
    x_centered <- data$X - centroid_x
    y_centered <- data$Y - centroid_y

    # Rotation
    data$X <- centroid_x + x_centered * cos(angle) - y_centered * sin(angle)
    data$Y <- centroid_y + x_centered * sin(angle) + y_centered * cos(angle)

    attr(data, "anonymization") <- list(method = "rotation", angle = angle,
                                        centroid = c(centroid_x, centroid_y))
  }

  return(data)
}

#' @noRd
.anonymize_attributes_internal <- function(data, params) {
  # Liste des colonnes sensibles à supprimer
  sensitive_patterns <- c(
    "Operator", "Field", "Farm", "Client", "Company", "Name",
    "Vehicle", "Equipment", "Serial", "Device", "Phone", "Email"
  )

  # Détecter les colonnes sensibles
  cols_to_remove <- c()
  for (pattern in sensitive_patterns) {
    matches <- grep(pattern, names(data), ignore.case = TRUE, value = TRUE)
    cols_to_remove <- c(cols_to_remove, matches)
  }

  # Colonnes spécifiques fournies par l'utilisateur
  if (!is.null(params$remove_cols)) {
    cols_to_remove <- c(cols_to_remove, params$remove_cols)
  }

  # Supprimer les doublons
  cols_to_remove <- unique(cols_to_remove)
  cols_to_remove <- intersect(cols_to_remove, names(data))

  if (length(cols_to_remove) > 0) {
    rlang::inform(paste("Colonnes sensibles supprimées:", paste(cols_to_remove, collapse = ", ")))
    data <- data[, !(names(data) %in% cols_to_remove)]
  }

  return(data)
}


#' Méta-fonction d'export des données
#'
#' Exporte les données nettoyées dans différents formats (CSV, GeoJSON, Shapefile,
#' raster, etc.).
#'
#' @param data Données à exporter (tibble ou objet sf)
#' @param file Chemin du fichier de sortie
#' @param format Format d'export: "csv", "geojson", "shp", "gpkg", "raster".
#'   Si NULL, détecté automatiquement à partir de l'extension du fichier.
#' @param ... Paramètres supplémentaires pour l'export
#' @return Chemin du fichier créé (invisible)
#' @export
#' @examples
#' \dontrun{
#' # Export CSV
#' export_data(data, "output.csv", format = "csv")
#'
#' # Export GeoJSON (format auto-détecté)
#' export_data(data, "output.geojson")
#'
#' # Export Shapefile avec options
#' export_data(data, "output.shp", format = "shp", overwrite = TRUE)
#'
#' # Export raster
#' export_data(data, "yield.tif", format = "raster", resolution = 5)
#' }
export_data <- function(data, file, format = NULL, ...) {
  params <- list(...)

  # Détecter le format à partir de l'extension si non fourni
  if (is.null(format)) {
    ext <- tolower(tools::file_ext(file))
    format <- switch(ext,
      "csv" = "csv",
      "geojson" = "geojson",
      "json" = "geojson",
      "shp" = "shp",
      "gpkg" = "gpkg",
      "tif" = "raster",
      "tiff" = "raster",
      rlang::abort(paste("Extension non reconnue:", ext, "- spécifiez le format"))
    )
  }

  format <- tolower(format)

  # Vérifier si le fichier existe déjà
  if (file.exists(file) && !isTRUE(params$overwrite)) {
    rlang::abort(paste("Le fichier existe déjà:", file, "- utilisez overwrite = TRUE"))
  }

  # Créer le répertoire parent si nécessaire
  dir.create(dirname(file), showWarnings = FALSE, recursive = TRUE)

  # Export selon le format
  switch(format,
    "csv" = .export_csv_internal(data, file, params),
    "geojson" = .export_geojson_internal(data, file, params),
    "shp" = .export_shapefile_internal(data, file, params),
    "gpkg" = .export_geopackage_internal(data, file, params),
    "raster" = .export_raster_internal(data, file, params),
    rlang::abort(paste("Format non supporté:", format))
  )

  rlang::inform(paste("Données exportées vers:", file))
  return(invisible(file))
}
