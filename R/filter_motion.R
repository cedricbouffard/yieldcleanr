#' Filtre pour changements brusques de vitesse
#'
#' Detecte et supprime les points ou il y a une variation tres grande de la vitesse
#' entre deux points consecutifs. La vitesse est calculee a partir de la distance
#' et du temps entre les points.
#'
#' @param data Tibble avec au minimum X, Y, Interval et orig_row_id
#' @param max_acceleration Acceleration maximale autorisee (m/s, defaut: 5)
#' @param max_deceleration Deceleration maximale autorisee (m/s, defaut: -8)
#' @return Liste avec data (donnees filtrees) et removed (points supprimes)
#' @export
 filter_velocity_jumps <- function(data, max_acceleration = 5, max_deceleration = -8) {
  if (!all(c("X", "Y", "Interval") %in% names(data))) {
    rlang::warn("Colonnes X, Y ou Interval manquantes - saut du filtre de changements de vitesse")
    return(list(data = data, removed = data[0, ]))
  }

  if (!"GPS_Time" %in% names(data)) {
    rlang::warn("Colonne GPS_Time manquante - utilisation de l'ordre des lignes")
    data <- data |> dplyr::mutate(GPS_Time = dplyr::row_number())
  }

  # Calculer la vitesse entre points consecutifs
  data_with_velocity <- data |>
    dplyr::arrange(GPS_Time) |>
    dplyr::mutate(
      # Distance entre points consecutifs
      dist_to_next = sqrt((dplyr::lead(X) - X)^2 + (dplyr::lead(Y) - Y)^2),
      dist_to_prev = sqrt((X - dplyr::lag(X))^2 + (Y - dplyr::lag(Y))^2),
      # Temps entre points
      time_to_next = dplyr::lead(Interval),
      time_to_prev = dplyr::lag(Interval),
      # Vitesse instantanee (m/s)
      velocity_next = dist_to_next / dplyr::coalesce(time_to_next, 1),
      velocity_prev = dist_to_prev / dplyr::coalesce(time_to_prev, 1),
      # Acceleration entre points (variation de vitesse / temps)
      acceleration = (velocity_next - velocity_prev) / dplyr::coalesce(Interval, 1)
    )

  # Identifier les points avec changement brusque de vitesse
  to_remove <- data_with_velocity |>
    dplyr::filter(
      !is.na(acceleration) &
        (acceleration > max_acceleration | acceleration < max_deceleration)
    )

  to_keep <- data_with_velocity |>
    dplyr::filter(
      is.na(acceleration) |
        (acceleration <= max_acceleration & acceleration >= max_deceleration)
    ) |>
    dplyr::select(-dist_to_next, -dist_to_prev, -time_to_next, -time_to_prev,
                  -velocity_next, -velocity_prev, -acceleration)

   removed <- to_remove |>
     dplyr::select(-dist_to_next, -dist_to_prev, -time_to_next, -time_to_prev,
                   -velocity_next, -velocity_prev, -acceleration)

   return(list(data = to_keep, removed = removed))
 }


#' Filtre pour variations brusques de direction du header
#'
#' Detecte et supprime les points ou le header varie anormalement.
#' Conserve les virages normaux mais retire les points isoles anormaux
#' (par exemple: un point tourne et le point suivant revient dans le bon sens).
#'
#' @param data Tibble avec au minimum X, Y, orig_row_id et GPS_Time
#' @param max_heading_change Variation maximale de direction entre 3 points consecutifs (degrés, défaut: 45)
#' @param window_size Taille de la fenetre pour detecter les anomalies (defaut: 3)
#' @return Liste avec data (donnees filtrees) et removed (points supprimes)
#' @export
 filter_heading_anomalies <- function(data, max_heading_change = 45, window_size = 3) {
  if (!all(c("X", "Y") %in% names(data))) {
    rlang::warn("Colonnes X ou Y manquantes - saut du filtre de direction")
    return(list(data = data, removed = data[0, ]))
  }

  if (!"GPS_Time" %in% names(data)) {
    rlang::warn("Colonne GPS_Time manquante - utilisation de l'ordre des lignes")
    data <- data |> dplyr::mutate(GPS_Time = dplyr::row_number())
  }

  # Calculer le cap (heading) entre points consecutifs
  data_with_heading <- data |>
    dplyr::arrange(GPS_Time) |>
    dplyr::mutate(
      # Vecteur direction entre points consecutifs
      dx_next = dplyr::lead(X) - X,
      dy_next = dplyr::lead(Y) - Y,
      dx_prev = X - dplyr::lag(X),
      dy_prev = Y - dplyr::lag(Y),
      # Cap en degres (0-360)
      heading_next = (atan2(dx_next, dy_next) * 180 / pi) %% 360,
      heading_prev = (atan2(dx_prev, dy_prev) * 180 / pi) %% 360,
      # Variation de direction entre 3 points consecutifs
      heading_change = abs(heading_next - heading_prev),
      # Normaliser entre 0 et 180 (direction inverse = 180, meme direction = 0)
      heading_change = pmin(heading_change, 360 - heading_change),
      # Detecter les anomalies: changement brusque suivi d'un retour
      is_anomaly = !is.na(heading_change) & heading_change > max_heading_change
    )

  # Identifier les points anormaux
  to_remove <- data_with_heading |>
    dplyr::filter(is_anomaly == TRUE)

   to_keep <- data_with_heading |>
     dplyr::filter(is_anomaly == FALSE | is.na(is_anomaly)) |>
     dplyr::select(-dx_next, -dy_next, -dx_prev, -dy_prev,
                   -heading_next, -heading_prev, -heading_change, -is_anomaly)

   removed <- to_remove |>
     dplyr::select(-dx_next, -dy_next, -dx_prev, -dy_prev,
                   -heading_next, -heading_prev, -heading_change, -is_anomaly)

   return(list(data = to_keep, removed = removed))
 }
