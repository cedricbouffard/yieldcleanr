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
 #' @param max_heading_change Variation maximale de direction entre 3 points consecutifs (degrés, défaut: 60)
 #' @param window_size Taille de la fenetre pour detecter les anomalies (defaut: 3)
 #' @return Liste avec data (donnees filtrees) et removed (points supprimes)
 #' @export
  filter_heading_anomalies <- function(data, max_heading_change = 60, window_size = 3) {
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


 #' Filtre de position pour eliminer les points hors champ
 #'
 #' Detecte et supprime les points qui sont en dehors du champ principal
 #' en utilisant une methode de buffer autour du centre du champ.
 #' Seuls les points dans les zones avec suffisamment de points voisins sont conserves.
 #'
 #' @param data Tibble avec au minimum X, Y
 #' @param buffer_radius Rayon du buffer en metres (defaut: 50)
 #' @param min_points_cell Nombre minimum de points par cellule pour qu'une zone soit valide (defaut: 5)
 #' @param grid_size Taille de la grille pour l'analyse en metres (defaut: 20)
 #' @return Liste avec data (donnees filtrees) et removed (points supprimes)
 #' @export
 filter_position_outliers <- function(data, buffer_radius = 50, min_points_cell = 5, grid_size = 20) {
   if (!all(c("X", "Y") %in% names(data))) {
     rlang::warn("Colonnes X ou Y manquantes - saut du filtre de position")
     return(list(data = data, removed = data[0, ]))
   }
   
   n_before <- nrow(data)
   
   # Creer une grille reguliere
   x_range <- range(data$X, na.rm = TRUE)
   y_range <- range(data$Y, na.rm = TRUE)
   
   # Creer les cellules de grille
   x_breaks <- seq(x_range[1], x_range[2], by = grid_size)
   y_breaks <- seq(y_range[1], y_range[2], by = grid_size)
   
   if (length(x_breaks) < 2 || length(y_breaks) < 2) {
     rlang::warn("Plage de coordonnees trop petite pour le filtre de position")
     return(list(data = data, removed = data[0, ]))
   }
   
   # Assigner chaque point a une cellule
   data$grid_x <- findInterval(data$X, x_breaks)
   data$grid_y <- findInterval(data$Y, y_breaks)
   data$grid_id <- paste(data$grid_x, data$grid_y, sep = "_")
   
   # Calculer le nombre de points par cellule
   grid_counts <- table(data$grid_id)
   
   # Identifier les cellules valides (avec au moins min_points_cell points)
   valid_cells <- names(grid_counts)[grid_counts >= min_points_cell]
   
   # Si aucune cellule valide, utiliser les cellules avec le plus de points
   if (length(valid_cells) == 0) {
     # Prendre les cellules avec le plus de points (top 80%)
     sorted_counts <- sort(grid_counts, decreasing = TRUE)
     n_cells_to_keep <- max(1, ceiling(length(sorted_counts) * 0.8))
     valid_cells <- names(sorted_counts)[1:n_cells_to_keep]
   }
   
   # Pour chaque point, verifier s'il est dans une cellule valide
   data$is_valid <- data$grid_id %in% valid_cells
   
   # Pour les points non valides, verifier s'ils sont dans le buffer d'une cellule valide
   # Utiliser une approche vectorisee plus rapide
   if (buffer_radius > 0 && length(valid_cells) > 0) {
     # Extraire les coordonnees des points valides
     valid_idx <- which(data$is_valid)
     
     if (length(valid_idx) > 0) {
       # Pour chaque point non valide, calculer la distance minimale aux points valides
       invalid_idx <- which(!data$is_valid)
       
       # Calcul vectorise: pour chaque point invalide, distance minimale a un point valide
       min_dists <- sapply(invalid_idx, function(i) {
         min(sqrt((data$X[i] - data$X[valid_idx])^2 + 
                  (data$Y[i] - data$Y[valid_idx])^2), na.rm = TRUE)
       })
       
       # Marquer comme valide si dans le buffer
       data$is_valid[invalid_idx] <- min_dists <= buffer_radius
     }
   }
   
   # Filtrer les donnees
   to_keep <- data |>
     dplyr::filter(is_valid) |>
     dplyr::select(-grid_x, -grid_y, -grid_id, -is_valid)
   
   to_remove <- data |>
     dplyr::filter(!is_valid) |>
     dplyr::select(-grid_x, -grid_y, -grid_id, -is_valid)
   
   n_removed <- n_before - nrow(to_keep)
   if (n_removed > 0) {
     rlang::inform(paste("Filtre position:", n_removed, "points hors champ elimines (", 
                         round(n_removed / n_before * 100, 1), "%)"))
   }
   
   return(list(data = to_keep, removed = to_remove))
 }
