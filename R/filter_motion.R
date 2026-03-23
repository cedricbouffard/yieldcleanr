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
 #' @noRd
 #' @keywords internal
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
#' Detecte et supprime les points GPS isoles ou le header varie anormalement.
#' Conserve les virages normaux en bout de rang (demi-tours) mais retire les
#' points erratiques isoles (par exemple: un point GPS qui "saute" puis revient).
#'
#' Le filtre utilise deux criteres combines:
#' \itemize{
#'   \item Critere d'isolement: le point est anormal uniquement si ses voisins
#'     (prev et next) sont alignes entre eux. Si les voisins aussi changent de
#'     direction, c'est un virage legitime (bout de rang).
#'   \item Critere de distance: seuls les points proches de leurs voisins sont
#'     consideres (distance < seuil). Les transitions en bout de rang ont une
#'     grande distance inter-point et ne sont pas des anomalies GPS.
#' }
#'
#' @param data Tibble avec au minimum X, Y, orig_row_id et GPS_Time
#' @param max_heading_change Variation maximale de direction entre 3 points
#'   consecutifs (degres, defaut: 60)
#' @param window_size Taille de la fenetre pour detecter les anomalies (defaut: 3)
#' @param min_isolation_angle Angle entre prev et next au-dessus duquel le
#'   changement de direction est considere comme un virage coherent et non une
#'   anomalie (degres, defaut: 30). Si skip_vs_prev > min_isolation_angle,
#'   le point est un virage legitime. Un GPS glitch a skip_vs_prev < 30.
#' @param max_neighbor_dist Distance maximale (metres) aux voisins pour
#'   considerer le point comme une anomalie GPS. Au-dela, c'est probablement
#'   un demi-tour en bout de rang (defaut: NULL = calcule automatiquement
#'   a partir de la distance mediane * 3).
#' @return Liste avec data (donnees filtrees) et removed (points supprimes)
#' @noRd
#' @keywords internal
filter_heading_anomalies <- function(data, max_heading_change = 60, window_size = 3,
                                     min_isolation_angle = NULL,
                                     max_neighbor_dist = NULL) {
  if (!all(c("X", "Y") %in% names(data))) {
    rlang::warn("Colonnes X ou Y manquantes - saut du filtre de direction")
    return(list(data = data, removed = data[0, ]))
  }

  if (!"GPS_Time" %in% names(data)) {
    rlang::warn("Colonne GPS_Time manquante - utilisation de l'ordre des lignes")
    data <- data |> dplyr::mutate(GPS_Time = dplyr::row_number())
  }

  # Seuil d'isolement: un GPS glitch a des voisins bien alignes (skip_vs_prev petit).
  # Un virage legitime a des voisins opposes (skip_vs_prev grand).
  # On utilise un seuil fixe de 30 deg par defaut (les voisins doivent etre
  # reellement bien alignes pour considerer le point comme un outlier isole).
  if (is.null(min_isolation_angle)) {
    min_isolation_angle <- 30
  }

  # Calculer le cap et les distances entre points consecutifs
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
      # --- Correction des flips du Heading capteur ---
      # Le capteur GPS indique parfois un changement de cap de 180° qui ne
      # correspond pas a un vrai virage. On detecte ces flips et on corrige.
      # Un flip = h_capteur change de ~180° entre points consecutifs
      # Si le heading recalcule depuis X,Y ne change pas (trajectoire droite),
      # alors c'est un faux flip du capteur.
      h_capteur = if ("Heading" %in% names(data)) Heading else NA_real_,
      h_capteur_diff = abs(h_capteur - dplyr::lag(h_capteur)),
      h_capteur_diff = pmin(h_capteur_diff, 360 - h_capteur_diff),
      h_capteur_lead_diff = abs(h_capteur - dplyr::lead(h_capteur)),
      h_capteur_lead_diff = pmin(h_capteur_lead_diff, 360 - h_capteur_lead_diff),
      # Correction: si flip du capteur (~180°) ET trajectoire droite, corriger
      h_capteur_is_flip = !is.na(h_capteur_diff) & h_capteur_diff > 90,
      h_is_straight = !is.na(heading_change) & heading_change < 30,
      h_neighbor_straight = !is.na(dplyr::lead(heading_change)) & dplyr::lead(heading_change) < 30,
      needs_correction = h_capteur_is_flip & h_is_straight & h_neighbor_straight,
      # Corriger: ajouter 180° pour ramener le cap dans le bon quadrant
      h_capteur_corrected = dplyr::if_else(
        needs_correction,
        (h_capteur + 180) %% 360,
        h_capteur
      ),
      # Distances aux voisins
      dist_prev = sqrt(dx_prev^2 + dy_prev^2),
      dist_next = sqrt(dx_next^2 + dy_next^2),
      dist_max_neighbor = pmax(dist_prev, dist_next, na.rm = TRUE),
      # Cap direct entre le point precedent et le point suivant (sans passer par le point courant)
      # Si ce cap est coherent avec heading_prev, le point courant est un outlier isole
      # Si ce cap differe aussi, c'est un vrai virage
      dx_skip = dplyr::lead(X) - dplyr::lag(X),
      dy_skip = dplyr::lead(Y) - dplyr::lag(Y),
      heading_skip = (atan2(dx_skip, dy_skip) * 180 / pi) %% 360,
      # Difference entre heading_prev (direction avant le point) et heading_skip (direction prev->next)
      skip_vs_prev = abs(heading_skip - heading_prev),
      skip_vs_prev = pmin(skip_vs_prev, 360 - skip_vs_prev)
    )

  # Calculer le seuil de distance automatiquement si non fourni
  if (is.null(max_neighbor_dist)) {
    median_dist <- stats::median(
      c(data_with_heading$dist_prev, data_with_heading$dist_next),
      na.rm = TRUE
    )
    max_neighbor_dist <- median_dist * 3
  }

  # Detecter les anomalies avec les criteres combines
  data_with_heading <- data_with_heading |>
    dplyr::mutate(
      # Critere 1: changement de direction anormal
      has_heading_change = !is.na(heading_change) & heading_change > max_heading_change,
      # Critere 2: point isole - les voisins sont alignes entre eux
      # (si skip_vs_prev est faible, prev et next sont dans la meme direction
      #  donc le point courant est un outlier qui "sort" de la trajectoire)
      is_isolated = !is.na(skip_vs_prev) & skip_vs_prev < min_isolation_angle,
      # Critere 3: distance aux voisins courte (pas un demi-tour en bout de rang)
      is_close = !is.na(dist_max_neighbor) & dist_max_neighbor < max_neighbor_dist,
      # Anomalie = les 3 criteres reunis
      is_anomaly = has_heading_change & is_isolated & is_close
    )

  # Colonnes temporaires a nettoyer
  temp_cols <- c("dx_next", "dy_next", "dx_prev", "dy_prev",
                 "heading_next", "heading_prev", "heading_change",
                 "dist_prev", "dist_next", "dist_max_neighbor",
                 "dx_skip", "dy_skip", "heading_skip",
                 "skip_vs_prev", "has_heading_change",
                 "is_isolated", "is_close", "is_anomaly")

  # Identifier les points anormaux
  to_remove <- data_with_heading |>
    dplyr::filter(is_anomaly == TRUE)

  to_keep <- data_with_heading |>
    dplyr::filter(is_anomaly == FALSE | is.na(is_anomaly)) |>
    dplyr::select(-dplyr::any_of(temp_cols))

  removed <- to_remove |>
    dplyr::select(-dplyr::any_of(temp_cols))

  return(list(data = to_keep, removed = removed))
}


 #' Filtre de position pour eliminer les points hors champ
 #'
 #' Detecte et supprime les points qui sont en dehors du champ principal
 #' en utilisant DBSCAN pour identifier le cluster principal et eliminer
 #' les points isoles ou dans des petits clusters.
 #'
 #' @param data Tibble avec au minimum X, Y
 #' @param eps Rayon epsilon pour DBSCAN en metres (defaut: 15)
 #' @param min_pts Nombre minimum de points pour former un cluster (defaut: 10)
 #' @param min_cluster_pct Pourcentage minimum du total pour garder un cluster (defaut: 0.5%)
 #' @return Liste avec data (donnees filtrees) et removed (points supprimes)
 #' @noRd
 #' @keywords internal
  filter_position_outliers <- function(data, eps = 15, min_pts = 10, min_cluster_pct = 0.5) {
   if (!all(c("X", "Y") %in% names(data))) {
     rlang::warn("Colonnes X ou Y manquantes - saut du filtre de position")
     return(list(data = data, removed = data[0, ]))
   }
   
   n_before <- nrow(data)
   
   if (n_before < min_pts * 2) {
     rlang::warn("Pas assez de points pour le filtre DBSCAN")
     return(list(data = data, removed = data[0, ]))
   }
   
   # ============================================
   # DBSCAN clustering
   # ============================================
   coords <- as.matrix(data[, c("X", "Y")])
   
   # Executer DBSCAN
   db_result <- dbscan::dbscan(coords, eps = eps, minPts = min_pts)
   
   data$cluster <- db_result$cluster
   
   # Cluster 0 = bruit (points isoles) - toujours supprimer
   # Autres clusters: garder seulement ceux avec assez de points
   
   cluster_counts <- table(data$cluster)
   
   # Calculer le seuil minimum de points pour un cluster valide
   min_cluster_size <- max(min_pts, ceiling(n_before * min_cluster_pct / 100))
   
   # Identifier les clusters valides (assez grands, excluant le bruit cluster 0)
   valid_clusters <- as.integer(names(cluster_counts)[
     cluster_counts >= min_cluster_size & names(cluster_counts) != "0"
   ])
   
   # Si aucun cluster valide, prendre le plus grand cluster non-bruit
   if (length(valid_clusters) == 0) {
     non_noise_clusters <- cluster_counts[names(cluster_counts) != "0"]
     if (length(non_noise_clusters) > 0) {
       valid_clusters <- as.integer(names(non_noise_clusters)[which.max(non_noise_clusters)])
     }
   }
   
   # Marquer les points valides
   data$is_valid <- data$cluster %in% valid_clusters
   
   # ============================================
   # Optionnel: Recuperer les points de bruit tres proches du cluster principal
   # (pour ne pas perdre des points legitimes aux bordures)
   # ============================================
   noise_idx <- which(data$cluster == 0)
   valid_idx <- which(data$is_valid)
   
   if (length(noise_idx) > 0 && length(valid_idx) > 0) {
     # Pour chaque point de bruit, verifier s'il est tres proche d'un point valide
     recovery_radius <- eps / 2  # Rayon plus strict pour la recuperation
     
     for (i in noise_idx) {
       min_dist <- min(sqrt((data$X[i] - data$X[valid_idx])^2 + 
                            (data$Y[i] - data$Y[valid_idx])^2), na.rm = TRUE)
       if (min_dist <= recovery_radius) {
         data$is_valid[i] <- TRUE
       }
     }
   }
   
   # Nettoyer
   to_keep <- data |>
     dplyr::filter(is_valid) |>
     dplyr::select(-cluster, -is_valid)
   
   to_remove <- data |>
     dplyr::filter(!is_valid) |>
     dplyr::select(-cluster, -is_valid)
   
   n_removed <- n_before - nrow(to_keep)
   if (n_removed > 0) {
     n_noise <- sum(data$cluster == 0)
     n_small_clusters <- n_removed - n_noise
     rlang::inform(paste("Filtre position DBSCAN:", n_removed, "points hors champ elimines (",
                         round(n_removed / n_before * 100, 1), "%) -",
                         n_noise, "bruit,", n_small_clusters, "petits clusters"))
   }
   
   return(list(data = to_keep, removed = to_remove))
 }
