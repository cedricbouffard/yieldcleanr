#' AYCE : Auto Yield Cleaning Engine
#'
#' Systeme expert automatise pour le nettoyage des donnees de rendement
#' sans intervention humaine ou avec intervention minimale.
#'
#' @name ayce
#' @aliases ayce_clean calculate_auto_thresholds apply_pcdi
#' @noRd
"_PACKAGE"


# ============================================================================== 
# AYCE - AUTO YIELD CLEANING ENGINE
# Implementation basee sur la methode USDA Yield Editor AYCE
# ============================================================================== 

#' Calculer le RSC (Relative Spatial Coherence) pour la methode PCDI
#'
#' Mesure la coherence spatiale relative du rendement pour optimiser les delais.
#' Plus le RSC est eleve, meilleur est l'alignement spatial.
#'
#' @param x Vecteur des coordonnees X
#' @param y Vecteur des coordonnees Y
#' @param yield Vecteur des valeurs de rendement
#' @return Valeur RSC entre 0 et 1
#' @noRd
calculate_rsc <- function(x, y, yield) {
  if (length(x) < 10 || length(unique(x)) < 3 || length(unique(y)) < 3) {
    return(0)
  }

  # Creer la grille et binariser les valeurs
  n_bins <- 50
  x_bins <- quantile(x, probs = seq(0, 1, length.out = n_bins))
  y_bins <- quantile(y, probs = seq(0, 1, length.out = n_bins))

  # Calculer les moyennes par cellule
  grid_yield <- matrix(NA, nrow = n_bins - 1, ncol = n_bins - 1)
  count_grid <- matrix(0, nrow = n_bins - 1, ncol = n_bins - 1)

  for (i in seq_along(x)) {
    xi <- findInterval(x[i], x_bins)
    yi <- findInterval(y[i], y_bins)
    if (xi > 0 && xi < n_bins && yi > 0 && yi < n_bins) {
      grid_yield[yi, xi] <- ifelse(is.na(grid_yield[yi, xi]),
                                    yield[i],
                                    (grid_yield[yi, xi] * count_grid[yi, xi] + yield[i]) / (count_grid[yi, xi] + 1))
      count_grid[yi, xi] <- count_grid[yi, xi] + 1
    }
  }

  # Calculer la structure spatiale - ratio variance intra/total
  valid_cells <- !is.na(grid_yield) & count_grid > 2
  if (sum(valid_cells) < 10) return(0)

  cell_means <- grid_yield[valid_cells]
  global_mean <- mean(yield, na.rm = TRUE)

  # Coherence spatiale = 1 - (variance intra / variance totale)
  within_var <- mean((cell_means - global_mean)^2, na.rm = TRUE)
  total_var <- var(yield, na.rm = TRUE)

  if (total_var == 0) return(0)
  rsc <- 1 - within_var / total_var

  return(max(0, min(1, rsc)))
}


#' Echantillonnage par grille spatiale
#'
#' Echantillonne les points en utilisant une grille pour reduire la taille
#' tout en preservant la distribution spatiale.
#'
#' @param data Data frame avec colonnes X, Y
#' @param max_points Nombre maximum de points a conserver
#' @param cellsize Taille des cellules de grille en metres
#' @return Data frame echantillonne
#' @noRd
grid_sample_data <- function(data, max_points = 10000, cellsize = 100, seed = 42) {
  n <- nrow(data)
  if (n <= max_points) {
    return(data)
  }
  
  # Creer une grille
  x_range <- range(data$X, na.rm = TRUE)
  y_range <- range(data$Y, na.rm = TRUE)
  
  nx <- ceiling(diff(x_range) / cellsize)
  ny <- ceiling(diff(y_range) / cellsize)
  
  # Assigner chaque point a une cellule
  data$grid_x <- floor((data$X - x_range[1]) / cellsize)
  data$grid_y <- floor((data$Y - y_range[1]) / cellsize)
  data$grid_id <- data$grid_x * ny + data$grid_y
  
  # Compter les points par cellule
  grid_counts <- table(data$grid_id)
  
  # Melanger les cellules avec le seed specifie
  set.seed(seed)
  grid_ids <- as.numeric(names(grid_counts))
  grid_ids <- sample(grid_ids)
  
  # Selectionner les cellules jusqu'a atteindre max_points
  cumsum_count <- cumsum(as.numeric(grid_counts[as.character(grid_ids)]))
  selected_grids <- grid_ids[cumsum_count <= max_points]
  
  # Si on n'a pas assez, prendre toutes les cellules
  if (length(selected_grids) == 0) {
    selected_grids <- grid_ids
  }
  
  # Filtrer les donnees
  result <- data[data$grid_id %in% selected_grids, ]
  
  # Nettoyer les colonnes temporaires
  result <- result[, !names(result) %in% c("grid_x", "grid_y", "grid_id")]
  
  return(result)
}


#' Calculer l'Indice de Moran avec rgeoda (rapide)
#'
#' Mesure l'autocorrelation spatiale en utilisant rgeoda pour la performance.
#' Plus l'indice de Moran est eleve, meilleur est l'alignement spatial.
#'
#' @param x Vecteur des coordonnees X
#' @param y Vecteur des coordonnees Y
#' @param value Vecteur des valeurs
#' @param bandwidth Distance de bande passante pour les poids spatiaux
#' @return Indice de Moran entre -1 et 1
#' @noRd
calculate_moran_fast <- function(x, y, value, bandwidth = 30, use_rgeoda = FALSE) {
  n <- length(x)
  if (n < 10 || length(unique(x)) < 3 || length(unique(y)) < 3) {
    return(0)
  }

  # Supprimer les NA
  valid <- !is.na(x) & !is.na(y) & !is.na(value)
  if (sum(valid) < 10) return(0)

  x <- x[valid]
  y <- y[valid]
  value <- value[valid]
  n <- length(value)
  
  # Par defaut, utiliser la methode legacy (plus stable)
  # rgeoda peut causer des segfaults avec des appels repetes
  if (!use_rgeoda || !requireNamespace("rgeoda", quietly = TRUE)) {
    return(calculate_moran_legacy(x, y, value, k = min(10, n %/% 10)))
  }

  tryCatch({
    # Creer un objet sf pour rgeoda
    coords <- data.frame(x = x, y = y, value = value)
    sf_obj <- sf::st_as_sf(coords, coords = c("x", "y"), crs = NA)
    
    # Calculer les poids de distance
    dist_w <- rgeoda::distance_weights(sf_obj, bandwidth)
    
    # Calculer le lag spatial
    lag_result <- rgeoda::spatial_lag(dist_w, data.frame(value = value))
    
    # Calculer Moran's I via regression
    M <- lm(lag_result$Spatial.Lag ~ value)
    moran_i <- coef(M)[2]
    
    return(as.numeric(moran_i))
  }, error = function(e) {
    # Fallback en cas d'erreur
    return(calculate_moran_legacy(x, y, value, k = min(10, n %/% 10)))
  })
}


#' Calculer l'Indice de Moran (methode legacy)
#'
#' Version pure R sans dependance externe, utilisee comme fallback.
#'
#' @param x Vecteur des coordonnees X
#' @param y Vecteur des coordonnees Y
#' @param value Vecteur des valeurs
#' @param k Nombre de voisins les plus proches (defaut 10)
#' @return Indice de Moran entre -1 et 1
#' @noRd
calculate_moran_legacy <- function(x, y, value, k = 5) {
  n <- length(x)
  if (n < k + 5 || length(unique(x)) < 3 || length(unique(y)) < 3) {
    return(0)
  }

  # Supprimer les NA
  valid <- !is.na(x) & !is.na(y) & !is.na(value)
  if (sum(valid) < k + 5) return(0)

  x <- x[valid]
  y <- y[valid]
  value <- value[valid]
  n <- length(value)

  # Limiter la taille pour la performance (3000 points max pour la vitesse)
  max_points <- 3000
  if (n > max_points) {
    set.seed(42)
    sample_idx <- sample(seq_len(n), size = max_points, replace = FALSE)
    x <- x[sample_idx]
    y <- y[sample_idx]
    value <- value[sample_idx]
    n <- length(value)
  }

  # Utiliser FNN si disponible pour plus de rapidite
  if (requireNamespace("FNN", quietly = TRUE)) {
    coords <- matrix(c(x, y), ncol = 2)
    knn_result <- FNN::knn.index(coords, k = k)
    
    # Calculer les distances pour les poids
    W <- matrix(0, nrow = n, ncol = k)
    for (i in 1:n) {
      for (j in 1:k) {
        neighbor <- knn_result[i, j]
        dist_sq <- (x[i] - x[neighbor])^2 + (y[i] - y[neighbor])^2
        W[i, j] <- 1 / (dist_sq + 1e-10)
      }
    }
    neighbor_idx <- knn_result
  } else {
    # Methode standard avec boucle
    W <- matrix(0, nrow = n, ncol = k)
    neighbor_idx <- matrix(0, nrow = n, ncol = k)
    
    for (i in 1:n) {
      dists <- (x - x[i])^2 + (y - y[i])^2
      dists[i] <- Inf
      neighbors <- order(dists)[1:k]
      neighbor_idx[i, ] <- neighbors
      W[i, ] <- 1 / (dists[neighbors] + 1e-10)
    }
  }

  # Normaliser les poids par lignes
  row_sums <- rowSums(W)
  row_sums[row_sums == 0] <- 1
  W <- W / row_sums

  # Calculer l'indice de Moran (vectorise)
  z <- value - mean(value)
  z_sd <- sd(value)
  if (z_sd == 0 || !is.finite(z_sd)) return(0)
  z <- z / z_sd

  # Calcul vectorise du numerateur
  z_matrix <- matrix(z, nrow = n, ncol = k, byrow = FALSE)
  z_neighbors <- matrix(z[neighbor_idx], nrow = n, ncol = k)
  numerator <- sum(W * z_matrix * z_neighbors)
  denominator <- sum(z^2)

  if (denominator == 0 || !is.finite(denominator)) return(0)
  moran_i <- numerator / denominator

  return(moran_i)
}


#' PCDI : Phase Correlation Delay Identification (Version Rapide)
#'
#' Determine automatiquement le delai optimal entre le flux et la position GPS
#' en utilisant une recherche en deux etapes (grossiere + fine) avec rgeoda.
#'
#' @param data Tibble avec donnees de rendement (X, Y, Flow, GPS_Time, Interval)
#' @param delay_range Plage de delais a tester (defaut -25:25 secondes)
#' @param coarse_step Pas pour la recherche grossiere (defaut 2)
#' @param value_col Nom de la colonne de valeurs a analyser
#' @param max_points Nombre maximum de points pour l'analyse
#' @param bandwidth Distance de bande passante pour Moran (defaut 30m)
#' @return Liste avec optimal_delay, score_values et stability_metrics
#' @export
#' @examples
#' \dontrun{
#' result <- apply_pcdi(data, delay_range = -25:25)
#' }
apply_pcdi <- function(data, delay_range = -25:25, n_iterations = 5,
                       noise_level = NULL, value_col = "Flow",
                       sample_fraction = NULL, method = NULL,
                       coarse_step = 2, max_points = 10000,
                       bandwidth = 30) {

  rlang::inform(paste("=== PCDI: Phase Correlation Delay Identification (", value_col, ") ==="))
  rlang::inform(paste("  Iterations:", n_iterations, "| Grid sampling avec pooling des resultats"))

  # Valider l'entree
  required_cols <- c("X", "Y", "GPS_Time", "Interval", value_col)
  if (!all(required_cols %in% names(data))) {
    missing <- setdiff(required_cols, names(data))
    rlang::warn(paste("Colonnes requises manquantes pour PCDI:", paste(missing, collapse = ", ")))
    return(list(
      optimal_delay = 0,
      score_values = NULL,
      stability = NA,
      warning = "Missing required columns"
    ))
  }
   
  # Calculer l'amplitude de rendement pour le bruit
  valid_values <- data[[value_col]][!is.na(data[[value_col]]) & is.finite(data[[value_col]])]

  # Debug: afficher les statistiques
  rlang::inform(paste("  PCDI Debug -", value_col, "values:", length(valid_values)))
  rlang::inform(paste("  PCDI Debug -", value_col, "range:", round(min(valid_values, na.rm = TRUE), 4), "to", round(max(valid_values, na.rm = TRUE), 4)))

  if (length(valid_values) < 10) {
    rlang::warn(paste("Pas assez de valeurs valides pour PCDI sur", value_col))
    return(list(
      optimal_delay = 0,
      score_values = NULL,
      stability = NA,
      warning = "Insufficient valid data"
    ))
  }
  
  value_sd <- stats::sd(valid_values)
  if (!is.finite(value_sd) || value_sd == 0) {
    rlang::warn(paste("Statistiques invalides pour PCDI sur", value_col))
    return(list(
      optimal_delay = 0,
      score_values = NULL,
      stability = NA,
      warning = "Invalid statistics"
    ))
  }

  # Nombre de points dans les donnees originales
  n_points <- nrow(data)
  need_sampling <- n_points > max_points
  
  # === ITERATIONS AVEC GRID SAMPLING ===
  rlang::inform(paste("  PCDI - Lancement de", n_iterations, "iterations avec grid sampling different..."))
  
  # Delais a tester
  all_delays <- min(delay_range):max(delay_range)
  
  # Matrice pour stocker les scores de toutes les iterations (delais x iterations)
  scores_matrix <- matrix(NA, nrow = length(all_delays), ncol = n_iterations)
  
  for (iter in 1:n_iterations) {
    # Echantillonnage par grille avec seed different pour chaque iteration
    if (need_sampling) {
      data_sampled <- grid_sample_data(data, max_points = max_points, cellsize = 100, seed = iter * 100)
      rlang::inform(paste("    Iteration", iter, ":", nrow(data_sampled), "points (seed =", iter * 100, ")"))
    } else {
      data_sampled <- data
    }
    
    # Tester tous les delais pour cette iteration
    for (d_idx in seq_along(all_delays)) {
      delay <- all_delays[d_idx]
      
      # Decaler les valeurs selon le delai
      if (delay >= 0) {
        shifted_values <- dplyr::lead(data_sampled[[value_col]], delay)
      } else {
        shifted_values <- dplyr::lag(data_sampled[[value_col]], abs(delay))
      }
      
      # Calculer le score Moran
      valid <- !is.na(shifted_values) & !is.na(data_sampled$X) & !is.na(data_sampled$Y)
      if (sum(valid) > 50) {
        scores_matrix[d_idx, iter] <- calculate_moran_fast(
          data_sampled$X[valid],
          data_sampled$Y[valid],
          shifted_values[valid],
          bandwidth = bandwidth
        )
      }
    }
  }
  
  # === POOLING DES RESULTATS ===
  # Calculer la moyenne et l'ecart-type des scores sur les iterations
  mean_scores <- rowMeans(scores_matrix, na.rm = TRUE)
  sd_scores <- apply(scores_matrix, 1, sd, na.rm = TRUE)
  
  # Trouver le delai optimal (maximum de la moyenne)
  opt_idx <- which.max(mean_scores)
  optimal_delay <- all_delays[opt_idx]
  
  rlang::inform(paste("  PCDI - Delai optimal (moyenne sur", n_iterations, "iterations):", optimal_delay, "sec"))
  rlang::inform(paste("  PCDI - Score Moran moyen:", round(mean_scores[opt_idx], 4), 
                      "(+/-", round(sd_scores[opt_idx], 4), ")"))
  
  # Afficher les top 5 delais pour debug
  top_idx <- order(mean_scores, decreasing = TRUE)[1:5]
  rlang::inform("  PCDI - Top 5 delais:")
  for (i in 1:5) {
    idx <- top_idx[i]
    rlang::inform(paste("    Delay", all_delays[idx], "sec: Moran =", round(mean_scores[idx], 4), 
                        "(+/-", round(sd_scores[idx], 4), ")"))
  }
  
  # Calculer la stabilite (CV du score optimal)
  stability <- ifelse(mean_scores[opt_idx] > 0 && !is.na(sd_scores[opt_idx]),
                      sd_scores[opt_idx] / mean_scores[opt_idx],
                      NA)
  
  # Avertir si la stabilite est faible
  warning_msg <- NULL
  if (!is.na(stability) && stability > 0.2) {
    warning_msg <- paste("PCDI stability issue - CV =", round(stability, 3))
    rlang::warn(warning_msg)
  }

  result <- list(
    optimal_delay = optimal_delay,
    delay_range_tested = delay_range,
    score_values = data.frame(
      delay = all_delays,
      mean_score = mean_scores,
      sd_score = sd_scores
    ),
    rsc_values = data.frame(
      delay = all_delays,
      mean_score = mean_scores,
      sd_score = sd_scores
    ),
    scores_matrix = scores_matrix,
    stability = stability,
    n_iterations = n_iterations,
    value_col = value_col,
    warning = warning_msg
  )

  rlang::inform(paste("Delai optimal (", value_col, "):", optimal_delay, "secondes"))

  return(result)
}


#' Calculer les seuils automatiques (methode quantiles-IQR)
#'
#' Automatise les filtres MINY, MAXY, MINV, MAXV, POS a partir d'analyses
#' de distributions basees sur les quantiles.
#'
#' @param data Tibble avec donnees de rendement
#' @param yllim Limite quantile basse (defaut 0.05)
#' @param yulim Limite quantile haute (defaut 0.95)
#' @param yscale Facteur d'extension IQR (defaut 1.5)
#' @param vllim Limite quantile basse vitesse (defaut 0.02)
#' @param vulim Limite quantile haute vitesse (defaut 0.98)
#' @param vscale Facteur d'extension IQR vitesse (defaut 1.5)
#' @param minv_abs Seuil minimal absolu de vitesse (defaut 0.5 m/s)
#' @param miny_abs Seuil minimal absolu de rendement (defaut 0)
#' @param gbuffer Marge pour le filtre de position en metres (defaut 100)
 #' @return Liste avec tous les seuils calcules
 #' @noRd
 #' @keywords internal
 calculate_auto_thresholds <- function(data,
                                        yllim = 0.05, yulim = 0.95, yscale = 1.5,
                                        vllim = 0.02, vulim = 0.98, vscale = 1.5,
                                        minv_abs = 0.5, miny_abs = 0,
                                        gbuffer = 100) {

   rlang::inform("=== Automatic Threshold Calculation (AYCE) ===")

   thresholds <- list()

   # ---- Seuils de rendement (MINY / MAXY) ----
   # Utiliser Yield_buacre si disponible, sinon Flow
   if ("Yield_buacre" %in% names(data)) {
     y_col <- "Yield_buacre"
   } else if ("Flow" %in% names(data)) {
     y_col <- "Flow"
   } else {
     y_col <- NULL
   }

   if (!is.null(y_col)) {
     # Exclure les Inf pour le calcul des quantiles
     data_yield <- data |>
       dplyr::filter(is.finite(.data[[y_col]]))

     if (nrow(data_yield) > 10) {
       y_quantiles <- quantile(data_yield[[y_col]], c(yllim, yulim), na.rm = TRUE)
       y_iqr <- y_quantiles[2] - y_quantiles[1]

       thresholds$min_yield <- max(miny_abs, y_quantiles[1] - yscale * y_iqr)
       thresholds$max_yield <- y_quantiles[2] + yscale * y_iqr

       rlang::inform(paste("Yield:", y_col, "MIN =", round(thresholds$min_yield, 2),
                           "MAX =", round(thresholds$max_yield, 2)))
     } else {
       # Utiliser les valeurs par defaut si donnees insuffisantes
       thresholds$min_yield <- miny_abs
       thresholds$max_yield <- 300  # Reasonable max for corn
       rlang::warn("Pas assez de donnees valides pour calculer seuils de rendement - utilisation valeurs par defaut")
     }
   }

   # ---- Seuils de vitesse (MINV / MAXV) ----
   if (all(c("X", "Y", "Interval") %in% names(data))) {
     data_vel <- data |>
       dplyr::mutate(
         velocity = sqrt((X - dplyr::lag(X))^2 + (Y - dplyr::lag(Y))^2) / Interval
       ) |>
       dplyr::filter(!is.na(velocity))

     if (nrow(data_vel) > 10) {
       v_quantiles <- quantile(data_vel$velocity, c(vllim, vulim), na.rm = TRUE)
       v_q1 <- as.numeric(v_quantiles[1])
       v_q2 <- as.numeric(v_quantiles[2])
       v_iqr <- v_q2 - v_q1

       thresholds$min_velocity <- max(minv_abs, v_q1 - vscale * v_iqr)
       thresholds$max_velocity <- v_q2 + vscale * v_iqr

       rlang::inform(paste("Velocity: MIN =", round(thresholds$min_velocity, 2),
                           "MAX =", round(thresholds$max_velocity, 2)))
     } else {
       # Utiliser les valeurs par defaut si donnees insuffisantes
       thresholds$min_velocity <- minv_abs
       thresholds$max_velocity <- 15
       rlang::warn("Pas assez de donnees pour calculer seuils de velocite - utilisation valeurs par defaut")
     }
   } else {
     # Utiliser les valeurs par defaut
     thresholds$min_velocity <- minv_abs
     thresholds$max_velocity <- 15
   }

   # ---- Filtre de position (POS) ----
   if (all(c("X", "Y") %in% names(data))) {
     x_quantiles <- quantile(data$X, c(0.02, 0.98), na.rm = TRUE)
     y_quantiles <- quantile(data$Y, c(0.02, 0.98), na.rm = TRUE)

     thresholds$pos_x_min <- x_quantiles[1] - gbuffer
     thresholds$pos_x_max <- x_quantiles[2] + gbuffer
     thresholds$pos_y_min <- y_quantiles[1] - gbuffer
     thresholds$pos_y_max <- y_quantiles[2] + gbuffer

     rlang::inform(paste("Position: X[", round(thresholds$pos_x_min, 0), "-",
                         round(thresholds$pos_x_max, 0), "]"))
     rlang::inform(paste("Position: Y[", round(thresholds$pos_y_min, 0), "-",
                         round(thresholds$pos_y_max, 0), "]"))
   }

   # Stocker les parametres pour reference
   thresholds$parameters <- list(
     yllim = yllim, yulim = yulim, yscale = yscale,
     vllim = vllim, vulim = vulim, vscale = vscale,
     minv_abs = minv_abs, miny_abs = miny_abs,
     gbuffer = gbuffer
   )

   # Enlever les noms des seuils numeriques pour eviter les soucis avec paste()
   numeric_fields <- c("min_yield", "max_yield", "min_velocity", "max_velocity",
                       "pos_x_min", "pos_x_max", "pos_y_min", "pos_y_max")
   for (field in numeric_fields) {
     if (!is.null(thresholds[[field]])) {
       thresholds[[field]] <- unname(thresholds[[field]])
     }
   }

   return(thresholds)
 }


 #' Appliquer le filtre de position (POS)
 #'
 #' Elimine les flyers GPS en verifiant que les points sont dans l'enveloppe
 #' inter-quantile etendue du champ.
 #'
 #' @param data Tibble avec coordonnees X, Y
 #' @param thresholds Liste des seuils de position
 #' @return Tibble filtre
 #' @noRd
 #' @keywords internal
 apply_position_filter <- function(data, thresholds) {
   if (!all(c("X", "Y") %in% names(data))) {
     rlang::warn("Colonnes X, Y requises pour filtre de position")
     return(data)
   }

   n_before <- nrow(data)

   data <- data |>
     dplyr::filter(
       X >= thresholds$pos_x_min,
       X <= thresholds$pos_x_max,
       Y >= thresholds$pos_y_min,
       Y <= thresholds$pos_y_max
     )

   n_removed <- n_before - nrow(data)
   if (n_removed > 0) {
     rlang::inform(paste("Position filter:", n_removed, "points elimines (flyers GPS)"))
   }

   return(data)
 }
