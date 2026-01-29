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


#' PCDI : Phase Correlation Delay Identification
#'
#' Determine automatiquement le delai optimal entre le flux et la position GPS
#' en utilisant la methode de correlation de phase.
#'
#' @param data Tibble avec donnees de rendement (X, Y, Flow, GPS_Time, Interval)
#' @param delay_range Plage de delais a tester (defaut 0:20 secondes)
#' @param n_iterations Nombre d'iterations avec bruit aleatoire (defaut 10)
#' @param noise_level Niveau de bruit gaussien en proportion de la plage
 #' @return Liste avec optimal_delay, rsc_values et stability_metrics
 #' @export
 #' @examples
 #' \dontrun{
 #' result <- apply_pcdi(data, delay_range = 0:20)
 #' }
 apply_pcdi <- function(data, delay_range = 0:20, n_iterations = 10,
                       noise_level = 0.05, value_col = "Flow") {

  rlang::inform(paste("=== PCDI: Phase Correlation Delay Identification (", value_col, ") ==="))

  # Valider l'entree
  required_cols <- c("X", "Y", "GPS_Time", "Interval", value_col)
  if (!all(required_cols %in% names(data))) {
    missing <- setdiff(required_cols, names(data))
    rlang::warn(paste("Colonnes requises manquantes pour PCDI:", paste(missing, collapse = ", ")))
    return(list(
      optimal_delay = 2,
      rsc_values = NULL,
      stability = NA,
      warning = "Missing required columns"
    ))
  }

  # Calculer l'amplitude de rendement pour le bruit
  # Verifier qu'il y a des valeurs valides
  valid_values <- data[[value_col]][!is.na(data[[value_col]]) & is.finite(data[[value_col]])]
  
  if (length(valid_values) < 10) {
    rlang::warn(paste("Pas assez de valeurs valides pour PCDI sur", value_col))
    return(list(
      optimal_delay = 0,
      rsc_values = NULL,
      stability = NA,
      warning = "Insufficient valid data"
    ))
  }
  
  value_range <- diff(range(valid_values))
  value_sd <- stats::sd(valid_values)
  
  # Verifier que les statistiques sont valides
  if (!is.finite(value_range) || !is.finite(value_sd) || value_sd == 0) {
    rlang::warn(paste("Statistiques invalides pour PCDI sur", value_col))
    return(list(
      optimal_delay = 0,
      rsc_values = NULL,
      stability = NA,
      warning = "Invalid statistics"
    ))
  }

  # Stocker le RSC pour chaque delai
  rsc_matrix <- matrix(NA, nrow = length(delay_range), ncol = n_iterations)

  for (iter in 1:n_iterations) {
    # Ajouter un bruit aleatoire pour robustesse
    noise <- rnorm(nrow(data), 0, value_sd * noise_level)
    values_noisy <- data[[value_col]] + noise

    for (d_idx in seq_along(delay_range)) {
      delay <- delay_range[d_idx]

      # Decaler les valeurs selon le delai (positif = avant, negatif = arriere)
      if (delay >= 0) {
        # Decaler vers l'avant : ajouter des NA au debut
        shifted_values <- c(rep(NA, delay), values_noisy[1:(length(values_noisy) - delay)])
      } else {
        # Decaler vers l'arriere : enlever au debut, NA a la fin
        abs_delay <- abs(delay)
        shifted_values <- c(values_noisy[(abs_delay + 1):length(values_noisy)], rep(NA, abs_delay))
      }

      # Calculer le RSC
      valid <- !is.na(shifted_values) & !is.na(data$X) & !is.na(data$Y)
      if (sum(valid) > 100) {
        rsc_matrix[d_idx, iter] <- calculate_rsc(
          data$X[valid],
          data$Y[valid],
          shifted_values[valid]
        )
      }
    }
  }

  # Calculer le RSC moyen sur les iterations
  mean_rsc <- rowMeans(rsc_matrix, na.rm = TRUE)
  std_rsc <- apply(rsc_matrix, 1, stats::sd, na.rm = TRUE)

  # Trouver le delai optimal (maximum du RSC moyen)
  opt_idx <- which.max(mean_rsc)
  optimal_delay <- delay_range[opt_idx]

  # Calculer la stabilite (CV entre iterations)
  stability <- ifelse(mean_rsc[opt_idx] > 0,
                      std_rsc[opt_idx] / mean_rsc[opt_idx],
                      NA)

  # Avertir si la stabilite est faible
  warning_msg <- NULL
  if (!is.na(stability) && stability > 0.1) {
    warning_msg <- paste("PCDI stability issue - CV =", round(stability, 3))
    rlang::warn(warning_msg)
  }

  # Resultats
  result <- list(
    optimal_delay = optimal_delay,
    delay_range_tested = delay_range,
    rsc_values = data.frame(
      delay = delay_range,
      mean_rsc = mean_rsc,
      std_rsc = std_rsc
    ),
    stability = stability,
    noise_level = noise_level,
    n_iterations = n_iterations,
    value_col = value_col,
    warning = warning_msg
  )

  rlang::inform(paste("Delai optimal (", value_col, "):", optimal_delay, "secondes"))
  rlang::inform(paste("RSC a l'optimal :", round(mean_rsc[opt_idx], 4)))
  rlang::inform(paste("Stabilite (CV) :", round(stability, 4)))

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
 #' @export
 #' @examples
 #' \dontrun{
 #' thresholds <- calculate_auto_thresholds(data)
 #' }
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
 #' @export
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
