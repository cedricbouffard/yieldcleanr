#' AYCE: Auto Yield Cleaning Engine
#'
#' Système expert automatisé pour le nettoyage des données de rendement
#' sans intervention humaine ou avec intervention minimale.
#'
#' @name ayce
#' @aliases ayce_clean calculate_auto_thresholds apply_pcdi
#' @noRd
"_PACKAGE"


# ==============================================================================
# AYCE - AUTO YIELD CLEANING ENGINE
# Implémentation basée sur USDA Yield Editor AYCE methodology
# ==============================================================================

#' Calculate RSC (Relative Spatial Coherence) for PCDI method
#'
#' Mesure la cohérence spatiale relative du rendement pour optimiser les délais.
#' Plus le RSC est élevé, meilleure est l'alignement spatial.
#'
#' @param x Vector of X coordinates
#' @param y Vector of Y coordinates
#' @param yield Vector of yield values
#' @return RSC value between 0 and 1
#' @noRd
calculate_rsc <- function(x, y, yield) {
  if (length(x) < 10 || length(unique(x)) < 3 || length(unique(y)) < 3) {
    return(0)
  }

  # Create grid and bin yield values
  n_bins <- 50
  x_bins <- quantile(x, probs = seq(0, 1, length.out = n_bins))
  y_bins <- quantile(y, probs = seq(0, 1, length.out = n_bins))

  # Calculate binned means
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

  # Calculate spatial structure - ratio of within-cell variance to total variance
  valid_cells <- !is.na(grid_yield) & count_grid > 2
  if (sum(valid_cells) < 10) return(0)

  cell_means <- grid_yield[valid_cells]
  global_mean <- mean(yield, na.rm = TRUE)

  # Spatial coherence = 1 - (within-cell variance / total variance)
  within_var <- mean((cell_means - global_mean)^2, na.rm = TRUE)
  total_var <- var(yield, na.rm = TRUE)

  if (total_var == 0) return(0)
  rsc <- 1 - within_var / total_var

  return(max(0, min(1, rsc)))
}


#' PCDI: Phase Correlation Delay Identification
#'
#' Détermine automatiquement le délai optimal entre le flux et la position GPS
#' en utilisant la méthode de corrélation de phase.
#'
#' @param data Tibble with yield data (must have X, Y, Flow, GPS_Time, Interval)
#' @param delay_range Range of delays to test (default 0:20 seconds)
#' @param n_iterations Number of iterations with random noise (default 10)
#' @param noise_level Gaussian noise level as proportion of yield range
#' @return List with optimal_delay, rsc_values, and stability_metrics
#' @noRd
#' @examples
#' \dontrun{
#' result <- apply_pcdi(data, delay_range = 0:20)
#' }
apply_pcdi <- function(data, delay_range = 0:20, n_iterations = 10,
                       noise_level = 0.05) {

  rlang::inform("=== PCDI: Phase Correlation Delay Identification ===")

  # Validate input
  required_cols <- c("X", "Y", "Flow", "GPS_Time", "Interval")
  if (!all(required_cols %in% names(data))) {
    rlang::warn("Colonnes requises manquantes pour PCDI")
    return(list(
      optimal_delay = 2,
      rsc_values = NULL,
      stability = NA,
      warning = "Missing required columns"
    ))
  }

  # Calculate yield range for noise
  yield_range <- diff(range(data$Flow, na.rm = TRUE))
  yield_sd <- stats::sd(data$Flow, na.rm = TRUE)

  # Store RSC for each delay
  rsc_matrix <- matrix(NA, nrow = length(delay_range), ncol = n_iterations)

  for (iter in 1:n_iterations) {
    # Add small random noise for robustness
    noise <- rnorm(nrow(data), 0, yield_sd * noise_level)
    yield_noisy <- data$Flow + noise

    for (d_idx in seq_along(delay_range)) {
      delay <- delay_range[d_idx]

      # Shift yield by delay (positive = forward, negative = backward)
      if (delay >= 0) {
        # Shift forward: prepend NAs
        shifted_yield <- c(rep(NA, delay), yield_noisy[1:(length(yield_noisy) - delay)])
      } else {
        # Shift backward: remove from start, append NAs at end
        abs_delay <- abs(delay)
        shifted_yield <- c(yield_noisy[(abs_delay + 1):length(yield_noisy)], rep(NA, abs_delay))
      }

      # Calculate RSC
      valid <- !is.na(shifted_yield) & !is.na(data$X) & !is.na(data$Y)
      if (sum(valid) > 100) {
        rsc_matrix[d_idx, iter] <- calculate_rsc(
          data$X[valid],
          data$Y[valid],
          shifted_yield[valid]
        )
      }
    }
  }

  # Calculate mean RSC across iterations
  mean_rsc <- rowMeans(rsc_matrix, na.rm = TRUE)
  std_rsc <- apply(rsc_matrix, 1, stats::sd, na.rm = TRUE)

  # Find optimal delay (maximum mean RSC)
  opt_idx <- which.min(mean_rsc)
  optimal_delay <- delay_range[opt_idx]

  # Calculate stability (coefficient of variation across iterations)
  stability <- ifelse(mean_rsc[opt_idx] > 0,
                      std_rsc[opt_idx] / mean_rsc[opt_idx],
                      NA)

  # Generate warning if stability is low
  warning_msg <- NULL
  if (!is.na(stability) && stability > 0.1) {
    warning_msg <- paste("PCDI stability issue - CV =", round(stability, 3))
    rlang::warn(warning_msg)
  }

  # Results
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
    warning = warning_msg
  )

  rlang::inform(paste("Optimal Flow Delay:", optimal_delay, "seconds"))
  rlang::inform(paste("RSC at optimal:", round(mean_rsc[opt_idx], 4)))
  rlang::inform(paste("Stability (CV):", round(stability, 4)))

  return(result)
}


#' Calculate automatic thresholds using quantile-IQR method
#'
#' Automatise les filtres MINY, MAXY, MINV, MAXV, POS à partir d'analyses
#' de distributions basées sur les quantiles.
#'
#' @param data Tibble with yield data
#' @param yllim Lower quantile limit for yield (default 0.05)
#' @param yulim Upper quantile limit for yield (default 0.95)
#' @param yscale Scale factor for IQR extension (default 1.5)
#' @param vllim Lower quantile limit for velocity (default 0.02)
#' @param vulim Upper quantile limit for velocity (default 0.98)
#' @param vscale Scale factor for velocity IQR extension (default 1.5)
#' @param minv_abs Absolute minimum velocity threshold (default 0.5 m/s)
#' @param miny_abs Absolute minimum yield threshold (default 0)
#' @param gbuffer Buffer for position filter in meters (default 100)
#' @return List with all calculated thresholds
#' @noRd
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

  # ---- Yield thresholds (MINY / MAXY) ----
  # Use Yield_buacre if available, otherwise fall back to Flow
  if ("Yield_buacre" %in% names(data)) {
    y_col <- "Yield_buacre"
  } else if ("Flow" %in% names(data)) {
    y_col <- "Flow"
  } else {
    y_col <- NULL
  }

  if (!is.null(y_col)) {
    # Filter out Inf values for quantile calculation
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
      # Use default values if not enough valid data
      thresholds$min_yield <- miny_abs
      thresholds$max_yield <- 300  # Reasonable max for corn
      rlang::warn("Pas assez de données valides pour calculer seuils de rendement - utilisation valeurs par défaut")
    }
  }

  # ---- Velocity thresholds (MINV / MAXV) ----
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
      # Use default values if not enough data
      thresholds$min_velocity <- minv_abs
      thresholds$max_velocity <- 15
      rlang::warn("Pas assez de données pour calculer seuils de vélocité - utilisation valeurs par défaut")
    }
  } else {
    # Use default values
    thresholds$min_velocity <- minv_abs
    thresholds$max_velocity <- 15
  }

  # ---- Position filter (POS) ----
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

  # Store parameters for reference
  thresholds$parameters <- list(
    yllim = yllim, yulim = yulim, yscale = yscale,
    vllim = vllim, vulim = vulim, vscale = vscale,
    minv_abs = minv_abs, miny_abs = miny_abs,
    gbuffer = gbuffer
  )

  # Unname all numeric thresholds to avoid issues with paste()
  numeric_fields <- c("min_yield", "max_yield", "min_velocity", "max_velocity",
                      "pos_x_min", "pos_x_max", "pos_y_min", "pos_y_max")
  for (field in numeric_fields) {
    if (!is.null(thresholds[[field]])) {
      thresholds[[field]] <- unname(thresholds[[field]])
    }
  }

  return(thresholds)
}


#' Apply position filter (POS)
#'
#' Élimine les flyers GPS en vérifiant que les points sont dans l'enveloppe
#' inter-quantile étendue du champ.
#'
#' @param data Tibble with X, Y coordinates
#' @param thresholds List with position thresholds
#' @return Filtered tibble
#' @noRd
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
    rlang::inform(paste("Position filter:", n_removed, "points éliminés (flyers GPS)"))
  }

  return(data)
}
