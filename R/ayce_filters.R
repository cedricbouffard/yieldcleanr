#' Bitmap-based Overlap Filter
#'
#' Implémente la méthode rasterisée de Han et al. (1997) pour détecter
#' et éliminer les zones de chevauchement.
#'
#' @param data Tibble with X, Y coordinates and swath width
#' @param cellsize Grid cell size in meters (default 0.3)
#' @param overlap_threshold Maximum acceptable overlap ratio (0-1, default 0.5)
#' @return Filtered tibble without overlaps
#' @noRd
#' @examples
#' # Create sample data with potential overlap points
#' data <- tibble::tibble(
#'   X = c(435000, 435001, 435002, 435003, 435100),
#'   Y = c(5262000, 5262001, 5262002, 5262003, 5262100),
#'   Flow = c(10, 15, 12, 18, 20),
#'   Swath = c(240, 240, 240, 240, 240)
#' )
#'
#' # Apply overlap filter (cellsize = 0.3m, max 50% overlap)
#' data_clean <- apply_overlap_filter(data, cellsize = 0.3, overlap_threshold = 0.5)
#' print(data_clean)
apply_overlap_filter <- function(data, cellsize = 0.3, overlap_threshold = 0.5) {

  rlang::inform("=== Bitmap Overlap Filter ===")

  if (!all(c("X", "Y", "Swath") %in% names(data))) {
    rlang::warn("Colonnes X, Y, Swath requises pour filtre overlap")
    return(data)
  }

  n_before <- nrow(data)

  # Convert swath from inches to meters
  swath_m <- data$Swath * 0.0254

  # Create bitmap grid with reasonable bounds
  x_min <- floor(min(data$X) / cellsize) * cellsize
  x_max <- ceiling(max(data$X) / cellsize) * cellsize
  y_min <- floor(min(data$Y) / cellsize) * cellsize
  y_max <- ceiling(max(data$Y) / cellsize) * cellsize

  n_x <- max(1, ceiling((x_max - x_min) / cellsize))
  n_y <- max(1, ceiling((y_max - y_min) / cellsize))

  # Limit grid size to avoid memory issues
  max_cells <- 10000
  use_sparse <- (n_x * n_y > max_cells)
  if (use_sparse) {
    rlang::inform("Données étendues - utilisation du bitmap sparse")
    bitmap_env <- new.env(hash = TRUE, parent = emptyenv())
    is_harvested <- function(yc, xc) {
      key <- paste0(yc, "_", xc)
      exists(key, envir = bitmap_env, inherits = FALSE)
    }
    mark_harvested <- function(yc, xc) {
      key <- paste0(yc, "_", xc)
      bitmap_env[[key]] <- TRUE
    }
  } else {
    bitmap <- matrix(as.integer(0), nrow = n_y, ncol = n_x)
    is_harvested <- function(yc, xc) {
      consider_harvested <- bitmap[yc, xc] == 1L
      consider_harvested
    }
    mark_harvested <- function(yc, xc) {
      bitmap[yc, xc] <- 1L
    }
  }

  # Process each point
  overlap_ratio <- numeric(nrow(data))

  for (i in seq_len(nrow(data))) {
    xi <- data$X[i]
    yi <- data$Y[i]
    sw <- swath_m[i]

    # Calculate cell indices
    cx <- floor((xi - x_min) / cellsize) + 1
    cy <- floor((yi - y_min) / cellsize) + 1

    if (cx < 1 || cx > n_x || cy < 1 || cy > n_y) {
      overlap_ratio[i] <- 0
      next
    }

    # Cells covered by swath (rectangle)
    half_width_cells <- max(1, ceiling((sw / 2) / cellsize))
    half_length_cells <- 1

    x_cells <- (cx - half_width_cells):(cx + half_width_cells)
    y_cells <- (cy - half_length_cells):(cy + half_length_cells)
    x_cells <- x_cells[x_cells >= 1 & x_cells <= n_x]
    y_cells <- y_cells[y_cells >= 1 & y_cells <= n_y]

    # Count overlap
    n_harvested <- 0
    n_total <- 0

    for (xc in x_cells) {
      for (yc in y_cells) {
        cell_x <- x_min + (xc - 0.5) * cellsize
        cell_y <- y_min + (yc - 0.5) * cellsize
        if (sqrt((cell_x - xi)^2 + (cell_y - yi)^2) <= sw / 2) {
          n_total <- n_total + 1
          if (is_harvested(yc, xc)) n_harvested <- n_harvested + 1
        }
      }
    }

    overlap_ratio[i] <- if (n_total > 0) n_harvested / n_total else 0

    # Mark cells as harvested
    for (xc in x_cells) {
      for (yc in y_cells) {
        cell_x <- x_min + (xc - 0.5) * cellsize
        cell_y <- y_min + (yc - 0.5) * cellsize
        if (sqrt((cell_x - xi)^2 + (cell_y - yi)^2) <= sw / 2) {
          mark_harvested(yc, xc)
        }
      }
    }
  }

  overlap_min <- min(overlap_ratio, na.rm = TRUE)
  overlap_max <- max(overlap_ratio, na.rm = TRUE)
  rlang::inform(paste("Overlap ratio: min", round(overlap_min, 3),
                      "max", round(overlap_max, 3)))

  # Filter
  data <- data |>
    dplyr::mutate(overlap_ratio = overlap_ratio) |>
    dplyr::filter(overlap_ratio <= overlap_threshold)

  n_removed <- n_before - nrow(data)
  rlang::inform(paste("Overlap filter:", n_removed, "points éliminés",
                      paste0("(", round(n_removed/n_before*100, 1), "%)")))

  return(data)
}


#' Localized Standard Deviation Filter
#'
#' Crée une grille spatiale et élimine les points aberrants locaux
#' basés sur l'écart-type local.
#'
#' @param data Tibble with X, Y, Flow columns
#' @param n_swaths Number of swath widths for grid cell (default 5)
#' @param lsd_limit Local SD limit multiplier (default 3)
#' @param min_cells Minimum observations per cell (default 3)
#' @return Filtered tibble
#' @noRd
#' @examples
#' # Create sample data with local outliers
#' data <- tibble::tibble(
#'   X = c(435000, 435001, 435002, 435003, 435004, 435005,
#'         435100, 435101, 435102, 435103, 435104, 435105),
#'   Y = c(5262000, 5262001, 5262002, 5262003, 5262004, 5262005,
#'         5262100, 5262101, 5262102, 5262103, 5262104, 5262105),
#'   Flow = c(50, 55, 52, 58, 300, 54,  # 300 is a local outlier
#'            45, 48, 47, 50, 49, 46),
#'   Swath = c(240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240)
#' )
#'
#' # Apply local SD filter
#' data_clean <- apply_local_sd_filter(data, n_swaths = 5, lsd_limit = 3)
#' print(data_clean)
apply_local_sd_filter <- function(data, n_swaths = 5, lsd_limit = 3,
                                   min_cells = 3) {

  rlang::inform("=== Localized SD Filter ===")

  if (!all(c("X", "Y", "Flow", "Swath") %in% names(data))) {
    rlang::warn("Colonnes X, Y, Flow, Swath requises")
    return(data)
  }

  # Convert swath to meters and calculate cell size
  swath_m <- mean(data$Swath, na.rm = TRUE) * 0.0254
  cellsize <- n_swaths * swath_m

  n_before <- nrow(data)

  # Create grid cell IDs
  data <- data |>
    dplyr::mutate(
      cell_x = floor(X / cellsize),
      cell_y = floor(Y / cellsize),
      cell_id = paste(cell_x, cell_y, sep = "_")
    )

  # Calculate local statistics per cell
  cell_stats <- data |>
    dplyr::group_by(cell_id) |>
    dplyr::summarise(
      local_mean = mean(Flow, na.rm = TRUE),
      local_sd = stats::sd(Flow, na.rm = TRUE),
      n = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::filter(n >= min_cells)

  # Calculate global statistics for cells with too few observations
  global_mean <- mean(data$Flow, na.rm = TRUE)
  global_sd <- stats::sd(data$Flow, na.rm = TRUE)

  # Join and filter
  data <- data |>
    dplyr::left_join(cell_stats, by = "cell_id") |>
    dplyr::mutate(
      local_mean = ifelse(is.na(local_mean), global_mean, local_mean),
      local_sd = ifelse(is.na(local_sd), global_sd, local_sd),
      sd_upper = local_mean + lsd_limit * local_sd,
      sd_lower = local_mean - lsd_limit * local_sd,
      is_outlier = Flow > sd_upper | Flow < sd_lower
    ) |>
    dplyr::filter(!is_outlier) |>
    dplyr::select(-cell_x, -cell_y, -cell_id, -local_mean, -local_sd,
                  -n, -sd_upper, -sd_lower, -is_outlier)

  n_removed <- n_before - nrow(data)
  rlang::inform(paste("Local SD filter:", n_removed, "points éliminés",
                      paste0("(", round(n_removed/n_before*100, 1), "%)")))

  return(data)
}


#' AYCE Validation and Quality Control
#'
#' Évalue la stabilité des résultats et signale les cas douteux.
#'
#' @param data_clean Cleaned data tibble
#' @param data_raw Raw data tibble
#' @param pcdi_result Result from PCDI analysis
#' @param thresholds Thresholds used
#' @return List with validation metrics and warnings
#' @noRd
ayce_validate <- function(data_clean, data_raw, pcdi_result = NULL,
                          thresholds = NULL) {

  rlang::inform("=== AYCE Validation & Quality Control ===")

  validation <- list()

  # Point retention rate
  retention_rate <- nrow(data_clean) / nrow(data_raw)
  validation$retention_rate <- retention_rate

  rlang::inform(paste("Retention rate:", round(retention_rate * 100, 1), "%"))

  # Flag unusual retention rates
  validation$warning <- NULL
  if (retention_rate < 0.5) {
    validation$warning <- "Very low retention rate - check parameters"
    rlang::warn(validation$warning)
  } else if (retention_rate > 0.99) {
    validation$warning <- "Almost no points removed - check if AYCE is working"
    rlang::warn(validation$warning)
  }

  # PCDI stability check
  if (!is.null(pcdi_result) && !is.null(pcdi_result$warning)) {
    validation$pcdi_stable <- is.null(pcdi_result$warning) || pcdi_result$warning == "None"
    if (!validation$pcdi_stable) {
      rlang::warn(paste("PCDI warning:", pcdi_result$warning))
    }
  } else {
    validation$pcdi_stable <- TRUE
  }

  # Yield statistics comparison
  if ("Flow" %in% names(data_raw) && "Flow" %in% names(data_clean)) {
    validation$raw_stats <- list(
      mean = mean(data_raw$Flow, na.rm = TRUE),
      sd = stats::sd(data_raw$Flow, na.rm = TRUE),
      cv = stats::sd(data_raw$Flow, na.rm = TRUE) / mean(data_raw$Flow, na.rm = TRUE)
    )
    validation$clean_stats <- list(
      mean = mean(data_clean$Flow, na.rm = TRUE),
      sd = stats::sd(data_clean$Flow, na.rm = TRUE),
      cv = stats::sd(data_clean$Flow, na.rm = TRUE) / mean(data_clean$Flow, na.rm = TRUE)
    )

    # Check for reasonable CV improvement
    cv_improvement <- validation$raw_stats$cv - validation$clean_stats$cv
    validation$cv_improvement <- cv_improvement

    rlang::inform(paste("Raw CV:", round(validation$raw_stats$cv * 100, 1), "%"))
    rlang::inform(paste("Clean CV:", round(validation$clean_stats$cv * 100, 1), "%"))
  }

  return(validation)
}
