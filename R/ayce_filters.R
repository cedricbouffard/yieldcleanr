#' Filtre de chevauchement base sur un bitmap
#'
#' Implemente la methode rasterisee de Han et al. (1997) pour detecter
#' et eliminer les zones de chevauchement.
#'
#' @param data Tibble avec coordonnees X, Y et largeur de coupe
#' @param cellsize Taille de cellule de grille en metres (defaut 0.3)
#' @param overlap_threshold Ratio maximal de chevauchement (0-1, defaut 0.5)
 #' @return Tibble filtre sans chevauchement
 #' @export
 #' @examples
 #' # Creer des donnees d'exemple avec chevauchements possibles
 #' data <- tibble::tibble(
 #'   X = c(435000, 435001, 435002, 435003, 435100),
 #'   Y = c(5262000, 5262001, 5262002, 5262003, 5262100),
 #'   Flow = c(10, 15, 12, 18, 20),
 #'   Swath = c(240, 240, 240, 240, 240)
 #' )
 #'
 #' # Appliquer le filtre de chevauchement (cellule 0.3m, max 50%)
 #' data_clean <- apply_overlap_filter(data, cellsize = 0.3, overlap_threshold = 0.5)
 #' print(data_clean)
 apply_overlap_filter <- function(data, cellsize = 0.3, overlap_threshold = 0.5) {

  rlang::inform("=== Bitmap Overlap Filter ===")

  if (!all(c("X", "Y", "Swath") %in% names(data))) {
    rlang::warn("Colonnes X, Y, Swath requises pour filtre overlap")
    return(data)
  }

  n_before <- nrow(data)

  # Convertir la largeur de coupe en metres
  swath_m <- data$Swath * 0.0254

  # Creer la grille bitmap avec des bornes raisonnables
  x_min <- floor(min(data$X) / cellsize) * cellsize
  x_max <- ceiling(max(data$X) / cellsize) * cellsize
  y_min <- floor(min(data$Y) / cellsize) * cellsize
  y_max <- ceiling(max(data$Y) / cellsize) * cellsize

  n_x <- max(1, ceiling((x_max - x_min) / cellsize))
  n_y <- max(1, ceiling((y_max - y_min) / cellsize))

  # Limiter la taille de grille pour eviter les problemes de memoire
  max_cells <- 10000
  use_sparse <- (n_x * n_y > max_cells)
  if (use_sparse) {
    rlang::inform("Donnees etendues - utilisation du bitmap sparse")
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

  # Traiter chaque point
  overlap_ratio <- numeric(nrow(data))

  for (i in seq_len(nrow(data))) {
    xi <- data$X[i]
    yi <- data$Y[i]
    sw <- swath_m[i]

    # Calculer les indices de cellule
    cx <- floor((xi - x_min) / cellsize) + 1
    cy <- floor((yi - y_min) / cellsize) + 1

    if (cx < 1 || cx > n_x || cy < 1 || cy > n_y) {
      overlap_ratio[i] <- 0
      next
    }

    # Cellules couvertes par la coupe (rectangle)
    half_width_cells <- max(1, ceiling((sw / 2) / cellsize))
    half_length_cells <- 1

    x_cells <- (cx - half_width_cells):(cx + half_width_cells)
    y_cells <- (cy - half_length_cells):(cy + half_length_cells)
    x_cells <- x_cells[x_cells >= 1 & x_cells <= n_x]
    y_cells <- y_cells[y_cells >= 1 & y_cells <= n_y]

    # Calculer le chevauchement
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

    # Marquer les cellules comme recoltees
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

  # Filtrer
  data <- data |>
    dplyr::mutate(overlap_ratio = overlap_ratio) |>
    dplyr::filter(overlap_ratio <= overlap_threshold)

  n_removed <- n_before - nrow(data)
  rlang::inform(paste("Overlap filter:", n_removed, "points éliminés",
                      paste0("(", round(n_removed/n_before*100, 1), "%)")))

  return(data)
}


#' Filtre d'ecart-type localise
#'
#' Cree une grille spatiale et elimine les points aberrants locaux
#' bases sur l'ecart-type local.
#'
#' @param data Tibble avec colonnes X, Y, Flow
#' @param n_swaths Nombre de largeurs de coupe par cellule (defaut 5)
#' @param lsd_limit Multiplicateur de l'ET local (defaut 3)
#' @param min_cells Observations minimales par cellule (defaut 3)
 #' @return Tibble filtre
 #' @export
 #' @examples
 #' # Creer des donnees d'exemple avec outliers locaux
 #' data <- tibble::tibble(
 #'   X = c(435000, 435001, 435002, 435003, 435004, 435005,
 #'         435100, 435101, 435102, 435103, 435104, 435105),
 #'   Y = c(5262000, 5262001, 5262002, 5262003, 5262004, 5262005,
 #'         5262100, 5262101, 5262102, 5262103, 5262104, 5262105),
 #'   Flow = c(50, 55, 52, 58, 300, 54,  # 300 = outlier local
 #'            45, 48, 47, 50, 49, 46),
 #'   Swath = c(240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240)
 #' )
 #'
 #' # Appliquer le filtre ET local
 #' data_clean <- apply_local_sd_filter(data, n_swaths = 5, lsd_limit = 3)
 #' print(data_clean)
 apply_local_sd_filter <- function(data, n_swaths = 5, lsd_limit = 3,
                                   min_cells = 3) {

  rlang::inform("=== Localized SD Filter ===")

  if (!all(c("X", "Y", "Flow", "Swath") %in% names(data))) {
    rlang::warn("Colonnes X, Y, Flow, Swath requises")
    return(data)
  }

  # Convertir la largeur de coupe et calculer la taille de cellule
  swath_m <- mean(data$Swath, na.rm = TRUE) * 0.0254
  cellsize <- n_swaths * swath_m

  n_before <- nrow(data)

  # Creer les identifiants de cellule
  data <- data |>
    dplyr::mutate(
      cell_x = floor(X / cellsize),
      cell_y = floor(Y / cellsize),
      cell_id = paste(cell_x, cell_y, sep = "_")
    )

  # Calculer les statistiques locales par cellule
  cell_stats <- data |>
    dplyr::group_by(cell_id) |>
    dplyr::summarise(
      local_mean = mean(Flow, na.rm = TRUE),
      local_sd = stats::sd(Flow, na.rm = TRUE),
      n = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::filter(n >= min_cells)

  # Calculer les statistiques globales pour cellules peu peuplees
  global_mean <- mean(data$Flow, na.rm = TRUE)
  global_sd <- stats::sd(data$Flow, na.rm = TRUE)

  # Joindre et filtrer
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


#' Validation AYCE et controle qualite
#'
#' Evalue la stabilite des resultats et signale les cas douteux.
#'
#' @param data_clean Tibble des donnees nettoyees
#' @param data_raw Tibble des donnees brutes
#' @param pcdi_result Resultat de l'analyse PCDI
#' @param thresholds Seuils utilises
#' @return Liste avec metriques de validation et avertissements
#' @noRd
ayce_validate <- function(data_clean, data_raw, pcdi_result = NULL,
                          thresholds = NULL) {

  rlang::inform("=== AYCE Validation & Quality Control ===")

  validation <- list()

  # Taux de retention des points
  retention_rate <- nrow(data_clean) / nrow(data_raw)
  validation$retention_rate <- retention_rate

  rlang::inform(paste("Retention rate:", round(retention_rate * 100, 1), "%"))

  # Signaler les taux de retention inhabituels
  validation$warning <- NULL
  if (retention_rate < 0.5) {
    validation$warning <- "Very low retention rate - check parameters"
    rlang::warn(validation$warning)
  } else if (retention_rate > 0.99) {
    validation$warning <- "Almost no points removed - check if AYCE is working"
    rlang::warn(validation$warning)
  }

  # Verification de la stabilite PCDI
  if (!is.null(pcdi_result) && !is.null(pcdi_result$warning)) {
    validation$pcdi_stable <- is.null(pcdi_result$warning) || pcdi_result$warning == "None"
    if (!validation$pcdi_stable) {
      rlang::warn(paste("PCDI warning:", pcdi_result$warning))
    }
  } else {
    validation$pcdi_stable <- TRUE
  }

  # Comparaison des statistiques de rendement
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

    # Verifier l'amelioration du CV
    cv_improvement <- validation$raw_stats$cv - validation$clean_stats$cv
    validation$cv_improvement <- cv_improvement

    rlang::inform(paste("Raw CV:", round(validation$raw_stats$cv * 100, 1), "%"))
    rlang::inform(paste("Clean CV:", round(validation$clean_stats$cv * 100, 1), "%"))
  }

  return(validation)
}
