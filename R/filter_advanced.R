#' Remove overlapping points
#'
#' Cette fonction identifie et élimine les points de chevauchement
#' en utilisant une grille cellsize x cellsize. Les points dans les cellules
#' avec PLUS de max_pass sont considérés comme du chevauchement et éliminés.
#'
#' @param data A tibble with yield data (must have X, Y columns in UTM)
#' @param cellsize Size of grid cells in meters (default 0.3m = 30cm)
#' @param max_pass Maximum number of passes before considering overlap (default 50)
#' @return Filtered tibble without overlaps
#' @noRd
#' @examples
#' \dontrun{
#' # Create example data with UTM coordinates
#' data <- tibble::tibble(
#'   X = c(435000, 435050, 435100, 435000, 435050),
#'   Y = c(5262000, 5262050, 5262100, 5262150, 5262200),
#'   Flow = c(2.5, 3.1, 2.8, 3.0, 2.9)
#' )
#'
#' # Paramètres par défaut (méthode USDA)
#' data_clean <- remove_overlap(data, cellsize = 0.3, max_pass = 50)
#'
#' # Pour des données avec beaucoup de chevauchement
#' data_clean <- remove_overlap(data, cellsize = 0.3, max_pass = 30)
#' }
remove_overlap <- function(data, cellsize = 0.3, max_pass = 50) {
  if (!all(c("X", "Y") %in% names(data))) {
    rlang::abort("Les colonnes X et Y (UTM) sont requises pour le filtrage overlap")
  }

  n_before <- nrow(data)

  # Créer un ID de cellule basé sur la grille
  data <- data |>
    dplyr::mutate(
      cell_id_x = floor(X / cellsize),
      cell_id_y = floor(Y / cellsize),
      cell_id = paste(cell_id_x, cell_id_y, sep = "_")
    )

  # Compter le nombre de points par cellule
  cell_counts <- data |>
    dplyr::count(cell_id, name = "cell_count")

  # Joindre et filtrer - éliminer les cellules avec TROP de passages (chevauchement)
  data <- data |>
    dplyr::left_join(cell_counts, by = "cell_id") |>
    dplyr::filter(cell_count <= max_pass) |>
    dplyr::select(-cell_id_x, -cell_id_y, -cell_id, -cell_count)

  n_removed <- n_before - nrow(data)

  rlang::inform(paste(
    "Overlap filter complete: Cellsize:", cellsize,
    "Max Pass:", max_pass,
    "-", n_removed, "points éliminés"
  ))

  return(data)
}


#' Apply local STD filter
#'
#' Cette fonction identifie et élimine les points anormaux en utilisant
#' un voisinage de n swathes autour de chaque point. Les points dont le
#' rendement s'écarte de plus de STD_limit écarts-types de la moyenne
#' locale sont éliminés.
#'
#' @param data A tibble with yield data (must have Pass column)
#' @param swath_window Number of swaths to include in local neighborhood
#' @param std_limit Maximum number of standard deviations from local mean
#' @param yield_col Name of yield column (default "Flow")
#' @return Filtered tibble with outliers removed
#' @noRd
#' @examples
#' \dontrun{
#' data_clean <- filter_local_std(data, swath_window = 5, std_limit = 3)
#' }
filter_local_std <- function(data, swath_window = 5, std_limit = 3,
                              yield_col = "Flow") {
  if (!"Pass" %in% names(data)) {
    rlang::warn("Colonne Pass non trouvée, saut du filtrage STD local")
    return(data)
  }

  n_before <- nrow(data)

  # Calculer la moyenne et STD locales pour chaque point
  data <- data |>
    dplyr::arrange(Pass, .row_id) |>
    dplyr::group_by(Pass) |>
    dplyr::mutate(
      local_mean = mean(!!rlang::sym(yield_col), na.rm = TRUE),
      local_sd = stats::sd(!!rlang::sym(yield_col), na.rm = TRUE)
    ) |>
    dplyr::ungroup()

  # Calculer les limites locale (moyenne +/- std_limit * SD)
  # Utiliser une fenêtre de passages adjacents
  data <- data |>
    dplyr::mutate(
      pass_min = pmax(1, Pass - swath_window),
      pass_max = Pass + swath_window
    )

  # Pour chaque point, calculer les statistiques locales avec fenêtre
  # Note: implémentation simplifiée - utilise la variation intra-pass
  data <- data |>
    dplyr::mutate(
      # Limites de filtrage
      upper_limit = local_mean + std_limit * local_sd,
      lower_limit = local_mean - std_limit * local_sd,
      # Marquer les points à éliminer
      is_outlier = !!rlang::sym(yield_col) > upper_limit |
                   !!rlang::sym(yield_col) < lower_limit
    )

  # Compter les outliers par pass pour le log
  outliers_summary <- data |>
    dplyr::filter(is_outlier) |>
    dplyr::count(Pass, name = "n_outliers")

  # Filtrer
  data <- data |>
    dplyr::filter(!is_outlier) |>
    dplyr::select(-local_mean, -local_sd, -pass_min, -pass_max,
                  -upper_limit, -lower_limit, -is_outlier)

  n_removed <- n_before - nrow(data)

  rlang::inform(paste(
    "Local STD Filter complete: Swath :", swath_window,
    "STDLimit :", std_limit,
    "-", n_removed, "points éliminés"
  ))

  return(data)
}


#' Apply sliding window filter
#'
#' Cette fonction applique un filtre à fenêtre glissante pour éliminer
#' les valeurs aberrantes basées sur les voisins temporels.
#'
#' @param data A tibble with yield data
#' @param window_size Size of sliding window
#' @param n_std Number of standard deviations for threshold
#' @param yield_col Name of yield column
#' @return Filtered tibble
#' @noRd
filter_sliding_window <- function(data, window_size = 11, n_std = 3,
                                   yield_col = "Flow") {
  n_before <- nrow(data)
  n <- nrow(data)
  half_win <- floor(window_size / 2)

  # Extract the yield column as a vector for calculations
  yield_vals <- data[[yield_col]]

  # Calculate rolling mean and SD
  rolling_mean <- zoo::rollmean(
    yield_vals,
    k = window_size,
    fill = NA,
    align = "center"
  )

  # Calculate rolling SD manually
  rolling_sd <- vapply(1:n, function(i) {
    idx_start <- max(1, i - half_win)
    idx_end <- min(n, i + half_win)
    sd(yield_vals[idx_start:idx_end], na.rm = TRUE)
  }, numeric(1))

  # Add columns to data
  data <- data |>
    dplyr::arrange(.row_id) |>
    dplyr::mutate(
      rolling_mean = rolling_mean,
      rolling_sd = rolling_sd
    )

  # Identifier les outliers
  data <- data |>
    dplyr::mutate(
      is_outlier = dplyr::case_when(
        is.na(rolling_mean) ~ FALSE,
        (!!rlang::sym(yield_col) > rolling_mean + n_std * rolling_sd) ~ TRUE,
        (!!rlang::sym(yield_col) < rolling_mean - n_std * rolling_sd) ~ TRUE,
        TRUE ~ FALSE
      )
    )

  # Filtrer
  data <- data |>
    dplyr::filter(!is_outlier) |>
    dplyr::select(-rolling_mean, -rolling_sd, -is_outlier)

  n_removed <- n_before - nrow(data)

  rlang::inform(paste(
    "Sliding window filter:", window_size, "points de fenêtre,",
    n_std, "STD -", n_removed, "points éliminés"
  ))

  return(data)
}
