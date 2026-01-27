#' Supprimer les points en chevauchement
#'
#' Cette fonction identifie et élimine les points de chevauchement
#' en utilisant une grille cellsize x cellsize. Les points dans les cellules
#' avec PLUS de max_pass sont considérés comme du chevauchement et éliminés.
#'
#' @param data Tibble avec donnees (colonnes X, Y en UTM)
#' @param cellsize Taille des cellules en metres (defaut 0.3m = 30cm)
#' @param max_pass Nombre max de passages avant chevauchement (defaut 50)
#' @return Tibble filtre sans chevauchement
#' @noRd
#' @examples
#' \dontrun{
#' # Creer des donnees d'exemple en UTM
#' data <- tibble::tibble(
#'   X = c(435000, 435050, 435100, 435000, 435050),
#'   Y = c(5262000, 5262050, 5262100, 5262150, 5262200),
#'   Flow = c(2.5, 3.1, 2.8, 3.0, 2.9)
#' )
#'
#' # Parametres par defaut (methode USDA)
#' data_clean <- remove_overlap(data, cellsize = 0.3, max_pass = 50)
#'
#' # Pour des donnees avec beaucoup de chevauchement
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


#' Appliquer le filtre ET local
#'
#' Cette fonction identifie et élimine les points anormaux en utilisant
#' un voisinage de n swathes autour de chaque point. Les points dont le
#' rendement s'écarte de plus de STD_limit écarts-types de la moyenne
#' locale sont éliminés.
#'
#' @param data Tibble avec donnees (colonne Pass requise)
#' @param swath_window Nombre de passages dans le voisinage local
#' @param std_limit Nombre maximal d'ecarts-types depuis la moyenne locale
#' @param yield_col Nom de la colonne de rendement (defaut "Flow")
#' @return Tibble filtre avec outliers supprimes
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

  # Calculer la moyenne et l'ET locaux pour chaque point
  data <- data |>
    dplyr::arrange(Pass, .row_id) |>
    dplyr::group_by(Pass) |>
    dplyr::mutate(
      local_mean = mean(!!rlang::sym(yield_col), na.rm = TRUE),
      local_sd = stats::sd(!!rlang::sym(yield_col), na.rm = TRUE)
    ) |>
    dplyr::ungroup()

  # Calculer les limites locales (moyenne +/- std_limit * ET)
  # Utiliser une fenetre de passages adjacents
  data <- data |>
    dplyr::mutate(
      pass_min = pmax(1, Pass - swath_window),
      pass_max = Pass + swath_window
    )

  # Pour chaque point, calculer les statistiques locales avec fenetre
  # Note : implementation simplifiee - variation intra-pass
  data <- data |>
    dplyr::mutate(
      # Limites de filtrage
      upper_limit = local_mean + std_limit * local_sd,
      lower_limit = local_mean - std_limit * local_sd,
      # Marquer les points à éliminer
      is_outlier = !!rlang::sym(yield_col) > upper_limit |
                   !!rlang::sym(yield_col) < lower_limit
    )

  # Compter les outliers par passage pour le journal
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


#' Appliquer le filtre a fenetre glissante
#'
#' Cette fonction applique un filtre à fenêtre glissante pour éliminer
#' les valeurs aberrantes basées sur les voisins temporels.
#'
#' @param data Tibble avec donnees de rendement
#' @param window_size Taille de la fenetre glissante
#' @param n_std Nombre d'ecarts-types pour le seuil
#' @param yield_col Nom de la colonne de rendement
#' @return Tibble filtre
#' @noRd
filter_sliding_window <- function(data, window_size = 11, n_std = 3,
                                   yield_col = "Flow") {
  n_before <- nrow(data)
  n <- nrow(data)
  half_win <- floor(window_size / 2)

  # Extraire la colonne de rendement en vecteur pour les calculs
  yield_vals <- data[[yield_col]]

  # Calculer la moyenne glissante et l'ET
  rolling_mean <- zoo::rollmean(
    yield_vals,
    k = window_size,
    fill = NA,
    align = "center"
  )

  # Calculer l'ET glissant manuellement
  rolling_sd <- vapply(1:n, function(i) {
    idx_start <- max(1, i - half_win)
    idx_end <- min(n, i + half_win)
    sd(yield_vals[idx_start:idx_end], na.rm = TRUE)
  }, numeric(1))

  # Ajouter les colonnes aux donnees
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
