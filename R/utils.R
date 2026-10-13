#' @importFrom stats median quantile rnorm sd var step
#' @importFrom utils head modifyList
#' @importFrom rlang .data %||%
NULL

#' Valider la structure d'un jeu de donnees de rendement
#'
#' Verifie qu'un objet est un data frame non vide et possede les colonnes requises.
#' Fournit un message d'erreur clair avec suggestions en cas de colonnes manquantes.
#'
#' @param data Objet a verifier (data.frame, tibble ou sf)
#' @param required_cols Vecteur des noms de colonnes obligatoires (defaut NULL)
#' @param arg_name Nom du parametre dans la fonction appelante (defaut "data")
#' @return Invisible TRUE si valide, sinon leve une erreur informative
#' @export
#' @examples
#' df <- data.frame(Longitude = -73.5, Latitude = 45.5, Flow = 10)
#' validate_yield_dataframe(df, required_cols = c("Longitude", "Latitude"))
validate_yield_dataframe <- function(data, required_cols = NULL, arg_name = "data") {
  if (missing(data) || is.null(data)) {
    rlang::abort(paste0("`", arg_name, "` doit etre fourni."))
  }

  if (!is.data.frame(data)) {
    rlang::abort(paste0("`", arg_name, "` doit etre un data frame ou un tibble (recu: ", class(data)[1], ")."))
  }

  if (nrow(data) == 0) {
    rlang::abort(paste0("`", arg_name, "` ne contient aucune ligne (data frame vide)."))
  }

  if (!is.null(required_cols) && length(required_cols) > 0) {
    missing_cols <- setdiff(required_cols, names(data))
    if (length(missing_cols) > 0) {
      avail <- paste(names(data), collapse = ", ")
      rlang::abort(paste0(
        "Colonne(s) requise(s) manquante(s) dans `", arg_name, "`: ",
        paste(missing_cols, collapse = ", "),
        "\nColonnes disponibles: ", avail
      ))
    }
  }

  invisible(TRUE)
}

#' Division securisee contre les divisions par zero ou NaN
#'
#' @param numerator Numerateur (scalaire ou vecteur)
#' @param denominator Denominateur (scalaire ou vecteur)
#' @param default Valeur par defaut a renvoyer en cas de division impossible (defaut 0)
#' @return Resultat de la division avec default aux positions indefinies
#' @export
#' @examples
#' safe_divide(10, 2)
#' safe_divide(10, 0, default = NA)
safe_divide <- function(numerator, denominator, default = 0) {
  res <- numerator / denominator
  invalid <- is.na(res) | is.nan(res) | is.infinite(res)
  if (any(invalid)) {
    if (length(default) == 1) {
      res[invalid] <- default
    } else {
      res[invalid] <- default[invalid]
    }
  }
  res
}

#' Verifier la validite des coordonnees geographiques
#'
#' @param lon Vecteur de longitudes
#' @param lat Vecteur de latitudes
#' @param crs Code EPSG du CRS (defaut 4326 pour WGS84)
#' @return Vecteur logique indiquant si chaque paire de coordonnees est valide
#' @export
#' @examples
#' is_valid_coordinates(c(-73.5, 200), c(45.5, 95))
is_valid_coordinates <- function(lon, lat, crs = 4326) {
  if (length(lon) != length(lat)) {
    rlang::abort("`lon` et `lat` doivent avoir la meme longueur.")
  }

  valid <- !is.na(lon) & !is.na(lat) & is.finite(lon) & is.finite(lat)

  if (crs == 4326) {
    valid <- valid & (lon >= -180 & lon <= 180) & (lat >= -90 & lat <= 90)
  }

  valid
}

#' Calcul securise de la moyenne
#'
#' @param x Vecteur numerique
#' @param na.rm Supprimer les NA (defaut TRUE)
#' @param default Valeur par defaut si le vecteur est vide (defaut 0)
#' @return Moyenne ou default
#' @export
safe_mean <- function(x, na.rm = TRUE, default = 0) {
  if (length(x) == 0 || (na.rm && all(is.na(x)))) {
    return(default)
  }
  m <- mean(x, na.rm = na.rm)
  if (is.na(m) || !is.finite(m)) default else m
}

#' Calcul securise de la médiane
#'
#' @param x Vecteur numerique
#' @param na.rm Supprimer les NA (defaut TRUE)
#' @param default Valeur par defaut si le vecteur est vide (defaut 0)
#' @return Mediane ou default
#' @export
safe_median <- function(x, na.rm = TRUE, default = 0) {
  if (length(x) == 0 || (na.rm && all(is.na(x)))) {
    return(default)
  }
  m <- stats::median(x, na.rm = na.rm)
  if (is.na(m) || !is.finite(m)) default else m
}

#' Detecter une colonne parmi une liste de candidats possibles
#'
#' Cherche sans sensibilite a la casse ou separateurs une colonne
#' existante parmi une liste de noms alternatifs.
#'
#' @param data Data frame
#' @param candidates Vecteur de chaines de caracteres des noms recherches
#' @param default Valeur par defaut si non trouvee (defaut NULL)
#' @return Le nom exact de la colonne trouvee dans data, ou default
#' @export
#' @examples
#' df <- data.frame(Yield_kg_ha = c(5000, 6000))
#' detect_column(df, c("yield", "flow", "yield_kg_ha"))
detect_column <- function(data, candidates, default = NULL) {
  if (!is.data.frame(data) || length(candidates) == 0) {
    return(default)
  }

  actual_cols <- names(data)
  clean_actual <- tolower(gsub("[_\\s-]", "", actual_cols))
  clean_candidates <- tolower(gsub("[_\\s-]", "", candidates))

  for (cand in clean_candidates) {
    match_idx <- which(clean_actual == cand)
    if (length(match_idx) > 0) {
      return(actual_cols[match_idx[1]])
    }
  }

  default
}