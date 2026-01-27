#' Lire des donnees de rendement brutes depuis un fichier texte
#'
#' Cette fonction lit les donnees brutes de rendement depuis un fichier
#' texte formate selon le standard des fichiers de moissonneuse.
#' Supporte differents formats de fichiers (15-17 colonnes).
#'
#' @param file_path Chemin du fichier texte d'entree
#' @param col_names Logique, si TRUE utilise les noms de colonnes standard
#' @return Un tibble avec les donnees brutes
#' @export
#' @examples
#' # Exemple avec donnees d'exemple (creation d'un fichier temporaire)
#' temp_file <- tempfile(pattern = "yield_data", fileext = ".txt")
#' writeLines(c(
#'   "-69.856661,47.506122,1.53,1762958157,2,77,240,30.8,33,1,2410019049,F0:1,L0:<1>,Maïs,7,0,61.3",
#'   "-69.856681,47.506136,3.7,1762958159,2,87,240,30.9,33,1,2410019049,F0:1,L0:<1>,Maïs,7,0,61.5"
#' ), temp_file)
#'
#' data <- read_yield_data(temp_file)
#' print(data)
read_yield_data <- function(file_path, col_names = TRUE) {
  # Verification du fichier
  if (!file.exists(file_path)) {
    rlang::abort(paste("Le fichier n'existe pas:", file_path))
  }

  # Determiner si le fichier a un en-tete avec ID (format "ID|...")
  first_line <- readLines(file_path, n = 1)
  has_id_prefix <- grepl("^[0-9]+\\|", first_line)

  if (has_id_prefix) {
    # Format avec prefixe ID et separateur |
    data <- readr::read_delim(
      file_path,
      delim = "|",
      col_names = FALSE,
      trim_ws = TRUE,
      show_col_types = FALSE
    )

    # Premiere colonne = ID, seconde = contenu
    if (ncol(data) >= 2 && all(grepl(",", data[[2]]))) {
      # La deuxieme colonne contient les donnees separees par des virgules
      temp_data <- data |>
        dplyr::mutate(temp = .data[[2]]) |>
        dplyr::select(temp) |>
        tidyr::separate(temp, into = c(
          "Longitude", "Latitude", "Flow", "GPS_Time",
          "Interval", "Distance", "Swath", "Moisture",
          "HeaderStatus", "Pass", "Serial", "FieldID",
          "LoadID", "GrainType"
        ), sep = ",", fill = "right", extra = "merge")

      # Compter les colonnes creees et detecter le format
      n_parsed <- ncol(temp_data)

      # Verifier si les dernieres colonnes sont numeriques (Altitude)
      # et si l'avant-derniere est un ID (chaine)
      if (n_parsed >= 15) {
        # Essayer de parser les colonnes 15 et 16 comme valeurs potentielles
        col15 <- suppressWarnings(as.numeric(temp_data[[15]]))
        col16 <- suppressWarnings(as.numeric(temp_data[[16]]))

        if (!is.na(col16[1])) {
          # Colonne 16 numerique = Altitude
          # Colonne 15 = ID ou extra ID
          temp_data <- temp_data |>
            dplyr::mutate(
              Extra_ID = .data[[15]],
              Altitude = as.numeric(.data[[16]])
            ) |>
            dplyr::select(-dplyr::any_of(c("15", "16")))
        } else if (!is.na(col15[1])) {
          # Colonne 15 numerique = Altitude, pas de colonne 16
          temp_data <- temp_data |>
            dplyr::mutate(
              Altitude = as.numeric(.data[[15]])
            ) |>
            dplyr::select(-"15")
        }
      }

      # Ajouter les colonnes manquantes
      if (!"DOP" %in% names(temp_data)) temp_data$DOP <- NA_real_
      if (!"GPSStatus" %in% names(temp_data)) temp_data$GPSStatus <- NA_integer_

      data <- temp_data
    }
  } else {
    # Format simple avec virgules comme separateur
    data <- readr::read_delim(
      file_path,
      delim = ",",
      col_names = FALSE,
      trim_ws = TRUE,
      show_col_types = FALSE
    )

    # Ajuster les noms de colonnes selon le nombre de colonnes
    n_cols <- ncol(data)

    if (n_cols == 17) {
      # Format standard avec Altitude et DOP
      colnames(data) <- c(
        "Longitude", "Latitude", "Flow", "GPS_Time",
        "Interval", "Distance", "Swath", "Moisture",
        "HeaderStatus", "Pass", "Serial", "FieldID",
        "LoadID", "GrainType", "GPSStatus", "DOP", "Altitude"
      )
    } else if (n_cols == 16) {
      # Format avec 16 colonnes - peut avoir Altitude ou pas
      # Verifier si la derniere colonne est numerique (Altitude)
      col16 <- suppressWarnings(as.numeric(data[[16]]))

      if (!is.na(col16[1])) {
        # Colonne 16 numerique = Altitude
        # Colonnes 1-14 : standard, 15 : Variety, 16 : Altitude
        colnames(data) <- c(
          "Longitude", "Latitude", "Flow", "GPS_Time",
          "Interval", "Distance", "Swath", "Moisture",
          "HeaderStatus", "Pass", "Serial", "FieldID",
          "LoadID", "GrainType", "Variety", "Altitude"
        )
      } else {
        # Colonne 16 non numerique, colonne 15 doit etre Altitude
        colnames(data) <- c(
          "Longitude", "Latitude", "Flow", "GPS_Time",
          "Interval", "Distance", "Swath", "Moisture",
          "HeaderStatus", "Pass", "Serial", "FieldID",
          "LoadID", "GrainType", "Altitude", "Extra"
        )
      }
      # Ajouter les colonnes manquantes
      data$DOP <- NA_real_
      data$GPSStatus <- NA_integer_
      # Convertir Serial en caractere
      data$Serial <- as.character(data$Serial)
    } else if (n_cols == 15) {
      # Format court (ex : sample3.txt, sample4.txt)
      # Verifier si la derniere colonne est numerique et ressemble a une altitude (>100)
      col15 <- suppressWarnings(as.numeric(data[[15]]))
      is_altitude <- !is.na(col15[1]) && abs(col15[1]) > 100

      if (is_altitude) {
        # Colonne 15 numerique et ressemble a une altitude
        colnames(data) <- c(
          "Longitude", "Latitude", "Flow", "GPS_Time",
          "Interval", "Distance", "Swath", "Moisture",
          "HeaderStatus", "Pass", "Serial", "FieldID",
          "LoadID", "GrainType", "Altitude"
        )
      } else {
        # Colonne 15 n'est pas une altitude, probablement un ID
        colnames(data) <- c(
          "Longitude", "Latitude", "Flow", "GPS_Time",
          "Interval", "Distance", "Swath", "Moisture",
          "HeaderStatus", "Pass", "Serial", "FieldID",
          "LoadID", "GrainType", "FieldID2"
        )
        data$Altitude <- NA_real_
      }
      # Ajouter les colonnes manquantes
      data$DOP <- NA_real_
      data$GPSStatus <- NA_integer_
    } else {
      # Essayer de mapper generiquement
      std_names <- c(
        "Longitude", "Latitude", "Flow", "GPS_Time",
        "Interval", "Distance", "Swath", "Moisture",
        "HeaderStatus", "Pass", "Serial", "FieldID",
        "LoadID", "GrainType"
      )
      if (n_cols > length(std_names)) {
        colnames(data) <- c(std_names, paste0("extra_", 1:(n_cols - length(std_names))))
      } else {
        colnames(data) <- std_names[1:n_cols]
      }
      # Ajouter les colonnes manquantes
      if (!"DOP" %in% names(data)) data$DOP <- NA
      if (!"GPSStatus" %in% names(data)) data$GPSStatus <- NA
      if (!"Altitude" %in% names(data)) data$Altitude <- NA
    }
  }

  # Conversion des types - approche simple pour compatibilite
  num_cols <- c("Longitude", "Latitude", "Flow", "Moisture", "DOP", "Altitude")
  int_cols <- c("GPS_Time", "Interval", "Distance", "Swath", "HeaderStatus", "Pass", "Serial", "GPSStatus")
  char_cols <- c("FieldID", "LoadID", "GrainType")
  
  for (col in num_cols) {
    if (col %in% names(data)) data[[col]] <- as.numeric(data[[col]])
  }
  for (col in int_cols) {
    if (col %in% names(data)) data[[col]] <- as.integer(data[[col]])
  }
  for (col in char_cols) {
    if (col %in% names(data)) data[[col]] <- as.character(data[[col]])
  }
  
  data <- data |>
    dplyr::filter(!is.na(Flow)) |>
    dplyr::filter(Flow >= 0)

  # Ajout d'un index de ligne
  data <- data |> dplyr::mutate(.row_id = dplyr::row_number())

  return(data)
}


#' Parser les noms de colonnes des donnees de rendement
#'
#' Traduit les noms de colonnes du format standard vers des noms descriptifs.
#'
#' @param data Tibble avec donnees de rendement
#' @return Donnees avec noms de colonnes standardises
#' @noRd
parse_column_names <- function(data) {
  # Mapping des colonnes standards
  col_mapping <- c(
    "Longitude" = "Longitude",
    "Latitude" = "Latitude",
    "Grain_Flow" = "Flow",
    "GPS_Time" = "Time",
    "Logging_Interval" = "Interval",
    "Distance" = "Distance",
    "Swath" = "Swath",
    "Moisture" = "Moisture",
    "Header_Status" = "HeaderStatus",
    "Pass" = "Pass",
    "Serial_Number" = "Serial",
    "Field_ID" = "FieldID",
    "Load_ID" = "LoadID",
    "Grain_Type" = "GrainType",
    "GPS_Status" = "GPSStatus",
    "DOP" = "DOP",
    "Altitude" = "Altitude"
  )

  # Renommage des colonnes existantes
  existing_cols <- intersect(names(data), names(col_mapping))
  if (length(existing_cols) > 0) {
    names(data) <- ifelse(
      names(data) %in% names(col_mapping),
      col_mapping[names(data)],
      names(data)
    )
  }

  return(data)
}
