#' Read raw yield data from text file
#'
#' Cette fonction lit les données brutes de rendement depuis un fichier
#' texte formaté selon le standard des fichiers de moissonneuse.
#' Supporte différents formats de fichiers (15-17 colonnes).
#'
#' @param file_path Path to the input text file
#' @param col_names Logical, if TRUE uses standard column names
#' @return A tibble with the raw data
#' @export
#' @examples
#' # Example with sample data (create temp file for demo)
#' temp_file <- tempfile(pattern = "yield_data", fileext = ".txt")
#' writeLines(c(
#'   "-69.856661,47.506122,1.53,1762958157,2,77,240,30.8,33,1,2410019049,F0:1,L0:<1>,Maïs,7,0,61.3",
#'   "-69.856681,47.506136,3.7,1762958159,2,87,240,30.9,33,1,2410019049,F0:1,L0:<1>,Maïs,7,0,61.5"
#' ), temp_file)
#'
#' data <- read_yield_data(temp_file)
#' print(data)
read_yield_data <- function(file_path, col_names = TRUE) {
  # Vérification du fichier
  if (!file.exists(file_path)) {
    rlang::abort(paste("Le fichier n'existe pas:", file_path))
  }

  # Déterminer si le fichier a un en-tête avec ID (format "ID|...")
  first_line <- readLines(file_path, n = 1)
  has_id_prefix <- grepl("^[0-9]+\\|", first_line)

  if (has_id_prefix) {
    # Format avec préfixe ID et séparateur |
    data <- readr::read_delim(
      file_path,
      delim = "|",
      col_names = FALSE,
      trim_ws = TRUE,
      show_col_types = FALSE
    )

    # Première colonne contient l'ID, la seconde contient le reste
    if (ncol(data) >= 2 && all(grepl(",", data[[2]]))) {
      # La deuxième colonne contient les données séparées par des virgules
      temp_data <- data |>
        dplyr::mutate(temp = .data[[2]]) |>
        dplyr::select(temp) |>
        tidyr::separate(temp, into = c(
          "Longitude", "Latitude", "Flow", "GPS_Time",
          "Interval", "Distance", "Swath", "Moisture",
          "HeaderStatus", "Pass", "Serial", "FieldID",
          "LoadID", "GrainType"
        ), sep = ",", fill = "right", extra = "merge")

      # Compter les colonnes créées et détecter le format
      n_parsed <- ncol(temp_data)

      # Vérifier si les dernières colonnes sont numériques (Altitude)
      # et si la colonne avant dernière est un ID (string)
      if (n_parsed >= 15) {
        # Essayer de parser la colonne 15 et 16 comme valeurs potentielles
        col15 <- suppressWarnings(as.numeric(temp_data[[15]]))
        col16 <- suppressWarnings(as.numeric(temp_data[[16]]))

        if (!is.na(col16[1])) {
          # Colonne 16 est numérique = Altitude
          # Colonne 15 est probablement un ID ou extra ID
          temp_data <- temp_data |>
            dplyr::mutate(
              Extra_ID = .data[[15]],
              Altitude = as.numeric(.data[[16]])
            ) |>
            dplyr::select(-dplyr::any_of(c("15", "16")))
        } else if (!is.na(col15[1])) {
          # Colonne 15 est numérique = Altitude, pas de colonne 16
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
    # Format simple avec virgules comme séparateur
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
      # Vérifier si la dernière colonne est numérique (Altitude)
      col16 <- suppressWarnings(as.numeric(data[[16]]))

      if (!is.na(col16[1])) {
        # Colonne 16 est numérique = Altitude
        # Colonnes 1-14: standard, colonne 15: Variety, colonne 16: Altitude
        colnames(data) <- c(
          "Longitude", "Latitude", "Flow", "GPS_Time",
          "Interval", "Distance", "Swath", "Moisture",
          "HeaderStatus", "Pass", "Serial", "FieldID",
          "LoadID", "GrainType", "Variety", "Altitude"
        )
      } else {
        # Colonne 16 n'est pas numérique, colonne 15 doit être Altitude
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
      # Convertir Serial en caractère
      data$Serial <- as.character(data$Serial)
    } else if (n_cols == 15) {
      # Format court (ex: sample3.txt, sample4.txt)
      # Vérifier si la dernière colonne est numérique et ressemble à une altitude (>1000)
      col15 <- suppressWarnings(as.numeric(data[[15]]))
      is_altitude <- !is.na(col15[1]) && abs(col15[1]) > 100

      if (is_altitude) {
        # Colonne 15 est numérique et ressemble à une altitude
        colnames(data) <- c(
          "Longitude", "Latitude", "Flow", "GPS_Time",
          "Interval", "Distance", "Swath", "Moisture",
          "HeaderStatus", "Pass", "Serial", "FieldID",
          "LoadID", "GrainType", "Altitude"
        )
      } else {
        # Colonne 15 n'est pas altitude, c'est probablement un ID
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
      # Essayer de mapper génériquement
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

  # Conversion des types - simple approach for compatibility
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


#' Parse yield data column names
#'
#' Traduit les noms de colonnes du format standard vers des noms descriptifs.
#'
#' @param data A tibble with yield data
#' @return Data with standardized column names
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
