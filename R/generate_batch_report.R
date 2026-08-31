#' Générer un rapport multi-champs à partir de fichiers de données
#'
#' Cette fonction traite un ou plusieurs fichiers de données de rendement
#' (ZIP, CSV, TXT, ou GeoJSON), nettoie les données de chaque champ, et génère un
#' rapport professionnel avec tous les champs organisés par année.
#'
#' @param file_paths chemin(s) vers le(s) fichier(s) de données.
#'   Formats supportés: ZIP (contenant des shapefiles), CSV, TXT, GeoJSON
#' @param output_file chemin du fichier de sortie (optionnel).
#'   Par défaut, le rapport est créé dans le même répertoire que le premier fichier.
#' @param title Titre du rapport (optionnel)
#' @param output_format format de sortie: "pdf" (défaut) ou "html"
#' @param style Style visuel du rapport: "irda" (défaut) ou "ced" (Cedric Bouffard)
#' @return Chemin du fichier généré (invisible)
#' @export
#' @examples
#' \dontrun{
#' # Un seul ZIP vers PDF
#' generate_batch_report("RDT2025.zip")
#'
#' # Vers HTML avec style Cedric Bouffard
#' generate_batch_report("RDT2025.zip", output_format = "html", style = "ced")
#'
#' # Plusieurs fichiers vers HTML
#' generate_batch_report(c("field1.geojson", "field2.geojson"), output_format = "html")
#' }
generate_batch_report <- function(file_paths, output_file = NULL, title = NULL, output_format = c("pdf", "html"), style = c("irda", "ced")) {
  output_format <- match.arg(output_format)
  style <- match.arg(style)
  
  # S'assurer que file_paths est un vecteur
  file_paths <- as.character(file_paths)

  # Vérifier que tous les fichiers existent
  missing_files <- file_paths[!file.exists(file_paths)]
  if (length(missing_files) > 0) {
    rlang::abort(paste("Fichier(s) non trouve(s):", paste(missing_files, collapse = ", ")))
  }

  # Vérifier les extensions supportées
  valid_extensions <- c("zip", "csv", "txt", "geojson")
  file_extensions <- tolower(tools::file_ext(file_paths))
  invalid_files <- file_paths[!file_extensions %in% valid_extensions]
  if (length(invalid_files) > 0) {
    rlang::abort(paste("Format(s) non supporte(s):", paste(invalid_files, collapse = ", "),
                       "\nFormats acceptes: zip, csv, txt, geojson"))
  }

  # Définir le fichier de sortie par défaut
  if (is.null(output_file)) {
    ext <- if (output_format == "html") "html" else "pdf"
    if (length(file_paths) == 1) {
      output_file <- file.path(dirname(file_paths[1]),
                                paste0(tools::file_path_sans_ext(basename(file_paths[1])), "_rapport.", ext))
    } else {
      output_file <- file.path(dirname(file_paths[1]), paste0("rapport_rendements.", ext))
    }
  }

  # Définir le titre par défaut
  if (is.null(title)) {
    if (length(file_paths) == 1) {
      title <- paste0("Rapport de rendements - ", tools::file_path_sans_ext(basename(file_paths[1])))
    } else {
      title <- "Rapport de rendements"
    }
  }

  # Créer le répertoire de travail temporaire
  work_dir <- tempfile("yield_processing_")
  dir.create(work_dir, recursive = TRUE)
  on.exit(unlink(work_dir, recursive = TRUE), add = TRUE)

  # Séparer les fichiers par type
  zip_files <- file_paths[file_extensions == "zip"]
  text_files <- file_paths[file_extensions %in% c("csv", "txt")]
  geojson_files <- file_paths[file_extensions == "geojson"]

  # Traiter tous les fichiers et combiner les champs
  all_fields_data <- list()

  # Traiter les fichiers ZIP
  for (zip_path in zip_files) {
    rlang::inform(paste("\n========================================"))
    rlang::inform(paste("Traitement du ZIP:", basename(zip_path)))
    rlang::inform(paste("========================================"))

    fields_data <- .process_zip_fields(zip_path, work_dir)

    if (length(fields_data) > 0) {
      for (field_name in names(fields_data)) {
        unique_key <- paste0(tools::file_path_sans_ext(basename(zip_path)), "_", field_name)
        all_fields_data[[unique_key]] <- fields_data[[field_name]]
      }
    }
  }

  # Traiter les fichiers CSV/TXT
  for (text_file in text_files) {
    rlang::inform(paste("\n========================================"))
    rlang::inform(paste("Traitement du fichier:", basename(text_file)))
    rlang::inform(paste("========================================"))

    field_data <- .process_text_file(text_file, work_dir)

    if (!is.null(field_data)) {
      unique_key <- tools::file_path_sans_ext(basename(text_file))
      all_fields_data[[unique_key]] <- field_data
    }
  }

  # Traiter les fichiers GeoJSON (données déjà nettoyées)
  for (geojson_file in geojson_files) {
    rlang::inform(paste("\n========================================"))
    rlang::inform(paste("Traitement du GeoJSON:", basename(geojson_file)))
    rlang::inform(paste("========================================"))

    field_data <- .process_geojson_file(geojson_file, work_dir)

    if (!is.null(field_data)) {
      unique_key <- tools::file_path_sans_ext(basename(geojson_file))
      all_fields_data[[unique_key]] <- field_data
    }
  }

  if (length(all_fields_data) == 0) {
    rlang::abort("Aucun champ valide trouve dans les ZIP")
  }

  rlang::inform(paste("\n========================================"))
  rlang::inform(paste("Total:", length(all_fields_data), "champs a inclure dans le rapport"))
  rlang::inform(paste("========================================"))

  # Générer le rapport
  result <- .generate_multi_field_report(all_fields_data, output_file, title, work_dir, output_format, style)

  invisible(result)
}


# ============================================================================
# Fonctions internes
# ============================================================================

#' Traduire le nom de la culture en français
#' @noRd
.translate_crop <- function(crop_name) {
  if (is.na(crop_name) || crop_name == "") return("")

  crop_lower <- tolower(crop_name)
  translations <- list(
    "corn" = "Mais",
    "maize" = "Mais",
    "soybean" = "Soya",
    "soybeans" = "Soya",
    "soy" = "Soya",
    "wheat" = "Ble",
    "barley" = "Orge",
    "oats" = "Avoine",
    "canola" = "Canola",
    "rapeseed" = "Canola",
    "alfalfa" = "Luzerne"
  )

  if (crop_lower %in% names(translations)) {
    return(translations[[crop_lower]])
  } else {
    return(paste0(toupper(substring(crop_name, 1, 1)), substring(crop_name, 2)))
  }
}

#' Extraire la date de récolte des données
#' @noRd
.extract_harvest_date <- function(data, metadata) {
  harvest_date <- NA_character_

  # Essayer depuis les métadonnées
  if (!is.null(metadata) && !is.null(metadata$field_info$date)) {
    harvest_date <- metadata$field_info$date
    if (!is.na(harvest_date) && grepl("T", harvest_date)) {
      tryCatch({
        harvest_date <- as.character(as.Date(as.POSIXct(harvest_date, format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC")))
      }, error = function(e) NULL)
    }
  }

  # Fallback: IsoTime dans les données
  if (is.na(harvest_date) || harvest_date == "") {
    if ("IsoTime" %in% names(data)) {
      tryCatch({
        first_time <- data$IsoTime[1]
        if (!is.na(first_time) && first_time != "") {
          harvest_date <- as.character(as.Date(as.POSIXct(first_time, format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC")))
        }
      }, error = function(e) NULL)
    }
  }

  # Fallback: Time dans les données
  if (is.na(harvest_date) || harvest_date == "") {
    if ("Time" %in% names(data)) {
      tryCatch({
        first_time <- data$Time[1]
        if (!is.na(first_time) && first_time != "") {
          harvest_date <- as.character(as.Date(first_time, format = "%m/%d/%Y"))
        }
      }, error = function(e) NULL)
    }
  }

  return(harvest_date)
}

#' Traiter un fichier CSV/TXT et extraire les informations du champ
#' @noRd
.process_text_file <- function(file_path, output_dir = tempdir()) {
  field_name <- tools::file_path_sans_ext(basename(file_path))

  rlang::inform(paste("--- Traitement du fichier:", field_name, "---"))


  tryCatch({
    # Lire les données avec read_yield_data
    rlang::inform("  Lecture du fichier...")
    data <- read_yield_data(file_path)

    if (is.null(data) || nrow(data) == 0) {
      rlang::warn(paste("  Pas de donnees valides pour", field_name))
      return(NULL)
    }

    rlang::inform(paste(" ", nrow(data), "points lus"))

    # Récupérer les métadonnées
    metadata <- attr(data, "jd_metadata")

    # Vérifier si Yield_kg_ha existe, sinon convertir
    if (!"Yield_kg_ha" %in% names(data)) {
      rlang::inform("  Yield_kg_ha manquant, tentative de conversion...")
      if (all(c("Flow", "Interval", "Swath", "Distance") %in% names(data))) {
        data <- convert_flow_to_yield(data)
        rlang::inform("  Conversion OK")
      } else {
        yield_cols <- grep("yield|rend|yld", names(data), ignore.case = TRUE, value = TRUE)
        if (length(yield_cols) > 0) {
          data$Yield_kg_ha <- as.numeric(data[[yield_cols[1]]])
          rlang::inform(paste("  Utilisation de", yield_cols[1]))
        } else {
          rlang::warn("  Impossible de trouver une colonne de rendement")
          return(NULL)
        }
      }
    }

    # Nettoyer les données
    rlang::inform("  Nettoyage des donnees...")
    result <- clean_yield_fast(data, phase = "full")

    if (is.null(result) || is.null(result$data) || nrow(result$data) == 0) {
      rlang::warn(paste("  Pas de donnees apres nettoyage pour", field_name))
      return(NULL)
    }

    cleaned_data <- result$data
    n_raw <- nrow(data)
    n_clean <- nrow(cleaned_data)

    # Extraire les informations du champ
    field_info <- list(
      field_name = if (!is.null(metadata$field_info$field) && !is.na(metadata$field_info$field)) {
        metadata$field_info$field
      } else {
        field_name
      },
      farm_name = if (!is.null(metadata$field_info$farm)) metadata$field_info$farm else NA_character_,
      crop_name = .translate_crop(if (!is.null(metadata$crop_info$crop_name)) metadata$crop_info$crop_name else ""),
      season_year = if (!is.null(metadata$field_info$season)) metadata$field_info$season else NA_integer_,
      harvest_date = .extract_harvest_date(cleaned_data, metadata),
      n_raw = n_raw,
      n_clean = n_clean,
      n_deleted = n_raw - n_clean,
      retention_rate = if (n_raw > 0) n_clean / n_raw * 100 else 0
    )

    # Calculer les statistiques de rendement
    if ("Yield_kg_ha" %in% names(cleaned_data)) {
      valid_yield <- cleaned_data$Yield_kg_ha[!is.na(cleaned_data$Yield_kg_ha)]
      if (length(valid_yield) > 0) {
        field_info$yield_mean <- mean(valid_yield, na.rm = TRUE)
        field_info$yield_sd <- sd(valid_yield, na.rm = TRUE)
        field_info$yield_min <- min(valid_yield, na.rm = TRUE)
        field_info$yield_max <- max(valid_yield, na.rm = TRUE)
        field_info$yield_median <- median(valid_yield, na.rm = TRUE)
      } else {
        field_info$yield_mean <- 0
        field_info$yield_sd <- 0
        field_info$yield_min <- 0
        field_info$yield_max <- 0
        field_info$yield_median <- 0
      }
    }

    # Calculer l'humidité moyenne
    moisture_col <- NULL
    for (col in names(cleaned_data)) {
      if (grepl("(?i)moisture|humid|water", col)) {
        moisture_col <- col
        break
      }
    }
    field_info$moisture_mean <- if (!is.null(moisture_col)) mean(cleaned_data[[moisture_col]], na.rm = TRUE) else NA_real_

    # Déterminer l'année
    if (!is.na(field_info$harvest_date) && field_info$harvest_date != "") {
      field_info$year <- as.integer(format(as.Date(field_info$harvest_date), "%Y"))
    } else if (!is.na(field_info$season_year)) {
      field_info$year <- as.integer(field_info$season_year)
    } else {
      field_info$year <- as.integer(format(Sys.Date(), "%Y"))
    }

    # Sauvegarder les données nettoyées en GeoJSON temporaire
    geojson_file <- file.path(output_dir, paste0("field_", gsub("[^a-zA-Z0-9]", "_", field_name), ".geojson"))
    sf::st_write(cleaned_data, geojson_file, delete_dsn = TRUE, quiet = TRUE)

    rlang::inform(paste("  OK:", field_info$n_clean, "points, rendement moyen:",
                        round(field_info$yield_mean/1000, 2), "t/ha"))

    return(list(
      info = field_info,
      data = cleaned_data,
      geojson_path = geojson_file
    ))

  }, error = function(e) {
    rlang::warn(paste("  ERREUR pour", field_name, ":", e$message))
    return(NULL)
  })
}

#' Traiter un fichier GeoJSON et extraire les informations du champ
#' @noRd
.process_geojson_file <- function(file_path, output_dir = tempdir()) {
  field_name <- tools::file_path_sans_ext(basename(file_path))

  rlang::inform(paste("--- Traitement du GeoJSON:", field_name, "---"))

  tryCatch({
    # Lire le fichier GeoJSON
    rlang::inform("  Lecture du fichier GeoJSON...")
    data <- sf::st_read(file_path, quiet = TRUE)

    if (is.null(data) || nrow(data) == 0) {
      rlang::warn(paste("  Pas de donnees valides pour", field_name))
      return(NULL)
    }

    rlang::inform(paste(" ", nrow(data), "geometries lues"))

    # Tenter de lire les métadonnées depuis un fichier JSON accompagnant
    metadata <- NULL
    base_name <- tools::file_path_sans_ext(file_path)
    metadata_file <- paste0(base_name, "_metadata.json")
    if (file.exists(metadata_file)) {
      rlang::inform("  Lecture des metadonnees depuis le fichier JSON...")
      metadata <- jsonlite::fromJSON(metadata_file)
      rlang::inform(paste("  Metadonnees lues: champ =", metadata$field_info$field, 
                          ", culture =", metadata$crop_info$crop_name))
    } else {
      # Essayer de recuperer depuis les attributs de l'objet sf
      metadata <- attr(data, "jd_metadata")
    }

    # S'assurer que Yield_kg_ha existe
    if (!"Yield_kg_ha" %in% names(data)) {
      # Chercher une colonne de rendement alternative
      yield_cols <- grep("yield|rend|yld", names(data), ignore.case = TRUE, value = TRUE)
      if (length(yield_cols) > 0) {
        data$Yield_kg_ha <- as.numeric(data[[yield_cols[1]]])
        rlang::inform(paste("  Utilisation de", yield_cols[1], "comme colonne de rendement"))
      } else {
        rlang::warn("  Colonne de rendement non trouvee dans le GeoJSON")
        return(NULL)
      }
    }

    # Les données GeoJSON sont considérées comme déjà nettoyées
    # Extraire les statistiques directement
    n_clean <- nrow(data)
    n_raw <- n_clean  # Pour GeoJSON, on suppose que c'est déjà le résultat du nettoyage

    # Extraire les informations du champ
    field_info <- list(
      field_name = if (!is.null(metadata$field_info$field) && !is.na(metadata$field_info$field)) {
        metadata$field_info$field
      } else {
        field_name
      },
      farm_name = if (!is.null(metadata$field_info$farm)) metadata$field_info$farm else NA_character_,
      crop_name = .translate_crop(if (!is.null(metadata$crop_info$crop_name)) metadata$crop_info$crop_name else ""),
      season_year = if (!is.null(metadata$field_info$season)) metadata$field_info$season else NA_integer_,
      harvest_date = .extract_harvest_date(data, metadata),
      n_raw = n_raw,
      n_clean = n_clean,
      n_deleted = 0,  # Pour GeoJSON, pas d'information sur les points supprimés
      retention_rate = 100  # Pour GeoJSON, taux de rétention de 100%
    )

    # Calculer les statistiques de rendement
    if ("Yield_kg_ha" %in% names(data)) {
      valid_yield <- data$Yield_kg_ha[!is.na(data$Yield_kg_ha)]
      if (length(valid_yield) > 0) {
        field_info$yield_mean <- mean(valid_yield, na.rm = TRUE)
        field_info$yield_sd <- sd(valid_yield, na.rm = TRUE)
        field_info$yield_min <- min(valid_yield, na.rm = TRUE)
        field_info$yield_max <- max(valid_yield, na.rm = TRUE)
        field_info$yield_median <- median(valid_yield, na.rm = TRUE)
      } else {
        field_info$yield_mean <- 0
        field_info$yield_sd <- 0
        field_info$yield_min <- 0
        field_info$yield_max <- 0
        field_info$yield_median <- 0
      }
    }

    # Calculer l'humidité moyenne
    moisture_col <- NULL
    for (col in names(data)) {
      if (grepl("(?i)moisture|humid|water", col)) {
        moisture_col <- col
        break
      }
    }
    field_info$moisture_mean <- if (!is.null(moisture_col)) mean(data[[moisture_col]], na.rm = TRUE) else NA_real_

    # Déterminer l'année
    if (!is.na(field_info$harvest_date) && field_info$harvest_date != "") {
      field_info$year <- as.integer(format(as.Date(field_info$harvest_date), "%Y"))
    } else if (!is.na(field_info$season_year)) {
      field_info$year <- as.integer(field_info$season_year)
    } else {
      field_info$year <- as.integer(format(Sys.Date(), "%Y"))
    }

    # Copier le GeoJSON dans le répertoire de travail
    geojson_file <- file.path(output_dir, paste0("field_", gsub("[^a-zA-Z0-9]", "_", field_name), ".geojson"))
    file.copy(file_path, geojson_file, overwrite = TRUE)

    rlang::inform(paste("  OK:", field_info$n_clean, "points, rendement moyen:",
                        round(field_info$yield_mean/1000, 2), "t/ha"))

    return(list(
      info = field_info,
      data = data,
      geojson_path = geojson_file
    ))

  }, error = function(e) {
    rlang::warn(paste("  ERREUR pour", field_name, ":", e$message))
    return(NULL)
  })
}

#' Traiter un fichier ZIP et extraire les informations de tous les champs
#' @noRd
.process_zip_fields <- function(zip_path, output_dir = tempdir()) {
  rlang::inform(paste("=== Traitement du ZIP:", basename(zip_path), "==="))

  # Utiliser la fonction du package pour lister les champs (shapefiles)
  fields_list <- list_fields_from_zip(zip_path)

  if (nrow(fields_list) == 0) {
    rlang::inform("Aucun champ (shapefile) trouve dans le ZIP")
    return(list())
  }

  rlang::inform(paste("Champs trouves:", paste(fields_list$field_name, collapse = ", ")))

  fields_data <- list()

  for (i in seq_len(nrow(fields_list))) {
    field_name <- fields_list$field_name[i]

    rlang::inform(paste("\n--- Traitement du champ:", field_name, "---"))

    tryCatch({
      rlang::inform("  Lecture du shapefile...")
      data <- read_yield_from_zip(zip_path, field_name)

      if (is.null(data) || nrow(data) == 0) {
        rlang::inform(paste("  Pas de donnees valides pour", field_name))
        next
      }

      rlang::inform(paste(" ", nrow(data), "points lus"))

      metadata <- attr(data, "jd_metadata")

      # Vérifier si Yield_kg_ha existe
      if (!"Yield_kg_ha" %in% names(data)) {
        rlang::inform("  Yield_kg_ha manquant, tentative de conversion...")
        if (all(c("Flow", "Interval", "Swath", "Distance") %in% names(data))) {
          data <- convert_flow_to_yield(data)
          rlang::inform("  Conversion OK")
        } else {
          yield_cols <- grep("yield|rend|yld", names(data), ignore.case = TRUE, value = TRUE)
          if (length(yield_cols) > 0) {
            data$Yield_kg_ha <- as.numeric(data[[yield_cols[1]]])
            rlang::inform(paste("  Utilisation de", yield_cols[1]))
          }
        }
      }

      rlang::inform("  Nettoyage des donnees...")
      result <- clean_yield_fast(data, phase = "full")

      if (is.null(result) || is.null(result$data) || nrow(result$data) == 0) {
        rlang::inform(paste("  Pas de donnees apres nettoyage pour", field_name))
        next
      }

      cleaned_data <- result$data
      n_raw <- nrow(data)
      n_clean <- nrow(cleaned_data)

      field_info <- list(
        field_name = if (!is.null(metadata$field_info$field) && !is.na(metadata$field_info$field)) {
          metadata$field_info$field
        } else {
          field_name
        },
        farm_name = if (!is.null(metadata$field_info$farm)) metadata$field_info$farm else NA_character_,
        crop_name = .translate_crop(if (!is.null(metadata$crop_info$crop_name)) metadata$crop_info$crop_name else ""),
        season_year = if (!is.null(metadata$field_info$season)) metadata$field_info$season else NA_integer_,
        harvest_date = .extract_harvest_date(cleaned_data, metadata),
        n_raw = n_raw,
        n_clean = n_clean,
        n_deleted = n_raw - n_clean,
        retention_rate = if (n_raw > 0) n_clean / n_raw * 100 else 0
      )

      if ("Yield_kg_ha" %in% names(cleaned_data)) {
        valid_yield <- cleaned_data$Yield_kg_ha[!is.na(cleaned_data$Yield_kg_ha)]
        if (length(valid_yield) > 0) {
          field_info$yield_mean <- mean(valid_yield, na.rm = TRUE)
          field_info$yield_sd <- sd(valid_yield, na.rm = TRUE)
          field_info$yield_min <- min(valid_yield, na.rm = TRUE)
          field_info$yield_max <- max(valid_yield, na.rm = TRUE)
          field_info$yield_median <- median(valid_yield, na.rm = TRUE)
        } else {
          field_info$yield_mean <- 0
          field_info$yield_sd <- 0
          field_info$yield_min <- 0
          field_info$yield_max <- 0
          field_info$yield_median <- 0
        }
      }

      moisture_col <- NULL
      for (col in names(cleaned_data)) {
        if (grepl("(?i)moisture|humid|water", col)) {
          moisture_col <- col
          break
        }
      }
      field_info$moisture_mean <- if (!is.null(moisture_col)) mean(cleaned_data[[moisture_col]], na.rm = TRUE) else NA_real_

      if (!is.na(field_info$harvest_date) && field_info$harvest_date != "") {
        field_info$year <- as.integer(format(as.Date(field_info$harvest_date), "%Y"))
      } else if (!is.na(field_info$season_year)) {
        field_info$year <- as.integer(field_info$season_year)
      } else {
        field_info$year <- as.integer(format(Sys.Date(), "%Y"))
      }

      geojson_file <- file.path(output_dir, paste0("field_", gsub("[^a-zA-Z0-9]", "_", field_name), ".geojson"))
      sf::st_write(cleaned_data, geojson_file, delete_dsn = TRUE, quiet = TRUE)

      fields_data[[field_name]] <- list(
        info = field_info,
        data = cleaned_data,
        geojson_path = geojson_file
      )

      rlang::inform(paste("  OK:", field_info$n_clean, "points, rendement moyen:",
                          round(field_info$yield_mean/1000, 2), "t/ha"))

    }, error = function(e) {
      rlang::warn(paste("  ERREUR pour", field_name, ":", e$message))
    })
  }

  rlang::inform(paste("\n===", length(fields_data), "champs traites avec succes ==="))

  return(fields_data)
}

#' Générer le contenu Rmd pour un champ
#' @noRd
.generate_field_rmd <- function(field, is_new_year = FALSE) {
  info <- field$info

  year_label <- as.character(info$year)
  date_label <- if (!is.na(info$harvest_date) && info$harvest_date != "") {
    format(as.Date(info$harvest_date), "%d %b %Y")
  } else {
    year_label
  }
  field_label <- info$field_name
  crop_label <- info$crop_name

  rmd_content <- ""

  if (is_new_year) {
    rmd_content <- paste0(rmd_content, '
::: {.section-break .secondary}

# ', year_label, ' {#year-', year_label, '}

:::

')
  }

  rmd_content <- paste0(rmd_content, '
## ', field_label, '
')

  if (crop_label != "") {
    crop_lower <- tolower(crop_label)
    crop_color <- if (crop_lower %in% c("mais", "corn", "maize")) {
      "#F1C40F"
    } else if (crop_lower %in% c("soya", "soybean", "soy")) {
      "#27AE60"
    } else if (crop_lower %in% c("ble", "orge", "avoine", "wheat", "barley", "oats")) {
      "#F9E79F"
    } else {
      "#9B59B6"
    }
    rmd_content <- paste0(rmd_content, '<span style="background-color:', crop_color,
                          '; color: black; padding: 3px 8px; border-radius: 4px; font-weight: bold;">',
                          crop_label, '</span> ')
  }

  rmd_content <- paste0(rmd_content, '<span class="badge primary">', date_label, '</span>

')

  geojson_escaped <- gsub("\\\\", "/", field$geojson_path)
  chunk_id <- gsub("[^a-zA-Z0-9]", "-", info$field_name)

  rmd_content <- paste0(rmd_content, '
```{r field-', chunk_id, ', include=FALSE}
# Charger les donnees du champ
gdf <- sf::st_read("', geojson_escaped, '", quiet = TRUE)

# Statistiques
yield_mean <- ', info$yield_mean, '
yield_median <- ', info$yield_median, '
yield_sd <- ', info$yield_sd, '
yield_min <- ', info$yield_min, '
yield_max <- ', info$yield_max, '
moisture_mean <- ', ifelse(is.na(info$moisture_mean), 0, info$moisture_mean), '
n_raw <- ', info$n_raw, '
n_clean <- ', info$n_clean, '
n_deleted <- ', info$n_deleted, '
retention_rate <- ', info$retention_rate, '

# Calculer les bins et couleurs
calc_bins_local <- function(gdf) {
  if (!"Yield_kg_ha" %in% names(gdf)) return(NULL)

  valid_yield <- gdf$Yield_kg_ha[!is.na(gdf$Yield_kg_ha) & !is.nan(gdf$Yield_kg_ha) & is.finite(gdf$Yield_kg_ha)]
  if (length(valid_yield) == 0) return(NULL)

  med <- median(valid_yield)
  yield_min_val <- min(valid_yield)
  yield_max_val <- max(valid_yield)

  calc_edge_pct <- function(bin_size, valid_yield, med, yield_min, yield_max) {
    med_bin_start <- floor(med / bin_size) * bin_size
    breaks_custom <- c()
    start_val <- med_bin_start - (3 * bin_size)
    for (i in 0:7) breaks_custom <- c(breaks_custom, start_val + (i * bin_size))
    if (yield_min < breaks_custom[1]) breaks_custom[1] <- yield_min - 1
    if (yield_max > breaks_custom[length(breaks_custom)]) breaks_custom[length(breaks_custom)] <- yield_max + 1
    labels <- 1:(length(breaks_custom)-1)
    yield_cut <- cut(valid_yield, breaks = breaks_custom, labels = labels, include.lowest = TRUE)
    bin_counts <- table(yield_cut)
    bin_pct <- as.numeric(bin_counts) / sum(bin_counts) * 100
    avg_edge <- (bin_pct[1] + bin_pct[length(bin_pct)]) / 2
    list(avg_edge = avg_edge, breaks = breaks_custom, bin_size = bin_size)
  }

  result_05 <- calc_edge_pct(500, valid_yield, med, yield_min_val, yield_max_val)
  message(paste("DEBUG result_05 avg_edge:", result_05$avg_edge))
  if (result_05$avg_edge < 2) {
    bin_size <- 250
  } else if (result_05$avg_edge <= 10) {
    bin_size <- 500
  } else {
    bin_size <- 5000
    message("DEBUG Trying larger bins...")
    for (bs in c(1000, 1500, 2000, 2500, 3000, 3500, 4000, 4500, 5000, 6000, 8000, 10000, 12000, 15000, 20000)) {
      res <- calc_edge_pct(bs, valid_yield, med, yield_min_val, yield_max_val)
      message(paste("DEBUG testing", bs, "-> avg_edge:", res$avg_edge))
      if (res$avg_edge <= 10) { 
        bin_size <- bs
        message(paste("DEBUG FOUND bin_size:", bin_size))
        break 
      }
    }
    if (bin_size == 5000) {
      message("DEBUG Still using 5000 - no bin size worked")
    }
  }
  message(paste("FINAL bin_size:", bin_size))

  med_rounded <- floor(med / bin_size) * bin_size
  breaks_custom <- c()
  for (i in -3:4) breaks_custom <- c(breaks_custom, med_rounded + (i * bin_size))
  if (yield_min_val < breaks_custom[1]) breaks_custom[1] <- floor(yield_min_val / bin_size) * bin_size - bin_size
  if (yield_max_val > breaks_custom[length(breaks_custom)]) breaks_custom[length(breaks_custom)] <- ceiling(yield_max_val / bin_size) * bin_size + bin_size

  n_bins <- length(breaks_custom) - 1
  median_bin_idx <- which(breaks_custom[-length(breaks_custom)] <= med & med < breaks_custom[-1])
  if (length(median_bin_idx) == 0) median_bin_idx <- 4

  labels <- c()
  for (i in 1:n_bins) {
    val1 <- breaks_custom[i] / 1000
    val2 <- breaks_custom[i+1] / 1000
    decimals <- ifelse(bin_size <= 500, 2, ifelse(bin_size <= 1000, 1, 0))
    if (i == 1) labels <- c(labels, paste0("< ", round(val2, decimals), " t/ha"))
    else if (i == n_bins) labels <- c(labels, paste0("> ", round(val1, decimals), " t/ha"))
    else labels <- c(labels, paste0(round(val1, decimals), " - ", round(val2, decimals), " t/ha"))
  }

  bin_colors <- character(n_bins)
  if (median_bin_idx > 1) {
    reds <- colorRampPalette(c("#C0392B", "#E74C3C", "#E67E22"))(median_bin_idx - 1)
    bin_colors[1:(median_bin_idx-1)] <- reds
  }
  bin_colors[median_bin_idx] <- "#F1C40F"
  if (median_bin_idx < n_bins) {
    greens <- colorRampPalette(c("#AED136", "#27AE60", "#1E8449"))(n_bins - median_bin_idx)
    bin_colors[(median_bin_idx+1):n_bins] <- greens
  }

  yield_cut <- cut(valid_yield, breaks = breaks_custom, labels = labels, include.lowest = TRUE)
  bin_counts <- table(yield_cut)
  bin_pct <- round(bin_counts / sum(bin_counts) * 100, 1)

  gdf$yield_bin <- cut(gdf$Yield_kg_ha, breaks = breaks_custom, labels = labels, include.lowest = TRUE)
  bin_areas <- sapply(labels, function(lbl) {
    bin_data <- gdf[gdf$yield_bin == lbl & !is.na(gdf$yield_bin), ]
    if (nrow(bin_data) > 0) round(sum(as.numeric(sf::st_area(bin_data)), na.rm = TRUE) / 10000, 1) else 0
  })

  list(
    bins = data.frame(Classe = labels, Pourcentage = as.numeric(bin_pct), Superficie = bin_areas),
    colors = bin_colors,
    breaks = breaks_custom,
    labels = labels
  )
}

bins_result <- calc_bins_local(gdf)
yield_bins <- bins_result$bins
bin_colors <- bins_result$colors
breaks_custom <- bins_result$breaks
names(bin_colors) <- yield_bins$Classe

valid_idx <- !is.na(gdf$Yield_kg_ha) & !is.nan(gdf$Yield_kg_ha) & is.finite(gdf$Yield_kg_ha)
gdf_valid <- gdf[valid_idx, ]

field_angle <- 0
tryCatch({
  gdf_wgs84 <- sf::st_transform(gdf_valid, 4326)
  coords <- sf::st_coordinates(sf::st_centroid(gdf_wgs84))
  if (nrow(coords) > 1) {
    lat <- mean(coords[,2], na.rm = TRUE)
    meters_per_degree_lon <- 111320 * cos(lat * pi / 180)
    meters_per_degree_lat <- 111320
    dx_m <- diff(coords[,1]) * meters_per_degree_lon
    dy_m <- diff(coords[,2]) * meters_per_degree_lat
    angles <- atan2(dy_m, dx_m) * 180 / pi
    angles_valid <- angles[!is.na(angles) & (abs(dx_m) > 0.1 | abs(dy_m) > 0.1)]
    if (length(angles_valid) > 10) {
      angles_180 <- angles_valid %% 180
      h <- hist(angles_180, breaks = 36, plot = FALSE)
      field_angle <- h$mids[which.max(h$counts)]
    }
  }
}, error = function(e) NULL)

while (field_angle > 90) field_angle <- field_angle - 180
while (field_angle <= -90) field_angle <- field_angle + 180

valid_data <- gdf_valid |> sf::st_transform(3857)
valid_data$yield_bin <- cut(valid_data$Yield_kg_ha, breaks = breaks_custom, labels = yield_bins$Classe, include.lowest = TRUE)

if (abs(field_angle) > 5) {
  rot <- ggbasemap::coord_rotate(valid_data, field_angle, ratio = 7/5)
  p <- ggplot2::ggplot() +
    ggbasemap::add_basemap(bbox = rot$bbox,
      url = "https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}",
      alpha = 0.3, zoom_offset = 0) +
    ggplot2::geom_sf(data = valid_data, ggplot2::aes(fill = yield_bin), color = NA, size = 0.1) +
    ggplot2::scale_fill_manual(values = bin_colors, name = "Rendement", drop = FALSE) +
    ggspatial::annotation_north_arrow(which_north = "true", location = "tr",
      pad_x = ggplot2::unit(0.4, "cm"), pad_y = ggplot2::unit(0.4, "cm"),
      height = ggplot2::unit(1.8, "cm"), width = ggplot2::unit(1.5, "cm"),
      style = ggspatial::north_arrow_fancy_orienteering(
        fill = c("#002752", "white"), line_col = "#002752", line_width = 1,
        text_col = "#002752", text_face = "bold", text_size = 12)) +
    ggspatial::annotation_scale(location = "br",
      pad_x = ggplot2::unit(0.4, "cm"), pad_y = ggplot2::unit(0.4, "cm"),
      width_hint = 0.2, line_width = 1, line_col = "#002752", text_col = "#002752", text_cex = 0.9) +
    ggplot2::annotate("text", x = -Inf, y = -Inf, label = "Source: Esri World Imagery",
      hjust = 0, vjust = -0.5, size = 3, color = "#002752", fontface = "italic") +
    rot$coord +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text = ggplot2::element_blank(), axis.title = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(), legend.position = "none",
      plot.margin = ggplot2::margin(2, 0, 2, 0))
} else {
  p <- ggplot2::ggplot(valid_data) +
    ggbasemap::add_basemap(valid_data,
      url = "https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}",
      alpha = 0.3, zoom_offset = 0) +
    ggplot2::geom_sf(ggplot2::aes(fill = yield_bin), color = NA, size = 0.1) +
    ggplot2::scale_fill_manual(values = bin_colors, name = "Rendement", drop = FALSE) +
    ggspatial::annotation_north_arrow(which_north = "true", location = "tr",
      pad_x = ggplot2::unit(0.4, "cm"), pad_y = ggplot2::unit(0.4, "cm"),
      height = ggplot2::unit(1.8, "cm"), width = ggplot2::unit(1.5, "cm"),
      style = ggspatial::north_arrow_fancy_orienteering(
        fill = c("#002752", "white"), line_col = "#002752", line_width = 1,
        text_col = "#002752", text_face = "bold", text_size = 12)) +
    ggspatial::annotation_scale(location = "br",
      pad_x = ggplot2::unit(0.4, "cm"), pad_y = ggplot2::unit(0.4, "cm"),
      width_hint = 0.2, line_width = 1, line_col = "#002752", text_col = "#002752", text_cex = 0.9) +
    ggplot2::annotate("text", x = -Inf, y = -Inf, label = "Source: Esri World Imagery",
      hjust = 0, vjust = -0.5, size = 3, color = "#002752", fontface = "italic") +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text = ggplot2::element_blank(), axis.title = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(), legend.position = "none",
      plot.margin = ggplot2::margin(2, 2, 2, 2))
}
```

<div style="border-radius: 8pt; overflow: hidden; box-shadow: 0 4px 15px rgba(0,0,0,0.05);">
```{r map-', chunk_id, ', fig.width=7, fig.height=5}
print(p)
```

:::: {style="display: flex; gap: 20px; margin-top: 15px;"}

::: {style="flex: 0 0 50%; max-width: 50%;"}

```{r table-', chunk_id, ', results="asis"}
if (!is.null(yield_bins) && nrow(yield_bins) > 0) {
  html_table <- "<table class=\'yield-table\' style=\'width:100%; border-collapse: collapse; font-size: 10pt;\'>"
  html_table <- paste0(
    html_table,
    "<thead><tr style=\'background-color: #002752; color: white;\'>",
    "<th style=\'padding: 4px; text-align: left;\'>Classe</th>",
    "<th style=\'padding: 4px; text-align: center;\'>%</th>",
    "<th style=\'padding: 4px; text-align: center;\'>Superficie (ha)</th></tr></thead><tbody>"
  )
  for (i in 1:nrow(yield_bins)) {
    color <- bin_colors[i]
    html_table <- paste0(html_table,
      "<tr style=\'border-bottom: 1px solid #ddd;\'>",
      "<td style=\'padding: 3px;\'><span style=\'display: inline-block; width: 10px; height: 10px; ",
      "background-color: ", color, "; margin-right: 6px; border-radius: 2px;\'></span>", yield_bins$Classe[i], "</td>",
      "<td style=\'padding: 3px; text-align: center; font-weight: bold;\'>", yield_bins$Pourcentage[i], "%</td>",
      "<td style=\'padding: 3px; text-align: center; color: #666;\'>", yield_bins$Superficie[i], "</td>",
      "</tr>")
  }
  html_table <- paste0(html_table, "</tbody></table>")
  cat(html_table)
}
```

:::

::: {style="flex: 0 0 46%; max-width: 46%; padding-left: 15px; padding-top: 20px"}

::: {.value-box .info style="padding: var(--spacing-xs); box-shadow: var(--shadow-sm);"}
`r fontawesome::fa("chart-line", height = "1.2em")`
<span class="label">Rendement moyen</span>
<span class="value">`r round(yield_mean/1000, 1)` t/ha</span>
<span class="label">Mediane: `r round(yield_median/1000, 1)` t/ha</span>
:::

::: {.value-box .info style="margin-top: 15px; padding: var(--spacing-xs); box-shadow: var(--shadow-sm);"}
`r fontawesome::fa("tint", height = "1.2em")`
<span class="label">Humidite moyenne</span>
<span class="value">`r round(moisture_mean, 1)`%</span>
:::

:::

::::
</div>

')

  return(rmd_content)
}

#' Générer le rapport complet multi-champs
#' @noRd
.generate_multi_field_report <- function(fields_data, output_file, title, work_dir, output_format = "pdf", style = "irda") {
  rlang::inform("\n=== Generation du rapport multi-champs ===")
  rlang::inform(paste("Format de sortie:", output_format))
  rlang::inform(paste("Style:", style))

  if (length(fields_data) == 0) {
    rlang::abort("Aucun champ a inclure dans le rapport")
  }

  fields_sorted <- fields_data[order(
    sapply(fields_data, function(x) x$info$year),
    sapply(fields_data, function(x) x$info$field_name)
  )]

  temp_dir <- file.path(work_dir, "report_build")
  dir.create(temp_dir, recursive = TRUE, showWarnings = FALSE)

  # Determiner le repertoire de base et le repertoire du style
  template_dir <- system.file("rapport", package = "yieldcleanr")
  if (template_dir == "") {
    template_dir <- file.path(getwd(), "inst", "rapport")
  }
  
  # Repertoire du style selectionne
  if (style == "ced") {
    style_dir <- system.file("rapport", "ced", package = "yieldcleanr")
    if (style_dir == "") {
      style_dir <- file.path(getwd(), "inst", "rapport", "ced")
    }
    if (!dir.exists(style_dir)) {
      rlang::warn("Style 'ced' directory not found, falling back to 'irda' style")
      style_dir <- template_dir
      style <- "irda"
    }
  } else {
    style_dir <- template_dir
  }
  
  rlang::inform(paste("Style directory:", style_dir))
  
  # Nom de l'organisation selon le style
  org_name <- if (style == "ced") "Cedric Bouffard" else "IRDA"

  # Fonction pour convertir une image en base64
  image_to_base64 <- function(image_path) {
    if (!file.exists(image_path)) {
      return(NULL)
    }
    img_data <- readBin(image_path, "raw", file.size(image_path))
    base64_data <- base64enc::base64encode(img_data)
    base64_data <- gsub("\n", "", base64_data)
    base64_data <- gsub("\r", "", base64_data)
    ext <- tolower(tools::file_ext(image_path))
    mime_type <- switch(ext, "png" = "image/png", "jpg" = "image/jpeg", "jpeg" = "image/jpeg", "gif" = "image/gif", "image/png")
    paste0("data:", mime_type, ";base64,", base64_data)
  }

  # Copier et modifier le CSS pour integrer les images en base64
  css_source <- file.path(style_dir, "brochure.css")
  css_dest <- file.path(temp_dir, "brochure.css")
  if (file.exists(css_source)) {
    css_content <- readLines(css_source, warn = FALSE)
    # Pour le style IRDA, integrer bandeaugauche.png
    if (style == "irda") {
      bandeau_path <- file.path(style_dir, "bandeaugauche.png")
      if (file.exists(bandeau_path)) {
        bandeau_base64 <- image_to_base64(bandeau_path)
        if (!is.null(bandeau_base64)) {
          css_content <- gsub("url\\('bandeaugauche.png'\\)", paste0("url('", bandeau_base64, "')"), css_content)
        }
      }
    }
    # Pour le style CED, integrer background.png
    if (style == "ced") {
      background_path <- file.path(style_dir, "background.png")
      if (file.exists(background_path)) {
        background_base64 <- image_to_base64(background_path)
        if (!is.null(background_base64)) {
          css_content <- gsub("url\\('background.png'\\)", paste0("url('", background_base64, "')"), css_content)
        }
      }
    }
    writeLines(css_content, css_dest)
  }

  # Copier et modifier le HTML pour integrer les images en base64
  html_source <- file.path(style_dir, "header_overrides.html")
  html_dest <- file.path(temp_dir, "header_overrides.html")
  if (file.exists(html_source)) {
    html_content <- readLines(html_source, warn = FALSE)
    html_content_str <- paste(html_content, collapse = "\n")
    
    # Integrer logo.png - remplacer logo.src = 'logo.png'
    logo_path <- file.path(style_dir, "logo.png")
    if (file.exists(logo_path)) {
      logo_base64 <- image_to_base64(logo_path)
      if (!is.null(logo_base64)) {
        html_content_str <- gsub("logo\\.src = 'logo\\.png'", 
                                  paste0("logo.src = '", logo_base64, "'"), 
                                  html_content_str, fixed = FALSE)
      }
    }
    
    # Integrer image de couverture selon le style
    if (style == "irda") {
      couverture_path <- file.path(style_dir, "image_couverture.png")
      if (file.exists(couverture_path)) {
        couverture_base64 <- image_to_base64(couverture_path)
        if (!is.null(couverture_base64)) {
          html_content_str <- gsub("imgCouverture\\.src = 'image_couverture\\.png'", 
                                    paste0("imgCouverture.src = '", couverture_base64, "'"), 
                                    html_content_str, fixed = FALSE)
        }
      }
    } else {
      # Pour le style CED, utiliser background.png
      background_path <- file.path(style_dir, "background.png")
      if (file.exists(background_path)) {
        background_base64 <- image_to_base64(background_path)
        if (!is.null(background_base64)) {
          html_content_str <- gsub("imgCouverture\\.src = 'image_couverture\\.png'", 
                                    paste0("imgCouverture.src = '", background_base64, "'"), 
                                    html_content_str, fixed = FALSE)
          # Aussi remplacer background.png si present dans le HTML
          html_content_str <- gsub("url\\('background\\.png'\\)", 
                                    paste0("url('", background_base64, "')"), 
                                    html_content_str, fixed = FALSE)
        }
      }
    }
    
    writeLines(html_content_str, html_dest)
  }

  # Copier les images dans le repertoire temporaire ET preparer les base64 pour le Rmd
  logo_base64_for_rmd <- ""
  couverture_base64_for_rmd <- ""
  
  logo_path <- file.path(style_dir, "logo.png")
  if (file.exists(logo_path)) {
    file.copy(logo_path, file.path(temp_dir, "logo.png"), overwrite = TRUE)
    logo_base64_for_rmd <- image_to_base64(logo_path)
  }
  
  # Image de couverture selon le style
  if (style == "irda") {
    couverture_path <- file.path(style_dir, "image_couverture.png")
  } else {
    couverture_path <- file.path(style_dir, "background.png")
  }
  if (file.exists(couverture_path)) {
    file.copy(couverture_path, file.path(temp_dir, basename(couverture_path)), overwrite = TRUE)
    couverture_base64_for_rmd <- image_to_base64(couverture_path)
  }

  current_date <- format(Sys.Date(), "%B %Y")

  yaml_header <- paste0('---
title: "', title, '"
subtitle: "Analyse des donnees de rendement"
author: "YieldCleanr"
date: "', current_date, '"
header-left: "', title, '"
header-right: "', org_name, '"
footer-right: "', current_date, '"
page-number-position: "alternate"
output:
  pagedown::html_paged:
    css:
      - "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0/css/all.min.css"
      - "default-page"
      - "default"
      - "brochure.css"
    number_sections: false
    toc: true
    toc_depth: 2
    toc_title: "Table des matieres"
    includes:
      in_header: header_overrides.html
---

')

  # Utiliser base64 pour les images si disponible, sinon les fichiers locaux
  logo_src <- if (nchar(logo_base64_for_rmd) > 0) logo_base64_for_rmd else "logo.png"
  couverture_src <- if (nchar(couverture_base64_for_rmd) > 0) couverture_base64_for_rmd else "image_couverture.png"
  
  setup_chunk <- paste0('
```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE, fig.width = 7, fig.height = 5, out.width = "100%")
library(ggplot2)
library(sf)
library(dplyr)
library(fontawesome)
library(ggspatial)
library(ggbasemap)
```

```{r meta-tags, echo=FALSE, results="asis"}
# Insert meta tags for header/footer configuration
cat(\'<meta name="header-left" content="', title, '">\')
cat(\'<meta name="header-right" content="', org_name, '">\')
cat(\'<meta name="footer-right" content="', current_date, '">\')
cat(\'<meta name="page-number-position" content="alternate">\')
```

<style>
.heading-number,
.toc-num {
  display: none !important;
}
</style>

')

  fields_content <- ""
  current_year <- NULL

  for (field_name in names(fields_sorted)) {
    field <- fields_sorted[[field_name]]
    is_new_year <- is.null(current_year) || field$info$year != current_year

    if (is_new_year) {
      current_year <- field$info$year
    }

    fields_content <- paste0(fields_content, .generate_field_rmd(field, is_new_year))
  }

  full_rmd <- paste0(yaml_header, setup_chunk, fields_content)

  rmd_file <- file.path(temp_dir, "multi_report.Rmd")
  writeLines(full_rmd, rmd_file, useBytes = TRUE)

  rlang::inform(paste("Fichier Rmd genere:", rmd_file))
  rlang::inform("Rendu du rapport PDF...")

  output_html <- file.path(temp_dir, "multi_report.html")

  tryCatch({
    rmarkdown::render(
      rmd_file,
      output_file = output_html,
      output_dir = temp_dir,
      quiet = FALSE
    )

    if (file.exists(output_html)) {
      if (output_format == "html") {
        # Return HTML directly - but need to copy supporting files too
        html_output_file <- gsub("\\.pdf$", ".html", output_file)
        output_dir_html <- dirname(html_output_file)
        
        # Copy HTML
        file.copy(output_html, html_output_file, overwrite = TRUE)
        
        # Copy supporting files (CSS, images, HTML includes) to the same directory as the HTML
        # These are in temp_dir but need to go alongside the HTML
        supporting_patterns <- c("\\.css$", "\\.png$", "\\.jpg$", "\\.jpeg$", "\\.html$")
        for (pattern in supporting_patterns) {
          supporting_files <- list.files(temp_dir, pattern = pattern, full.names = TRUE)
          for (f in supporting_files) {
            fname <- basename(f)
            if (fname != basename(output_html)) {  # Don't copy the HTML again
              file.copy(f, file.path(output_dir_html, fname), overwrite = TRUE)
              rlang::inform(paste("  Copied:", fname))
            }
          }
        }
        
        rlang::inform(paste("Rapport HTML genere:", html_output_file))
        output_file <- html_output_file
      } else {
        # PDF format - try conversion
        rlang::inform("Conversion en PDF...")
        
        # Try to generate PDF with a long timeout
        pdf_success <- tryCatch({
          pagedown::chrome_print(output_html, output = output_file, wait = 300)
          rlang::inform(paste("Rapport PDF genere:", output_file))
          TRUE
        }, error = function(e) {
          rlang::warn(paste("Echec de la generation PDF:", e$message))
          rlang::inform("Retour du fichier HTML a la place...")
          FALSE
        })
        
        # If PDF failed, return the HTML file instead
        if (!pdf_success) {
          html_output_file <- gsub("\\.pdf$", ".html", output_file)
          file.copy(output_html, html_output_file, overwrite = TRUE)
          rlang::inform(paste("Rapport HTML genere:", html_output_file))
          # Update output_file to return the HTML path
          output_file <- html_output_file
        }
      }
    }

  }, error = function(e) {
    rlang::warn(paste("Erreur lors du rendu:", e$message))
    debug_rmd <- gsub("\\.(pdf|html)$", ".Rmd", output_file)
    file.copy(rmd_file, debug_rmd, overwrite = TRUE)
    rlang::inform(paste("Fichier Rmd copie pour debug:", debug_rmd))
  })

  return(output_file)
}


# ============================================================================
# HTML Report Generation (without pagedown/PDF)
# ============================================================================

#' Generate a multi-field HTML report
#'
#' @param file_paths Vector of file paths (GeoJSON)
#' @param output_file Output HTML file path
#' @param title Report title
#' @param style Style visuel du rapport: "irda" (défaut) ou "ced" (Cedric Bouffard)
#' @return Path to generated HTML file
#' @export
generate_batch_report_html <- function(file_paths, output_file = NULL, title = NULL, style = c("irda", "ced")) {
  file_paths <- as.character(file_paths)
  style <- match.arg(style)
  
  if (is.null(output_file)) {
    output_file <- gsub("\\.pdf$", ".html", tempfile(fileext = ".html"))
  }
  
  if (is.null(title)) {
    title <- "Rapport multi-champs"
  }
  
  rlang::inform(paste("Generation du rapport HTML:", title))
  rlang::inform(paste("Style:", style))
  
  # Process each GeoJSON file
  fields_data <- list()
  for (geojson_path in file_paths) {
    field_name <- tools::file_path_sans_ext(basename(geojson_path))
    rlang::inform(paste("Traitement:", field_name))
    
    field_data <- .process_geojson_file(geojson_path, tempdir())
    if (!is.null(field_data)) {
      fields_data[[field_name]] <- field_data
    }
  }
  
  if (length(fields_data) == 0) {
    rlang::abort("Aucun champ valide trouve")
  }
  
  # Sort by year and field name
  fields_sorted <- fields_data[order(
    sapply(fields_data, function(x) x$info$year),
    sapply(fields_data, function(x) x$info$field_name)
  )]
  
  rlang::inform(paste("Total:", length(fields_sorted), "champs"))
  
  # Generate HTML directly
  html_content <- .generate_html_report(fields_sorted, title, style)
  
  writeLines(html_content, output_file, useBytes = TRUE)
  rlang::inform(paste("Rapport HTML genere:", output_file))
  
  invisible(output_file)
}


#' Generate HTML content for multi-field report
#' @noRd
.generate_html_report <- function(fields_data, title, style = "irda") {
  
  current_date <- format(Sys.Date(), "%B %Y")
  
  # Couleurs selon le style
  if (style == "ced") {
    # Style Cedric Bouffard - Teal/Gold moderne
    primary_color <- "#2E3944"
    secondary_color <- "#4A9C8B"
    accent_color <- "#EAC22E"
    gradient_start <- "#2E3944"
    gradient_end <- "#3E4E5C"
    org_name <- "Cedric Bouffard"
  } else {
    # Style IRDA - Bleu/Vert lime
    primary_color <- "#002752"
    secondary_color <- "#AED136"
    accent_color <- "#D5785A"
    gradient_start <- "#002752"
    gradient_end <- "#004080"
    org_name <- "IRDA"
  }
  
  html_head <- paste0('<!DOCTYPE html>
<html lang="fr">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>', title, '</title>
  <link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0/css/all.min.css">
  <style>
    * { box-sizing: border-box; margin: 0; padding: 0; }
    body { 
      font-family: "Segoe UI", Tahoma, Geneva, Verdana, sans-serif; 
      color: #333; 
      line-height: 1.6;
      background: #f8f9fa;
    }
    .container { max-width: 1200px; margin: 0 auto; padding: 20px; }
    .header {
      background: linear-gradient(135deg, ', gradient_start, ' 0%, ', gradient_end, ' 100%);
      color: white;
      padding: 40px 20px;
      text-align: center;
      margin-bottom: 30px;
    }
    .header h1 { font-size: 2.5em; margin-bottom: 10px; }
    .header .subtitle { font-size: 1.2em; opacity: 0.9; }
    .year-section { margin-bottom: 40px; }
    .year-header {
      background: ', secondary_color, ';
      color: ', primary_color, ';
      padding: 15px 25px;
      font-size: 1.8em;
      font-weight: bold;
      border-radius: 8px;
      margin-bottom: 20px;
    }
    .field-card {
      background: white;
      border-radius: 12px;
      box-shadow: 0 4px 15px rgba(0,0,0,0.1);
      margin-bottom: 30px;
      overflow: hidden;
    }
    .field-header {
      background: ', primary_color, ';
      color: white;
      padding: 20px;
    }
    .field-header h2 { font-size: 1.5em; margin-bottom: 5px; }
    .field-header .crop-badge {
      display: inline-block;
      background: ', secondary_color, ';
      color: ', primary_color, ';
      padding: 3px 12px;
      border-radius: 15px;
      font-size: 0.9em;
      font-weight: 600;
    }
    .field-content { padding: 20px; }
    .stats-grid {
      display: grid;
      grid-template-columns: repeat(auto-fit, minmax(200px, 1fr));
      gap: 15px;
      margin-bottom: 20px;
    }
    .stat-box {
      background: #f8f9fa;
      padding: 15px;
      border-radius: 8px;
      text-align: center;
    }
    .stat-box .value { font-size: 1.8em; font-weight: bold; color: ', primary_color, '; }
    .stat-box .label { font-size: 0.9em; color: #666; }
    .map-container {
      background: #eee;
      height: 400px;
      border-radius: 8px;
      margin-top: 15px;
      display: flex;
      align-items: center;
      justify-content: center;
      color: #999;
    }
    .footer {
      text-align: center;
      padding: 20px;
      color: #666;
      font-size: 0.9em;
      border-top: 1px solid #ddd;
      margin-top: 40px;
    }
    @media print {
      body { background: white; }
      .field-card { box-shadow: none; border: 1px solid #ddd; }
      .map-container { height: 300px; }
    }
  </style>
</head>
<body>
  <div class="header">
    <h1>', title, '</h1>
    <div class="subtitle">', current_date, ' | ', org_name, '</div>
  </div>
  <div class="container">
')
  
  html_body <- ""
  current_year <- NULL
  
  for (field_name in names(fields_data)) {
    field <- fields_data[[field_name]]
    info <- field$info
    
    # Year header if new year
    if (is.null(current_year) || info$year != current_year) {
      if (!is.null(current_year)) {
        html_body <- paste0(html_body, "\n  </div>\n")
      }
      current_year <- info$year
      html_body <- paste0(html_body, '
  <div class="year-section">
    <div class="year-header">', current_year, '</div>
')
    }
    
    # Field card
    crop_display <- if (info$crop_name != "") info$crop_name else "Champ"
    
    html_body <- paste0(html_body, '
    <div class="field-card">
      <div class="field-header">
        <h2>', info$field_name, '</h2>
        ', if (info$crop_name != "") paste0('<span class="crop-badge">', info$crop_name, '</span>'), '
      </div>
      <div class="field-content">
        <div class="stats-grid">
          <div class="stat-box">
            <div class="value">', format(info$n_raw, big.mark = " "), '</div>
            <div class="label">Points bruts</div>
          </div>
          <div class="stat-box">
            <div class="value">', format(info$n_clean, big.mark = " "), '</div>
            <div class="label">Points retenus</div>
          </div>
          <div class="stat-box">
            <div class="value">', format(info$retention_rate, digits = 1), '%</div>
            <div class="label">Taux de retention</div>
          </div>
          <div class="stat-box">
            <div class="value">', round(info$yield_mean/1000, 2), ' t/ha</div>
            <div class="label">Rendement moyen</div>
          </div>
        </div>
        <div class="map-container">
          <p><i class="fas fa-map"></i> Carte du rendement - Voir la version PDF pour la carte</p>
        </div>
      </div>
    </div>
')
  }
  
  if (!is.null(current_year)) {
    html_body <- paste0(html_body, "\n  </div>\n")
  }
  
  html_footer <- paste0('
  </div>
  <div class="footer">
    <p>Genere par YieldCleanr le ', current_date, '</p>
  </div>
</body>
</html>
')
  
  return(paste0(html_head, html_body, html_footer))
}
