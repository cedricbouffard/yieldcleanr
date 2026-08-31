#' Lire des donnees de rendement brutes depuis un fichier texte
#'
#' Cette fonction lit les donnees brutes de rendement depuis un fichier
#' texte formate selon le standard des fichiers de moissonneuse.
#' Supporte differents formats de fichiers (15-17 colonnes).
#'
#' @param file_path Chemin du fichier texte d'entree
#' @param data Data frame deja charge. Alternative a file_path.
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
  read_yield_data <- function(file_path = NULL, data = NULL) {
  # Check if file_path is actually a data frame (when called without named arguments)
  if (!is.null(file_path) && (is.data.frame(file_path) || inherits(file_path, "tbl_df"))) {
    if (!".row_id" %in% names(file_path)) {
      file_path <- file_path |> dplyr::mutate(.row_id = dplyr::row_number())
    }
    return(file_path)
  }

  if (!is.null(data) && (is.data.frame(data) || inherits(data, "tbl_df"))) {
    if (!".row_id" %in% names(data)) {
      data <- data |> dplyr::mutate(.row_id = dplyr::row_number())
    }
    return(data)
  }

  if (is.null(file_path)) {
    rlang::abort("Either 'file_path' or 'data' must be provided")
  }

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
      # Colonne 15 = Altitude (toujours en position 15 pour ce format)
      # Colonne 14 = GrainType, pas de DOP/GPSStatus pour ce format
      colnames(data) <- c(
        "Longitude", "Latitude", "Flow", "GPS_Time",
        "Interval", "Distance", "Swath", "Moisture",
        "HeaderStatus", "Pass", "Serial", "FieldID",
        "LoadID", "GrainType", "Altitude"
      )
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
  int_cols <- c("GPS_Time", "Interval", "Distance", "Swath", "HeaderStatus", "Pass", "GPSStatus")
  char_cols <- c("FieldID", "LoadID", "GrainType", "Serial")
  
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

  # Detection et conversion des unites imperiales (AgLeader Advanced)
  # Les fichiers AgLeader ont Distance et Swath en pouces
  data <- detect_and_convert_imperial_units(data)

  return(data)
}

#' Detecter et convertir les unites imperiales (AgLeader Advanced)
#'
#' Cette fonction detecte si les donnees sont en unites imperiales (pouces)
#' et les convertit en unites metriques (metres). Les fichiers AgLeader
#' Advanced ont typiquement Distance et Swath en pouces.
#'
#' @param data Tibble avec donnees de rendement
#' @return Tibble avec unites converties en metres
#' @noRd
detect_and_convert_imperial_units <- function(data) {
  if (!all(c("Distance", "Swath") %in% names(data))) {
    return(data)
  }

  # Calculer les moyennes pour detection
  mean_distance <- mean(data$Distance[!is.na(data$Distance)], na.rm = TRUE)
  mean_swath <- mean(data$Swath[!is.na(data$Swath)], na.rm = TRUE)

  # Detection des unites basee sur des valeurs typiques
  # En pouces (AgLeader): Distance ~40-120 pouces (1-3m), Swath ~295-472 pouces (7.5-12m)
  # En metres: Distance ~1-3m, Swath ~7.5-12m
  # Un swath reel ne peut pas etre < 3m (sinon c'est des pouces)

  # Si Distance > 20 et < 200, c'est probablement en pouces
  if (!is.na(mean_distance) && mean_distance > 20 && mean_distance < 200) {
    rlang::inform(paste("Distance detectee en pouces (moyenne:", round(mean_distance, 1), ") - conversion en metres"))
    data$Distance <- data$Distance * 0.0254  # pouces -> metres
  }

   # Detection Swath:
   # Un swath reel est toujours entre 3-15m (typique: 6-12m pour moissonneuses)
   # Valeurs en pouces typiques: 120-472 pouces (10-40 pieds)
   # Valeurs en metres typiques: 3-15m
   # Si Swath > 100 et < 200, c'est probablement en pouces (120 = 10 pieds header)
   if (!is.na(mean_swath)) {
     if (mean_swath < 3) {
       # < 3m: probablement pouces (un swath reel est toujours > 3m)
       rlang::inform(paste("Swath detecte en pouces (moyenne:", round(mean_swath, 1), ") - conversion en metres"))
       data$Swath <- data$Swath * 0.0254
     } else if (mean_swath >= 100 && mean_swath <= 200) {
       # 100-200 pouces = 2.5-5m (10-16 pieds header -tres courant)
       rlang::inform(paste("Swath detecte en pouces (moyenne:", round(mean_swath, 1), ") - conversion en metres"))
       data$Swath <- data$Swath * 0.0254
     } else if (mean_swath > 200 && mean_swath < 500) {
       # 200-500 pouces (17-40 pieds - grandes captures)
       rlang::inform(paste("Swath detecte en pouces (moyenne:", round(mean_swath, 1), ") - conversion en metres"))
       data$Swath <- data$Swath * 0.0254
     }
     # Si 3-100m: on garde comme metres (valeur normale)
     # Si > 500: valeur aberrante, on garde telle quelle
   }

  return(data)
}

#' Lister les champs disponibles dans un fichier ZIP
#'
#' Cette fonction liste tous les champs disponibles dans un fichier ZIP
#' contenant des shapefiles (format John Deere, etc.)
#'
#' @param zip_path Chemin vers le fichier ZIP
#' @return Un tibble avec les informations sur les champs disponibles
#' @export
list_fields_from_zip <- function(zip_path) {
  if (!file.exists(zip_path)) {
    rlang::abort(paste("Le fichier ZIP n'existe pas:", zip_path))
  }
  
  if (!requireNamespace("utils", quietly = TRUE)) {
    rlang::abort("Le package 'utils' est requis")
  }
  
  # Lister le contenu du ZIP
  zip_contents <- utils::unzip(zip_path, list = TRUE)
  
  # Trouver les shapefiles (.shp)
  shp_files <- zip_contents$Name[grepl("\\.shp$", zip_contents$Name, ignore.case = TRUE)]
  
  if (length(shp_files) == 0) {
    rlang::warn("Aucun shapefile trouve dans le ZIP")
    return(tibble::tibble(
      field_name = character(),
      size_bytes = numeric()
    ))
  }
  
  # Extraire les noms de champs
  fields <- tibble::tibble(
    Name = shp_files,
    field_name = tools::file_path_sans_ext(basename(shp_files)),
    size_bytes = zip_contents$Length[match(shp_files, zip_contents$Name)]
  )
  
  return(fields)
}


#' Lire les donnees de rendement depuis un fichier ZIP
#'
#' Cette fonction lit les donnees de rendement depuis un fichier ZIP
#' contenant des shapefiles (format John Deere, etc.)
#'
#' @param zip_path Chemin vers le fichier ZIP
#' @param field_name Nom du champ a lire
#' @return Un objet sf avec les donnees de rendement
#' @export
read_yield_from_zip <- function(zip_path, field_name) {
  if (!file.exists(zip_path)) {
    rlang::abort(paste("Le fichier ZIP n'existe pas:", zip_path))
  }
  
  if (!requireNamespace("sf", quietly = TRUE)) {
    rlang::abort("Le package 'sf' est requis")
  }
  
  # Creer un repertoire temporaire
  temp_dir <- tempfile(pattern = "yield_zip_")
  dir.create(temp_dir, showWarnings = FALSE, recursive = TRUE)
  
  # Lister les champs disponibles
  fields <- list_fields_from_zip(zip_path)
  
  if (!field_name %in% fields$field_name) {
    rlang::abort(paste("Champ", field_name, "non trouve dans le ZIP"))
  }
  
  # Trouver le fichier shapefile correspondant
  shp_file <- fields$Name[fields$field_name == field_name]
  base_name <- tools::file_path_sans_ext(shp_file)
  
  # Extraire tous les fichiers associes au shapefile
  zip_contents <- utils::unzip(zip_path, list = TRUE)
  related_files <- zip_contents$Name[grepl(paste0("^", base_name, "\\."), zip_contents$Name, ignore.case = TRUE)]

  # Chercher aussi le fichier JSON de metadonnees (-Deere-Metadata.json)
  json_files <- zip_contents$Name[grepl(paste0("^", base_name, ".*-Deere-Metadata\\.json$"),
                                        zip_contents$Name, ignore.case = TRUE)]
  all_files <- unique(c(related_files, json_files))
  
  # Extraire les fichiers
  utils::unzip(zip_path, files = all_files, exdir = temp_dir)
  
  # Lire le fichier JSON de metadonnees s'il existe
  metadata <- NULL
  if (length(json_files) > 0) {
    json_path <- file.path(temp_dir, json_files[1])
    if (!file.exists(json_path)) {
      json_path <- file.path(temp_dir, basename(json_files[1]))
    }
    metadata <- parse_jd_metadata(json_path)
  }
  
  # Lire le shapefile
  shp_path <- file.path(temp_dir, shp_file)
  if (!file.exists(shp_path)) {
    shp_path <- file.path(temp_dir, basename(shp_file))
  }
  
  data <- sf::st_read(shp_path, quiet = TRUE)

  # Detacher l'objet sf de ses fichiers source en forcant une copie en memoire
  # Cela evite un segfault quand le repertoire temporaire est supprime
  # alors que l'objet sf maintient encore des references aux fichiers
  data <- sf::st_sf(sf::st_drop_geometry(data), geometry = sf::st_geometry(data))

  # Nettoyer le repertoire temporaire maintenant que les donnees sont en memoire
  unlink(temp_dir, recursive = TRUE)
  
  # Convertir les unites selon le JSON metadata (avant standardisation)
  data <- convert_units_from_json(data, metadata)

  # Standardiser les colonnes John Deere (avec metadonnees si disponibles)
  data <- standardize_jd_columns(data, metadata = metadata)

  # Convertir en data.frame (les coordonnees sont dans Longitude/Latitude)
  # Le pipeline de nettoyage travaille avec des data.frames, pas des sf
  # Les polygones sf seront recrees a la fin par data_to_sf()
  if (inherits(data, "sf")) {
    metadata_attrs <- attributes(data)[!names(attributes(data)) %in%
      c("names", "class", "row.names", "sf_column", "agr")]
    data <- sf::st_drop_geometry(data)
    # Restaurer les attributs custom (jd_metadata, etc.)
    for (attr_name in names(metadata_attrs)) {
      attr(data, attr_name) <- metadata_attrs[[attr_name]]
    }
  }
  
  return(data)
}

#' Standardiser les colonnes d'un fichier John Deere
#'
#' Convertit les noms de colonnes du format John Deere vers le format
#' standard de yieldcleanr.
#'
#' @param data Objet sf avec les donnees John Deere
#' @return Objet sf avec les colonnes standardisees
#' @noRd
standardize_jd_columns <- function(data, metadata = NULL) {
  # Debug: afficher les noms de colonnes trouves
  original_names <- names(data)
  message(paste("Colonnes trouvees dans le shapefile:", paste(original_names, collapse = ", ")))
  
  # Mapping des noms de colonnes John Deere vers yieldcleanr
  jd_mapping <- c(
    "DISTANCE" = "Distance",
    "SWATHWIDTH" = "Swath",
    "VRYIELDMAS" = "Flow",
    "NetYldA" = "Flow",
    "GrossYldA" = "Flow_Gross",
    "GrossYld" = "GrossYld_total",
    "NetYld" = "NetYld_total",
    "Trash" = "Trash",
    "SECTIONID" = "Pass",
    "Crop" = "GrainType",
    "WetMass" = "Flow_Wet",
    "Moisture" = "Moisture",
    "Time" = "GPS_Time",
    "Heading" = "Heading",
    "VARIETY" = "Variety",
    "Elevation" = "Altitude",
    "IsoTime" = "IsoTime",
    "Machine" = "Machine",
    "FUEL" = "Fuel",
    "VEHICLSPEED" = "Velocity",
    "DRYMATTER" = "DryMatter",
    "PRODUCTHASH" = "ProductHash"
  )

  # Si NetYldA existe mais pas VRYIELDMAS, on utilise NetYldA comme Flow
  # (NetYldA est le rendement net par acre, typiquement en ton/acre)
  # Priorite: VRYIELDMAS > NetYldA pour le mapping vers Flow
  if ("VRYIELDMAS" %in% original_names && "NetYldA" %in% original_names) {
    # Les deux existent, VRYIELDMAS a la priorite
    jd_mapping <- jd_mapping[names(jd_mapping) != "NetYldA"]
    jd_mapping["NetYldA"] <- "NetYldA_raw"
  }
  
  # Renommer les colonnes existantes
  renamed_count <- 0
  for (jd_name in names(jd_mapping)) {
    if (jd_name %in% names(data)) {
      old_name <- jd_name
      new_name <- jd_mapping[jd_name]
      names(data)[names(data) == old_name] <- new_name
      message(paste("Renomme:", old_name, "->", new_name))
      renamed_count <- renamed_count + 1
    }
  }
  
  # Detection automatique pour les colonnes essentielles manquantes
  if (!"Flow" %in% names(data)) {
    message("Recherche de colonne de rendement (Flow)...")
    yield_cols <- grep("yield|yld|rendement|rend|mass|vryield", original_names, ignore.case = TRUE, value = TRUE)
    if (length(yield_cols) > 0) {
      message(paste("Colonnes de rendement detectees:", paste(yield_cols, collapse = ", ")))
      names(data)[names(data) == yield_cols[1]] <- "Flow"
      message(paste("Utilise", yield_cols[1], "comme Flow"))
    }
  }
  
  if (!"Moisture" %in% names(data)) {
    moist_cols <- grep("moisture|moist|humidite|humid", original_names, ignore.case = TRUE, value = TRUE)
    if (length(moist_cols) > 0) {
      names(data)[names(data) == moist_cols[1]] <- "Moisture"
      message(paste("Utilise", moist_cols[1], "comme Moisture"))
    }
  }
  
  if (!"Swath" %in% names(data)) {
    message("Recherche de colonne de largeur (Swath)...")
    swath_cols <- grep("swath|width|largeur|larg|swathwidth", original_names, ignore.case = TRUE, value = TRUE)
    if (length(swath_cols) > 0) {
      names(data)[names(data) == swath_cols[1]] <- "Swath"
      message(paste("Utilise", swath_cols[1], "comme Swath"))
    }
  }
  
  if (!"Pass" %in% names(data)) {
    message("Recherche de colonne de passage (Pass)...")
    pass_cols <- grep("pass|passage|section|sectionid", original_names, ignore.case = TRUE, value = TRUE)
    if (length(pass_cols) > 0) {
      names(data)[names(data) == pass_cols[1]] <- "Pass"
      message(paste("Utilise", pass_cols[1], "comme Pass"))
    }
  }
  
  if (!"Distance" %in% names(data)) {
    dist_cols <- grep("distance|dist", original_names, ignore.case = TRUE, value = TRUE)
    if (length(dist_cols) > 0) {
      names(data)[names(data) == dist_cols[1]] <- "Distance"
      message(paste("Utilise", dist_cols[1], "comme Distance"))
    }
  }
  
  # Extraire les coordonnees de la geometrie si c'est un objet sf
  if (inherits(data, "sf")) {
    coords <- sf::st_coordinates(data)
    if (ncol(coords) >= 2) {
      data$Longitude <- coords[, 1]
      data$Latitude <- coords[, 2]
      message("Coordonnees extraites de la geometrie")
    }
  }
  
  # Utiliser les metadonnees JSON pour enrichir GrainType si c'est un code numerique
  if (!is.null(metadata) && "GrainType" %in% names(data)) {
    # Si GrainType est numerique (code culture JD), remplacer par le nom de culture
    if (is.numeric(data$GrainType) || is.integer(data$GrainType) ||
        all(grepl("^[0-9]+$", as.character(unique(data$GrainType[!is.na(data$GrainType)]))))) {
      crop_name <- metadata$crop_info$crop_name
      crop_token <- metadata$crop_info$crop_token
      if (!is.na(crop_name) && nchar(crop_name) > 0) {
        message(paste("GrainType numerique (code:", unique(data$GrainType), ") remplace par:", crop_name, "(", crop_token, ")"))
        data$GrainType <- crop_name
      }
    }
  }

  # Stocker les metadonnees comme attribut pour utilisation ulterieure
  if (!is.null(metadata)) {
    attr(data, "jd_metadata") <- metadata
  }

  # S'assurer que les colonnes essentielles existent
  # Note: Moisture n'est pas toujours presente (cultures maraicheres)
  essential_cols <- c("Flow", "Swath", "Pass", "Longitude", "Latitude")
  for (col in essential_cols) {
    if (!col %in% names(data)) {
      data[[col]] <- NA_real_
      message(paste("Colonne", col, "manquante, initialisee avec NA"))
    }
  }

  # Moisture: initialiser a NA si absent (cultures sans humidite grain)
  if (!"Moisture" %in% names(data)) {
    data$Moisture <- NA_real_
    message("Colonne Moisture absente (normal pour cultures maraicheres), initialisee avec NA")
  }
  
  # Ajouter les colonnes optionnelles si elles n'existent pas
  optional_cols <- c("GPS_Time", "Interval", "Distance", "HeaderStatus", "Altitude")
  for (col in optional_cols) {
    if (!col %in% names(data)) {
      data[[col]] <- NA_real_
    }
  }
  
  # Ajouter un index de ligne si des donnees existent
  if (nrow(data) > 0) {
    data <- data |> dplyr::mutate(.row_id = dplyr::row_number())
  }
  
  # Verifier les valeurs de Flow
  if ("Flow" %in% names(data)) {
    valid_flow <- sum(!is.na(data$Flow))
    message(paste("Valeurs Flow valides apres standardisation:", valid_flow, "/", nrow(data)))
  }
  
  return(data)
}

#' Convertir les donnees John Deere metriques vers le format yieldcleanr
#'
#' Cette fonction convertit les donnees John Deere exportees depuis MyJohnDeere
#' qui sont deja en unites metriques (tonnes/ha, metres) vers le format
#' attendu par yieldcleanr.
#'
#' @param data Objet sf avec les donnees John Deere metriques
#' @return Objet sf avec les colonnes converties
#' @noRd
convert_jd_metric_to_yieldcleanr <- function(data, metadata = NULL) {
  message("Conversion des donnees John Deere vers le format yieldcleanr...")

  # Recuperer les metadonnees (soit passees en parametre, soit en attribut)
  if (is.null(metadata)) {
    metadata <- attr(data, "jd_metadata")
  }

  # Extraire les unites des metadonnees JSON si disponibles
  units <- if (!is.null(metadata)) metadata$units else list()
  has_metadata_units <- length(units) > 0
  if (has_metadata_units) {
    # Filtrer les unites non-NA pour un affichage clair
    units_with_values <- units[!is.na(units)]
    if (length(units_with_values) > 0) {
      message("Unites lues depuis les metadonnees JSON:")
      for (unit_name in names(units_with_values)) {
        message(paste("  -", unit_name, ":", units_with_values[[unit_name]]))
      }
    }
  } else {
    message("Aucune metadonnee d'unite trouvee - utilisation de la detection heuristique")
  }

  # ====================================================================
  # CONVERSION DU RENDEMENT (Flow)
  # ====================================================================
  if (!"Flow" %in% names(data) || all(is.na(data$Flow))) {
    yield_cols <- c("Yield_kg_ha", "Yield_t_ha", "DryYield", "WetYield",
                    "Rendement", "RendementSec", "RendementHumide")
    for (col in yield_cols) {
      if (col %in% names(data) && !all(is.na(data[[col]]))) {
        message(paste("Utilisation de", col, "comme Flow"))
        data$Flow <- data[[col]]
        break
      }
    }
  }

  if ("Flow" %in% names(data) && !all(is.na(data$Flow))) {
    mean_flow <- mean(data$Flow[!is.na(data$Flow)], na.rm = TRUE)
    message(paste("Rendement moyen brut:", round(mean_flow, 2)))

    # Determiner l'unite du rendement
    yield_unit <- units[["NetYldA"]] %||% units[["VRYIELDMAS"]] %||% NA_character_

    if (!is.na(yield_unit) && grepl("ton1ac-1|ton/ac|ton\\.ac", yield_unit, ignore.case = TRUE)) {
      # Unite ton/acre (US short ton = 2000 lbs = 907.185 kg, 1 acre = 0.404686 ha)
      # 1 ton/acre = 907.185 / 0.404686 = 2241.7 kg/ha
      message(paste("Unite de rendement detectee depuis metadata:", yield_unit))
      message("Conversion ton/acre -> kg/ha (facteur: 2241.7)")
      data$Flow <- data$Flow * 2241.7
    } else if (!is.na(yield_unit) && grepl("kg1ha-1|kg/ha|kg\\.ha", yield_unit, ignore.case = TRUE)) {
      message("Rendement deja en kg/ha")
    } else if (!is.na(yield_unit) && grepl("t1ha-1|t/ha|tonne/ha", yield_unit, ignore.case = TRUE)) {
      message("Conversion tonnes/ha -> kg/ha")
      data$Flow <- data$Flow * 1000
    } else if (!is.na(yield_unit) && grepl("bu1ac-1|bu/ac|bushel", yield_unit, ignore.case = TRUE)) {
      # Boisseaux/acre -> kg/ha (approximation mais on aura besoin du lbs_per_bushel)
      lbs_per_bu <- get_lbs_per_bushel(data)
      # 1 bu/ac = (lbs_per_bu * 0.453592) / 0.404686 kg/ha
      factor <- lbs_per_bu * 0.453592 / 0.404686
      message(paste("Conversion bu/acre -> kg/ha (facteur:", round(factor, 2), ")"))
      data$Flow <- data$Flow * factor
    } else {
      # Pas de metadonnees d'unite: detection heuristique
      if (mean_flow < 100 && mean_flow > 0) {
        # Probablement en tonnes/ha ou ton/acre
        # Heuristique: si < 20, possiblement tonnes/ha pour cereales
        # Si entre 5 et 60 avec une culture maraichere, probablement ton/acre
        crop_name <- if (!is.null(metadata)) tolower(metadata$crop_info$crop_name %||% "") else ""
        is_vegetable <- grepl("onion|shallot|potato|carrot|beet|celery|turnip|radish|cabbage|lettuce", crop_name)

        if (is_vegetable || mean_flow > 3) {
          # Pour les cultures maraicheres avec rendement faible-moyen, c'est probablement ton/acre
          # ou tonnes/ha -- on ne peut pas distinguer sans metadonnees
          # Par defaut: si < 20, on suppose tonnes/ha (format JD metrique habituel)
          message("Conversion tonnes/ha -> kg/ha (heuristique, pas de metadonnees d'unite)")
          data$Flow <- data$Flow * 1000
        } else {
          message("Conversion tonnes/ha -> kg/ha")
          data$Flow <- data$Flow * 1000
        }
      }
      # Si > 100, on suppose deja en kg/ha
      if (mean_flow >= 100 && mean_flow < 50000) {
        message("Rendement detecte comme deja en kg/ha")
      }
    }

    # Stocker le rendement comme Yield_kg_ha_wet
    data$Yield_kg_ha_wet <- data$Flow
    message(paste("Yield_kg_ha_wet cree:", round(mean(data$Flow, na.rm = TRUE), 1), "kg/ha (",
                  sum(!is.na(data$Yield_kg_ha_wet)), "valeurs)"))
  }

  # Creer Yield_kg_ha_wet si Flow_Wet existe (donnees avec rendement humide explicite)
  if ("Flow_Wet" %in% names(data) && !all(is.na(data$Flow_Wet))) {
    mean_flow_wet <- mean(data$Flow_Wet[!is.na(data$Flow_Wet)], na.rm = TRUE)
    message(paste("Rendement humide explicite moyen:", round(mean_flow_wet, 2)))

    if (mean_flow_wet < 20 && mean_flow_wet > 0) {
      message("Conversion rendement humide tonnes/ha -> kg/ha")
      data$Flow_Wet <- data$Flow_Wet * 1000
    }

    if (!"Yield_kg_ha_wet" %in% names(data) || all(is.na(data$Yield_kg_ha_wet))) {
      data$Yield_kg_ha_wet <- data$Flow_Wet
      message(paste("Yield_kg_ha_wet cree a partir de Flow_Wet:", sum(!is.na(data$Yield_kg_ha_wet)), "valeurs"))
    }
  }

  # ====================================================================
  # CONVERSION FLOW_GROSS (rendement brut par acre)
  # ====================================================================
  if ("Flow_Gross" %in% names(data) && !all(is.na(data$Flow_Gross))) {
    gross_yield_unit <- units[["GrossYldA"]] %||% NA_character_
    mean_gross <- mean(data$Flow_Gross[!is.na(data$Flow_Gross)], na.rm = TRUE)

    if (!is.na(gross_yield_unit) && grepl("ton1ac-1|ton/ac|ton\\.ac", gross_yield_unit, ignore.case = TRUE)) {
      message(paste("Flow_Gross en ton/acre (metadata:", gross_yield_unit, ") -> conversion en kg/ha"))
      data$Flow_Gross <- data$Flow_Gross * 2241.7
    } else if (!is.na(gross_yield_unit) && grepl("t1ha-1|t/ha|tonne/ha", gross_yield_unit, ignore.case = TRUE)) {
      message("Flow_Gross en tonnes/ha -> conversion en kg/ha")
      data$Flow_Gross <- data$Flow_Gross * 1000
    } else if (mean_gross < 100 && mean_gross > 0) {
      # Heuristique: probablement en tonnes
      message("Flow_Gross heuristique: conversion tonnes -> kg/ha")
      data$Flow_Gross <- data$Flow_Gross * 1000
    }
  }

  # ====================================================================
  # CONVERSION NETYLD_TOTAL ET GROSSYLD_TOTAL (totaux par point, en tons)
  # ====================================================================
  if ("NetYld_total" %in% names(data) && !all(is.na(data$NetYld_total))) {
    netyld_unit <- units[["NetYld"]] %||% NA_character_
    if (!is.na(netyld_unit) && grepl("^ton$|^tons$", netyld_unit, ignore.case = TRUE)) {
      # US short ton = 907.185 kg
      message(paste("NetYld_total en tons imperiales (metadata:", netyld_unit, ") -> conversion en kg"))
      data$NetYld_total <- data$NetYld_total * 907.185
    } else if (!is.na(netyld_unit) && grepl("^t$|tonne|^kg$", netyld_unit, ignore.case = TRUE)) {
      if (grepl("^kg$", netyld_unit, ignore.case = TRUE)) {
        message("NetYld_total deja en kg")
      } else {
        message("NetYld_total en tonnes metriques -> conversion en kg")
        data$NetYld_total <- data$NetYld_total * 1000
      }
    }
  }

  if ("GrossYld_total" %in% names(data) && !all(is.na(data$GrossYld_total))) {
    grossyld_unit <- units[["GrossYld"]] %||% NA_character_
    if (!is.na(grossyld_unit) && grepl("^ton$|^tons$", grossyld_unit, ignore.case = TRUE)) {
      # US short ton = 907.185 kg
      message(paste("GrossYld_total en tons imperiales (metadata:", grossyld_unit, ") -> conversion en kg"))
      data$GrossYld_total <- data$GrossYld_total * 907.185
    } else if (!is.na(grossyld_unit) && grepl("^t$|tonne|^kg$", grossyld_unit, ignore.case = TRUE)) {
      if (grepl("^kg$", grossyld_unit, ignore.case = TRUE)) {
        message("GrossYld_total deja en kg")
      } else {
        message("GrossYld_total en tonnes metriques -> conversion en kg")
        data$GrossYld_total <- data$GrossYld_total * 1000
      }
    }
  }

  # ====================================================================
  # CONVERSION DU SWATH (largeur de coupe)
  # ====================================================================
  if (!"Swath" %in% names(data) || all(is.na(data$Swath))) {
    swath_cols <- c("Swath", "Width", "Largeur", "SwathWidth", "CuttingWidth")
    for (col in swath_cols) {
      if (col %in% names(data) && !all(is.na(data[[col]]))) {
        message(paste("Utilisation de", col, "comme Swath"))
        data$Swath <- data[[col]]
        break
      }
    }
  }

  if ("Swath" %in% names(data) && !all(is.na(data$Swath))) {
    mean_swath <- mean(data$Swath[!is.na(data$Swath)], na.rm = TRUE)
    swath_unit <- units[["SWATHWIDTH"]] %||% NA_character_

    if (!is.na(swath_unit) && grepl("^ft$|feet|foot", swath_unit, ignore.case = TRUE)) {
      message(paste("Swath en pieds (metadata:", swath_unit, ") moyenne:", round(mean_swath, 2), "ft -> conversion en metres"))
      data$Swath <- data$Swath * 0.3048  # pieds -> metres
    } else if (!is.na(swath_unit) && grepl("^in$|inch|pouce", swath_unit, ignore.case = TRUE)) {
      message(paste("Swath en pouces (metadata:", swath_unit, ") -> conversion en metres"))
      data$Swath <- data$Swath * 0.0254
    } else if (!is.na(swath_unit) && grepl("^m$|meter|metre", swath_unit, ignore.case = TRUE)) {
      message(paste("Swath deja en metres (metadata:", swath_unit, ")"))
    } else {
      # Detection heuristique (existante)
      message(paste("Swath moyen detecte:", round(mean_swath, 2), "(pas d'unite metadata)"))
      if (mean_swath > 200 && mean_swath < 500) {
        message("Conversion Swath (pouces) -> metres")
        data$Swath <- data$Swath * 0.0254
      } else if (mean_swath > 50 && mean_swath <= 200) {
        message("Conversion Swath (cm) -> metres")
        data$Swath <- data$Swath / 100
      } else if (mean_swath > 500) {
        message("Conversion Swath (mm) -> metres")
        data$Swath <- data$Swath / 1000
      } else if (mean_swath > 3 && mean_swath <= 50) {
        # Pourrait etre en pieds (3-50 ft = 0.9-15m -> plausible)
        # Heuristique: un swath > 15m est rare, > 20 pieds (6m) est courant
        if (mean_swath > 15) {
          message("Conversion Swath (pieds, heuristique) -> metres")
          data$Swath <- data$Swath * 0.3048
        }
        # Sinon, deja en metres
      }
    }
  }

  # ====================================================================
  # CONVERSION DE LA DISTANCE
  # ====================================================================
  if (!"Distance" %in% names(data) || all(is.na(data$Distance))) {
    dist_cols <- c("Distance", "Dist", "Distance_m", "TravelDist")
    for (col in dist_cols) {
      if (col %in% names(data) && !all(is.na(data[[col]]))) {
        message(paste("Utilisation de", col, "comme Distance"))
        data$Distance <- data[[col]]
        break
      }
    }
  }

  if ("Distance" %in% names(data) && !all(is.na(data$Distance))) {
    mean_dist <- mean(data$Distance[!is.na(data$Distance)], na.rm = TRUE)
    dist_unit <- units[["DISTANCE"]] %||% NA_character_

    if (!is.na(dist_unit) && grepl("^ft$|feet|foot", dist_unit, ignore.case = TRUE)) {
      message(paste("Distance en pieds (metadata:", dist_unit, ") moyenne:", round(mean_dist, 2), "ft -> conversion en metres"))
      data$Distance <- data$Distance * 0.3048  # pieds -> metres
    } else if (!is.na(dist_unit) && grepl("^in$|inch|pouce", dist_unit, ignore.case = TRUE)) {
      message(paste("Distance en pouces (metadata:", dist_unit, ") -> conversion en metres"))
      data$Distance <- data$Distance * 0.0254
    } else if (!is.na(dist_unit) && grepl("^m$|meter|metre", dist_unit, ignore.case = TRUE)) {
      message(paste("Distance deja en metres (metadata:", dist_unit, ")"))
    } else {
      # Detection heuristique (existante)
      message(paste("Distance moyenne detectee:", round(mean_dist, 2), "(pas d'unite metadata)"))
      if (mean_dist > 30 && mean_dist < 200) {
        message("Conversion Distance (pouces) -> metres")
        data$Distance <- data$Distance * 0.0254
      } else if (mean_dist > 200 && mean_dist <= 1000) {
        message("Conversion Distance (cm) -> metres")
        data$Distance <- data$Distance / 100
      } else if (mean_dist > 1000) {
        message("Conversion Distance (mm) -> metres")
        data$Distance <- data$Distance / 1000
      }
    }
  }

  # ====================================================================
  # CONVERSION DE LA VITESSE
  # ====================================================================
  if ("Velocity" %in% names(data) && !all(is.na(data$Velocity))) {
    speed_unit <- units[["VEHICLSPEED"]] %||% NA_character_

    if (!is.na(speed_unit) && grepl("mi1hr-1|mi/hr|mph|mile", speed_unit, ignore.case = TRUE)) {
      message(paste("Vitesse en mi/hr (metadata:", speed_unit, ") -> conversion en m/s"))
      data$Velocity <- data$Velocity * 0.44704  # mi/hr -> m/s
    } else if (!is.na(speed_unit) && grepl("km1hr-1|km/hr|kph|km/h", speed_unit, ignore.case = TRUE)) {
      message(paste("Vitesse en km/h (metadata:", speed_unit, ") -> conversion en m/s"))
      data$Velocity <- data$Velocity / 3.6
    } else if (!is.na(speed_unit) && grepl("m1s-1|m/s", speed_unit, ignore.case = TRUE)) {
      message("Vitesse deja en m/s")
    } else {
      # Heuristique
      mean_speed <- mean(data$Velocity[!is.na(data$Velocity)], na.rm = TRUE)
      if (mean_speed > 1 && mean_speed < 15) {
        # Probablement en mi/hr ou km/h pour une moissonneuse
        message(paste("Vitesse moyenne:", round(mean_speed, 2), "- assume mi/hr -> m/s"))
        data$Velocity <- data$Velocity * 0.44704
      }
    }
  }

  # ====================================================================
  # CREER Swath_m ET Distance_m APRES CONVERSION
  # Cela evite que sf_output.R refasse une detection heuristique erronee
  # (ex: Swath de 2m pour une recolteuse d'oignons serait pris pour des pouces)
  # ====================================================================
  if ("Swath" %in% names(data) && !all(is.na(data$Swath))) {
    data$Swath_m <- data$Swath
    message(paste("Swath_m cree:", round(mean(data$Swath_m, na.rm = TRUE), 2), "m"))
  }
  if ("Distance" %in% names(data) && !all(is.na(data$Distance))) {
    data$Distance_m <- data$Distance
    message(paste("Distance_m cree:", round(mean(data$Distance_m, na.rm = TRUE), 2), "m"))
  }

  # ====================================================================
  # CONVERSION DE L'ELEVATION
  # ====================================================================
  if ("Altitude" %in% names(data) && !all(is.na(data$Altitude))) {
    elev_unit <- units[["Elevation"]] %||% NA_character_

    if (!is.na(elev_unit) && grepl("^ft$|feet|foot", elev_unit, ignore.case = TRUE)) {
      message(paste("Elevation en pieds (metadata:", elev_unit, ") -> conversion en metres"))
      data$Altitude <- data$Altitude * 0.3048
    }
  }

  # ====================================================================
  # CALCUL DE L'INTERVALLE
  # ====================================================================
  if (!"Interval" %in% names(data) || all(is.na(data$Interval))) {
    isotime_col <- if ("IsoTime" %in% names(data)) "IsoTime" else if ("isotime" %in% names(data)) "isotime" else NULL

    if (!is.null(isotime_col) && any(!is.na(data[[isotime_col]]))) {
      message(paste("Calcul de l'intervalle a partir de", isotime_col, "..."))
      # Parser les timestamps ISO et calculer les intervalles
      tryCatch({
        timestamps <- as.POSIXct(data[[isotime_col]], format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC")
        if (sum(!is.na(timestamps)) > 1) {
          intervals <- c(NA, diff(as.numeric(timestamps)))
          # Remplacer les valeurs aberrantes (> 10s ou < 0) par la mediane
          median_interval <- stats::median(intervals[!is.na(intervals) & intervals > 0 & intervals < 10], na.rm = TRUE)
          if (is.na(median_interval)) median_interval <- 1
          intervals[is.na(intervals) | intervals <= 0 | intervals > 10] <- median_interval
          data$Interval <- intervals
          message(paste("Intervalle median calcule:", round(median_interval, 3), "secondes"))
        } else {
          data$Interval <- 1
          message("Timestamps non parsables, utilisation de 1 seconde par defaut")
        }
      }, error = function(e) {
        data$Interval <<- 1
        message(paste("Erreur parsing IsoTime, utilisation de 1s par defaut:", e$message))
      })
    } else {
      message("Interval non trouve, utilisation de 1 seconde par defaut")
      data$Interval <- 1
    }
  }

  # ====================================================================
  # VERIFICATION MOISTURE
  # ====================================================================
  if (!"Moisture" %in% names(data) || all(is.na(data$Moisture))) {
    moist_cols <- c("Moisture", "Humidite", "Moist", "Hum")
    for (col in moist_cols) {
      if (col %in% names(data) && !all(is.na(data[[col]]))) {
        message(paste("Utilisation de", col, "comme Moisture"))
        data$Moisture <- data[[col]]
        break
      }
    }
  }

  # ====================================================================
  # VERIFICATION PASS
  # ====================================================================
  if (!"Pass" %in% names(data) || all(is.na(data$Pass))) {
    pass_cols <- c("Pass", "Passage", "SwathNumber", "LineNumber", "SECTIONID")
    for (col in pass_cols) {
      if (col %in% names(data) && !all(is.na(data[[col]]))) {
        message(paste("Utilisation de", col, "comme Pass"))
        data$Pass <- data[[col]]
        break
      }
    }
  }

  if (!"Pass" %in% names(data) || all(is.na(data$Pass))) {
    message("Pass non trouve, utilisation de 1 par defaut")
    data$Pass <- 1
  }

  # ====================================================================
  # CALCUL DU RENDEMENT SEC
  # ====================================================================
  if ("Moisture" %in% names(data) && !all(is.na(data$Moisture))) {
    # Obtenir l'humidite standard selon la culture
    moisture_std <- get_standard_moisture(data)

    if (moisture_std == 0) {
      # Culture maraichere: pas d'ajustement humidite standard
      # Le rendement est utilise tel quel (deja net)
      message("Culture maraichere: pas d'ajustement humidite, Yield_kg_ha = Yield_kg_ha_wet")
      if ("Yield_kg_ha_wet" %in% names(data) && !all(is.na(data$Yield_kg_ha_wet))) {
        data$Yield_kg_ha <- data$Yield_kg_ha_wet
        message(paste("Rendement:", round(mean(data$Yield_kg_ha, na.rm = TRUE), 1), "kg/ha"))
      }
    } else {
      moisture_factor <- 100 - moisture_std
      message(paste("Humidite standard pour conversion:", moisture_std, "% (facteur:", moisture_factor, ")"))

      if ("Yield_kg_ha_wet" %in% names(data) && !all(is.na(data$Yield_kg_ha_wet))) {
        message("Calcul du rendement sec a partir du rendement humide...")
        data$Yield_kg_ha <- data$Yield_kg_ha_wet * (100 - data$Moisture) / moisture_factor
        message(paste("Rendement sec calcule:", round(mean(data$Yield_kg_ha, na.rm = TRUE), 1), "kg/ha"))
      }
      if ("Flow_Wet" %in% names(data) && !all(is.na(data$Flow_Wet))) {
        data$Flow <- data$Flow_Wet * (100 - data$Moisture) / moisture_factor
      }
    }
  } else {
    # Pas d'humidite disponible - le rendement est utilise tel quel
    message("Pas d'humidite disponible - Yield_kg_ha = Yield_kg_ha_wet")
    if ("Yield_kg_ha_wet" %in% names(data) && !all(is.na(data$Yield_kg_ha_wet))) {
      data$Yield_kg_ha <- data$Yield_kg_ha_wet
      message(paste("Rendement utilise:", round(mean(data$Yield_kg_ha, na.rm = TRUE), 1), "kg/ha"))
    }
  }

  message("Conversion terminee")
  return(data)
}


#' Parser le fichier JSON de metadonnees John Deere
#'
#' Lit le fichier JSON de metadonnees associe a un shapefile John Deere
#' pour en extraire les informations sur les unites, la culture, etc.
#'
#' @param json_path Chemin vers le fichier JSON de metadonnees
#' @return Liste avec les informations de metadonnees ou NULL si absent
#' @noRd
parse_jd_metadata <- function(json_path) {
  if (is.null(json_path) || !file.exists(json_path)) {
    return(NULL)
  }

  tryCatch({
    meta <- jsonlite::fromJSON(json_path)

    # Extraire les unites par attribut
    units <- list()
    if (!is.null(meta$DataAttributes)) {
      attrs <- meta$DataAttributes
      for (i in seq_len(nrow(attrs))) {
        name <- attrs$Name[i]
        unit <- if ("Unit" %in% names(attrs) && !is.na(attrs$Unit[i])) attrs$Unit[i] else NA_character_
        units[[name]] <- unit
      }
    }

    # Extraire les infos culture
    crop_info <- list(
      crop_name = meta$CropName %||% NA_character_,
      crop_token = meta$CropToken %||% NA_character_,
      crop_id = meta$CropId %||% NA_integer_,
      variety = if (!is.null(meta$Product)) meta$Product$ProductName else NA_character_
    )

    # Extraire les infos champ
    # Chercher la date de recolte dans plusieurs champs possibles
    harvest_date <- meta$FieldOperationStartDate %||% meta$HarvestDate %||% meta$StartDate %||% meta$Date %||% NA_character_
    
    field_info <- list(
      client = meta$ClientName %||% NA_character_,
      farm = meta$FarmName %||% NA_character_,
      field = meta$FieldName %||% NA_character_,
      operation = meta$Operation %||% NA_character_,
      season = meta$CropSeason %||% NA_integer_,
      date = harvest_date
    )

    result <- list(
      units = units,
      crop_info = crop_info,
      field_info = field_info,
      raw = meta
    )

    message(paste("Metadonnees JD lues:", crop_info$crop_name,
                  "(", crop_info$crop_token, ") -",
                  field_info$field, field_info$season))

    return(result)
  }, error = function(e) {
    message(paste("Impossible de lire le fichier JSON de metadonnees:", e$message))
    return(NULL)
  })
}


#' Obtenir l'humidite standard selon la culture
#'
#' Retourne l'humidite standard pour le calcul rendement sec.
#'
#' @param data Tibble avec GrainType ou Grain_Type
#' @return Humidite standard en pourcentage
#' @noRd
get_standard_moisture <- function(data) {
  # Verifier la colonne GrainType ou Grain_Type
  grain_col <- if ("GrainType" %in% names(data)) {
    "GrainType"
  } else if ("Grain_Type" %in% names(data)) {
    "Grain_Type"
  } else {
    NULL
  }
  
  if (!is.null(grain_col)) {
    grain <- tolower(unique(data[[grain_col]]))
    
    # Detecter le mais
    if (any(grepl("mais|corn|maize", grain))) {
      return(15.5)  # Mais standard USDA
    }
    
    # Detecter le soja
    if (any(grepl("soja|soy|soybean", grain))) {
      return(13.0)  # Soja standard USDA
    }
    
    # Detecter ble/cereales
    if (any(grepl("ble|wheat|blé|orge|barley|avoine|oat", grain))) {
      return(13.5)  # Ble standard USDA
    }

    # Detecter les cultures maraicheres (pas d'ajustement humidite)
    # Oignons, echalotes, pommes de terre, carottes, etc.
    if (any(grepl("onion|oignon|shallot|echalote|potato|patate|pomme.de.terre|carrot|carotte|betterave|beet|legume|vegetable|celeri|celery|navet|turnip|radis|radish|chou|cabbage|laitue|lettuce", grain))) {
      message(paste("Culture maraichere detectee ('", paste(grain, collapse = ", "), "'), pas d'ajustement humidite standard"))
      return(0)  # Pas d'ajustement humidite pour les cultures maraicheres
    }
    
    # Defaut : mais
    message(paste("GrainType non reconnu ('", paste(grain, collapse = ", "), "'), utilisation 15.5% (mais par defaut)"))
    return(15.5)
  }
  
  # Defaut si pas de GrainType
  message("Pas de colonne GrainType, utilisation 15.5% (mais par defaut)")
  return(15.5)
}


#' Creer des polygones rectangulaires a partir de donnees ponctuelles
#'
#' Convertit un data.frame avec coordonnees, heading, swath et distance
#' en polygones rectangulaires en UTM.
#'
#' @param data Data.frame avec colonnes Longitude, Latitude, et soit
#'   (Swath_m, Distance_m) soit (Swath, Distance) en metres
#' @param heading_col Nom de la colonne heading (defaut: "Heading" ou "heading")
#' @return Objet SF avec polygones en UTM
#' @export
create_polygons_from_data <- function(data, heading_col = NULL) {
  if (!requireNamespace("sf", quietly = TRUE)) {
    rlang::abort("Le package 'sf' est requis")
  }

 # Determiner la colonne heading
 if (is.null(heading_col)) {
   if ("heading" %in% names(data)) {
     heading_col <- "heading"
   } else if ("Heading" %in% names(data)) {
     heading_col <- "Heading"
   } else {
     # Calculer le heading a partir des coordonnees
     message("Calcul du heading a partir des coordonnees...")
     data <- data |>
       dplyr::mutate(
         heading = atan2(
           dplyr::lead(Longitude, default = Longitude[dplyr::n()]) - Longitude,
           dplyr::lead(Latitude, default = Latitude[dplyr::n()]) - Latitude
         ) * 180 / pi
       )
     data$heading[is.na(data$heading)] <- 0
     heading_col <- "heading"
   }
 }

 # Determiner les colonnes swath et distance
 swath_col <- if ("Swath_m" %in% names(data)) "Swath_m" else if ("Swath" %in% names(data)) "Swath" else NULL
 dist_col <- if ("Distance_m" %in% names(data)) "Distance_m" else if ("Distance" %in% names(data)) "Distance" else NULL

 if (is.null(swath_col) || is.null(dist_col)) {
   rlang::abort("Colonnes Swath(_m) et Distance(_m) requises")
 }

 # S'assurer que Swath_m et Distance_m existent
 if (!"Swath_m" %in% names(data)) data$Swath_m <- data[[swath_col]]
 if (!"Distance_m" %in% names(data)) data$Distance_m <- data[[dist_col]]

 # Verifier les colonnes requises
 required_cols <- c("Longitude", "Latitude", heading_col, "Swath_m", "Distance_m")
 missing_cols <- setdiff(required_cols, names(data))
 if (length(missing_cols) > 0) {
   rlang::abort(paste("Colonnes manquantes:", paste(missing_cols, collapse = ", ")))
 }

 # Filtrer les lignes valides
 valid_rows <- complete.cases(data[, required_cols, drop = FALSE])
 if (sum(valid_rows) == 0) {
   rlang::abort("Aucune ligne valide pour creer des polygones")
 }
 data <- data[valid_rows, ]

 message(paste("Creation de", nrow(data), "polygones..."))

 # Determiner la zone UTM
 zone <- floor((mean(data$Longitude) + 180) / 6) + 1
 hemisphere <- if (mean(data$Latitude) >= 0) 326 else 327
 utm_crs <- sf::st_crs(paste0("EPSG:", hemisphere, sprintf("%02d", zone)))

 # Convertir en UTM
 pts_wgs84 <- sf::st_as_sf(data, coords = c("Longitude", "Latitude"), crs = 4326)
 pts_utm <- sf::st_transform(pts_wgs84, utm_crs)
 coords_utm <- sf::st_coordinates(pts_utm)

 # Extraire les valeurs
 x_utm <- coords_utm[, 1]
 y_utm <- coords_utm[, 2]
 heading_rad <- data[[heading_col]] * pi / 180
 half_width <- pmax(data$Swath_m, 0.1) / 2
 half_length <- pmax(data$Distance_m, 0.1) / 2

 # Calculer les offsets
 dx_forward <- sin(heading_rad) * half_length
 dy_forward <- cos(heading_rad) * half_length
 dx_perp <- cos(heading_rad) * half_width
 dy_perp <- -sin(heading_rad) * half_width

 # Creer les polygones
 create_polygon <- function(i) {
   x <- x_utm[i]
   y <- y_utm[i]
   dxf <- dx_forward[i]
   dyf <- dy_forward[i]
   dxp <- dx_perp[i]
   dyp <- dy_perp[i]

   coords <- matrix(c(
     x + dxf + dxp, y + dyf + dyp,
     x + dxf - dxp, y + dyf - dyp,
     x - dxf - dxp, y - dyf - dyp,
     x - dxf + dxp, y - dyf + dyp,
     x + dxf + dxp, y + dyf + dyp
   ), ncol = 2, byrow = TRUE)

   sf::st_polygon(list(coords))
 }

 polygons_list <- lapply(seq_len(nrow(data)), create_polygon)
 polys_utm <- sf::st_sfc(polygons_list, crs = utm_crs)

 # Garder les colonnes de donnees
 data_cols <- names(data)[!names(data) %in% c("Longitude", "Latitude", "geometry")]
 data_valid <- data[, data_cols, drop = FALSE]

 # Creer le sf
 sf_data <- sf::st_sf(data_valid, geometry = polys_utm)

 message(paste("Polygones crees en UTM zone", zone))
 return(sf_data)
}


#' Lire des donnees John Deere et convertir en polygones metriques
#'
#' Lit un fichier ZIP John Deere (rendement, semis, vitesse, etc.) ou un
#' fichier vectoriel (shapefile, GeoPackage, GeoJSON) et cree des polygones.
#' Utilise les unites du JSON de metadonnees pour convertir en metrique.
#'
#' @param file_path Chemin vers un fichier ZIP ou un fichier vectoriel
#'   (.shp, .gpkg, .geojson)
#' @param field_name Nom du champ dans le fichier ZIP. Ignore si file_path est
#'   un fichier vectoriel. Si NULL et que le ZIP ne contient qu'un seul
#'   shapefile, celui-ci est utilise automatiquement.
#' @return Objet SF avec polygones et toutes les colonnes preservees
#' @export
read_jd_to_polygons <- function(file_path, field_name = NULL) {
  if (!file.exists(file_path)) {
    rlang::abort(paste("Le fichier n'existe pas:", file_path))
  }

  if (!requireNamespace("sf", quietly = TRUE)) {
    rlang::abort("Le package 'sf' est requis")
  }

  ext <- tolower(tools::file_ext(file_path))

  if (ext == "zip") {
    temp_dir <- tempfile(pattern = "jd_zip_")
    dir.create(temp_dir, showWarnings = FALSE, recursive = TRUE)

    fields <- list_fields_from_zip(file_path)

    if (is.null(field_name) && nrow(fields) == 1) {
      field_name <- fields$field_name[1]
    }
    if (is.null(field_name) || !field_name %in% fields$field_name) {
      rlang::abort(paste("Champ", field_name, "non trouve dans le ZIP - specifiez 'field_name'"))
    }

    shp_file <- fields$Name[fields$field_name == field_name]
    base_name <- tools::file_path_sans_ext(shp_file)

    zip_contents <- utils::unzip(file_path, list = TRUE)
    related_files <- zip_contents$Name[grepl(paste0("^", base_name, "\\."), zip_contents$Name, ignore.case = TRUE)]

    json_files <- zip_contents$Name[grepl(paste0("^", base_name, ".*-Deere-Metadata\\.json$"),
                                          zip_contents$Name, ignore.case = TRUE)]
    all_files <- unique(c(related_files, json_files))

    utils::unzip(file_path, files = all_files, exdir = temp_dir)

    metadata <- NULL
    if (length(json_files) > 0) {
      json_path <- file.path(temp_dir, json_files[1])
      if (!file.exists(json_path)) {
        json_path <- file.path(temp_dir, basename(json_files[1]))
      }
      metadata <- parse_jd_metadata(json_path)
    }

    shp_path <- file.path(temp_dir, shp_file)
    if (!file.exists(shp_path)) {
      shp_path <- file.path(temp_dir, basename(shp_file))
    }

    data <- sf::st_read(shp_path, quiet = TRUE)
    unlink(temp_dir, recursive = TRUE)

  } else if (ext %in% c("shp", "gpkg", "geojson", "json")) {
    data <- sf::st_read(file_path, quiet = TRUE)

    # Rechercher un JSON de metadonnees adjacent (-Deere-Metadata.json)
    metadata <- NULL
    base_name <- basename(tools::file_path_sans_ext(file_path))
    json_candidates <- list.files(
      dirname(file_path),
      pattern = paste0("^", base_name, ".*-Deere-Metadata\\.json$"),
      full.names = TRUE, ignore.case = TRUE
    )
    if (length(json_candidates) == 0) {
      json_candidates <- list.files(dirname(file_path),
                                    pattern = "-Deere-Metadata\\.json$",
                                    full.names = TRUE, ignore.case = TRUE)
    }
    if (length(json_candidates) > 0) {
      metadata <- parse_jd_metadata(json_candidates[1])
    }

  } else {
    rlang::abort(paste("Format de fichier non supporte:", ext,
                       "- fournir un ZIP ou un fichier .shp/.gpkg/.geojson"))
  }

  data <- sf::st_sf(sf::st_drop_geometry(data), geometry = sf::st_geometry(data))

  data <- convert_units_from_json(data, metadata)

  if (inherits(data, "sf")) {
    coords <- sf::st_coordinates(data)
    if (ncol(coords) >= 2) {
      data$Longitude <- coords[, 1]
      data$Latitude <- coords[, 2]
    }
    metadata_attrs <- attributes(data)[!names(attributes(data)) %in%
      c("names", "class", "row.names", "sf_column", "agr")]
    data <- sf::st_drop_geometry(data)
    for (attr_name in names(metadata_attrs)) {
      attr(data, attr_name) <- metadata_attrs[[attr_name]]
    }
  }

  data <- standardize_jd_columns(data, metadata = metadata)

  # Renommer les colonnes _m generees par convert_units_from_json
  col_m_mapping <- c(
    "SWATHWIDTH_m" = "Swath_m",
    "DISTANCE_m" = "Distance_m",
    "ELEVATION_m" = "Altitude_m",
    "VELOCITY_m" = "Velocity_m"
  )
  for (old_name in names(col_m_mapping)) {
    if (old_name %in% names(data)) {
      data[[col_m_mapping[[old_name]]]] <- data[[old_name]]
      data[[old_name]] <- NULL
    }
  }

  # S'assurer que Swath_m et Distance_m existent
  if (!"Swath_m" %in% names(data) && "Swath" %in% names(data)) {
    data$Swath_m <- data$Swath
  }
  if (!"Distance_m" %in% names(data) && "Distance" %in% names(data)) {
    data$Distance_m <- data$Distance
  }

  # Creer les polygones avec la fonction utilitaire
  sf_data <- create_polygons_from_data(data)

  return(sf_data)
}


#' Lire des donnees depuis un fichier vectoriel (shapefile, GeoPackage, GeoJSON)
#'
#' Equivalent de read_jd_to_polygons mais retourne un data frame de points
#' standardise (sans geometrie), pret pour le pipeline de nettoyage.
#' Utile pour les fichiers de semis, d'epandage, etc. qui ne sont pas du rendement.
#'
#' @param file_path Chemin vers un fichier .shp, .gpkg ou .geojson
#' @return Data frame avec colonnes standardisees et coordonnees Longitude/Latitude
#' @export
read_yield_from_vector <- function(file_path) {
  if (!file.exists(file_path)) {
    rlang::abort(paste("Le fichier n'existe pas:", file_path))
  }

  if (!requireNamespace("sf", quietly = TRUE)) {
    rlang::abort("Le package 'sf' est requis")
  }

  ext <- tolower(tools::file_ext(file_path))
  if (!ext %in% c("shp", "gpkg", "geojson", "json")) {
    rlang::abort(paste("Format de fichier non supporte:", ext,
                       "- fournir un fichier .shp/.gpkg/.geojson"))
  }

  data <- sf::st_read(file_path, quiet = TRUE)
  data <- sf::st_sf(sf::st_drop_geometry(data), geometry = sf::st_geometry(data))

  # Rechercher un JSON de metadonnees adjacent (-Deere-Metadata.json)
  metadata <- NULL
  base_name <- basename(tools::file_path_sans_ext(file_path))
  json_candidates <- list.files(
    dirname(file_path),
    pattern = paste0("^", base_name, ".*-Deere-Metadata\\.json$"),
    full.names = TRUE, ignore.case = TRUE
  )
  if (length(json_candidates) == 0) {
    json_candidates <- list.files(dirname(file_path),
                                  pattern = "-Deere-Metadata\\.json$",
                                  full.names = TRUE, ignore.case = TRUE)
  }
  if (length(json_candidates) > 0) {
    metadata <- parse_jd_metadata(json_candidates[1])
  }

  # Convertir les unites selon le JSON
  data <- convert_units_from_json(data, metadata)

  # Extraire les coordonnees et detacher la geometrie
  if (inherits(data, "sf")) {
    coords <- sf::st_coordinates(data)
    if (ncol(coords) >= 2) {
      data$Longitude <- coords[, 1]
      data$Latitude <- coords[, 2]
    }
    metadata_attrs <- attributes(data)[!names(attributes(data)) %in%
      c("names", "class", "row.names", "sf_column", "agr")]
    data <- sf::st_drop_geometry(data)
    for (attr_name in names(metadata_attrs)) {
      attr(data, attr_name) <- metadata_attrs[[attr_name]]
    }
  }

  # Standardiser les colonnes John Deere
  data <- standardize_jd_columns(data, metadata = metadata)

  return(data)
}


#' Convertir les unites selon le JSON metadata
#'
#' Parcourt toutes les colonnes et applique la conversion si l'unite est dans le JSON.
#' Gere les unites imperiales -> metrique.
#'
#' @param data Data frame avec les donnees
#' @param metadata Metadonnees JSON (optionnel)
#' @return Data frame avec unites converties et colonnes _m creees
#' @noRd
convert_units_from_json <- function(data, metadata = NULL) {
  message("Conversion des unites selon le JSON...")

  if (is.null(metadata)) {
    metadata <- attr(data, "jd_metadata")
  }

  units <- if (!is.null(metadata)) metadata$units else list()

  if (length(units) > 0) {
    message("Unites trouvees dans le JSON:")
    for (u in names(units)) {
      if (!is.na(units[[u]])) message(paste("  -", u, ":", units[[u]]))
    }
  }

  unit_conversions <- list(
    "ft" = list(to = "m", factor = 0.3048),
    "feet" = list(to = "m", factor = 0.3048),
    "foot" = list(to = "m", factor = 0.3048),
    "in" = list(to = "m", factor = 0.0254),
    "inch" = list(to = "m", factor = 0.0254),
    "mi1hr-1" = list(to = "m/s", factor = 0.44704),
    "mph" = list(to = "m/s", factor = 0.44704),
    "mi/hr" = list(to = "m/s", factor = 0.44704),
    "km1hr-1" = list(to = "m/s", factor = 1/3.6),
    "km/h" = list(to = "m/s", factor = 1/3.6),
    "ton1ac-1" = list(to = "kg/ha", factor = 2241.7),
    "ton/ac" = list(to = "kg/ha", factor = 2241.7),
    "t1ha-1" = list(to = "kg/ha", factor = 1000),
    "t/ha" = list(to = "kg/ha", factor = 1000),
    "bu1ac-1" = list(to = "kg/ha", factor = NA),
    "kg1ha-1" = list(to = "kg/ha", factor = 1),
    "kg/ha" = list(to = "kg/ha", factor = 1),
    "ton" = list(to = "kg", factor = 907.185),
    "tons" = list(to = "kg", factor = 907.185),
    "tonne" = list(to = "kg", factor = 1000),
    "kg" = list(to = "kg", factor = 1),
    "lb1ac-1" = list(to = "kg/ha", factor = 1.12085),
    "lb/ac" = list(to = "kg/ha", factor = 1.12085),
    "seeds1ac-1" = list(to = "seeds/ha", factor = 2.47105),
    "seeds/ac" = list(to = "seeds/ha", factor = 2.47105),
    "gal" = list(to = "L", factor = 3.78541),
    "gallon" = list(to = "L", factor = 3.78541),
    "l" = list(to = "L", factor = 1),
    "psi" = list(to = "kPa", factor = 6.89476),
    "bar" = list(to = "kPa", factor = 100),
    "prcnt" = list(to = "%", factor = 1)
  )

  col_mapping <- c(
    "DISTANCE" = "DISTANCE",
    "SWATH" = "SWATHWIDTH",
    "SWATHWIDTH" = "SWATHWIDTH",
    "VELOCITY" = "VEHICLSPEED",
    "VEHICLSPEED" = "VEHICLSPEED",
    "ELEVATION" = "Elevation",
    "ALTITUDE" = "ELEVATION",
    "APPLIEDRATE" = "AppliedRate",
    "CONTROLRATE" = "ControlRate",
    "TARGETRATE" = "TargetRate",
    "FUEL" = "FUEL",
    "NETYLDA" = "NetYldA",
    "GROSSYLDA" = "GrossYldA",
    "NETYLD" = "NetYld",
    "GROSSYLD" = "GrossYld",
    "TRASH" = "Trash"
  )

  for (col in names(data)) {
    col_upper <- toupper(col)
    lookup_key <- if (col_upper %in% names(units)) {
      col_upper
    } else if (col_upper %in% names(col_mapping) && col_mapping[[col_upper]] %in% names(units)) {
      col_mapping[[col_upper]]
    } else {
      NA_character_
    }

    if (is.na(lookup_key)) next

    unit <- units[[lookup_key]]
    if (is.na(unit)) {
      message(paste("  SKIP:", col, "(unit=NA)"))
      next
    }

    unit_lower <- tolower(unit)

    if (unit_lower %in% names(unit_conversions)) {
      conv <- unit_conversions[[unit_lower]]
      if (!is.na(conv$factor)) {
        data[[col]] <- data[[col]] * conv$factor
        message(paste(" ", col, ":", unit, "->", conv$to, "(x", round(conv$factor, 4), ")"))

        if (conv$to == "m" && toupper(col) %in% c("DISTANCE", "SWATH", "SWATHWIDTH", "ELEVATION", "ALTITUDE")) {
          data[[paste0(col, "_m")]] <- data[[col]]
        }
      }
    } else {
      message(paste("  Unite non reconnue pour", col, ":", unit))
    }
  }

  message("Conversion terminee")
  return(data)
}



