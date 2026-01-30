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
  read_yield_data <- function(file_path = NULL, data = NULL, col_names = TRUE) {
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
  on.exit(unlink(temp_dir, recursive = TRUE))
  
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
  
  # Extraire les fichiers
  utils::unzip(zip_path, files = related_files, exdir = temp_dir)
  
  # Lire le shapefile
  shp_path <- file.path(temp_dir, shp_file)
  if (!file.exists(shp_path)) {
    shp_path <- file.path(temp_dir, basename(shp_file))
  }
  
  data <- sf::st_read(shp_path, quiet = TRUE)
  
  # Standardiser les colonnes John Deere
  data <- standardize_jd_columns(data)
  
  # Convertir les unites John Deere metriques vers le format yieldcleanr
  data <- convert_jd_metric_to_yieldcleanr(data)
  
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
standardize_jd_columns <- function(data) {
  # Debug: afficher les noms de colonnes trouves
  original_names <- names(data)
  message(paste("Colonnes trouvees dans le shapefile:", paste(original_names, collapse = ", ")))
  
  # Mapping des noms de colonnes John Deere vers yieldcleanr
  jd_mapping <- c(
    "DISTANCE" = "Distance",
    "SWATHWIDTH" = "Swath",
    "VRYIELDMAS" = "Flow",
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
  
  # S'assurer que les colonnes essentielles existent
  essential_cols <- c("Flow", "Moisture", "Swath", "Pass", "Longitude", "Latitude")
  for (col in essential_cols) {
    if (!col %in% names(data)) {
      data[[col]] <- NA_real_
      message(paste("Colonne", col, "manquante, initialisee avec NA"))
    }
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
convert_jd_metric_to_yieldcleanr <- function(data) {
  message("Conversion des donnees John Deere metriques...")
  
  # Si les donnees sont deja en tonnes/ha ou kg/ha, on les garde telles quelles
  # mais on cree les colonnes attendues par le pipeline
  
  # Verifier si Flow existe et contient des valeurs
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
  
  # Convertir les unites si necessaire
  # Flow contient TOUJOURS le rendement HUMIDE (quelle que soit la source)
  if ("Flow" %in% names(data) && !all(is.na(data$Flow))) {
    mean_flow <- mean(data$Flow[!is.na(data$Flow)], na.rm = TRUE)
    message(paste("Rendement humide moyen:", round(mean_flow, 2)))
    
    # Si le rendement est > 100, c'est probablement en kg/ha
    # Si le rendement est < 20, c'est probablement en tonnes/ha
    if (mean_flow < 20 && mean_flow > 0) {
      message("Conversion tonnes/ha -> kg/ha")
      data$Flow <- data$Flow * 1000  # tonnes -> kg
    }
    
    # Flow contient le rendement HUMIDE
    data$Yield_kg_ha_wet <- data$Flow
    message(paste("Yield_kg_ha_wet (humide) cree avec", sum(!is.na(data$Yield_kg_ha_wet)), "valeurs"))
    
    # Yield_kg_ha (sec) sera calcule plus tard a partir de l'humidite
    # Ne pas creer Yield_kg_ha ici pour forcer le calcul avec humidite
  }
  
  # Creer Yield_kg_ha_wet si Flow_Wet existe (donnees avec rendement humide explicite)
  if ("Flow_Wet" %in% names(data) && !all(is.na(data$Flow_Wet))) {
    mean_flow_wet <- mean(data$Flow_Wet[!is.na(data$Flow_Wet)], na.rm = TRUE)
    message(paste("Rendement humide explicite moyen:", round(mean_flow_wet, 2)))
    
    if (mean_flow_wet < 20 && mean_flow_wet > 0) {
      message("Conversion rendement humide tonnes/ha -> kg/ha")
      data$Flow_Wet <- data$Flow_Wet * 1000  # tonnes -> kg
    }
    
    # Si Yield_kg_ha_wet n'existe pas encore, le creer
    if (!"Yield_kg_ha_wet" %in% names(data) || all(is.na(data$Yield_kg_ha_wet))) {
      data$Yield_kg_ha_wet <- data$Flow_Wet
      message(paste("Yield_kg_ha_wet cree a partir de Flow_Wet avec", sum(!is.na(data$Yield_kg_ha_wet)), "valeurs"))
    }
  }
  
  # Verifier Swath (largeur)
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
  
  # Convertir Swath en metres si necessaire
  if ("Swath" %in% names(data) && !all(is.na(data$Swath))) {
    mean_swath <- mean(data$Swath[!is.na(data$Swath)], na.rm = TRUE)
    message(paste("Swath moyen detecte:", round(mean_swath, 2)))
    
    if (mean_swath > 200 && mean_swath < 500) {
      message("Conversion Swath (pouces) -> metres")
      data$Swath <- data$Swath * 0.0254  # inches -> m
    } else if (mean_swath > 50 && mean_swath <= 200) {
      message("Conversion Swath (cm) -> metres")
      data$Swath <- data$Swath / 100  # cm -> m
    } else if (mean_swath > 500) {
      message("Conversion Swath (mm) -> metres")
      data$Swath <- data$Swath / 1000  # mm -> m
    }
  }
  
  # Verifier Distance
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
  
  # Convertir Distance en metres si necessaire
  if ("Distance" %in% names(data) && !all(is.na(data$Distance))) {
    mean_dist <- mean(data$Distance[!is.na(data$Distance)], na.rm = TRUE)
    message(paste("Distance moyenne detectee:", round(mean_dist, 2)))
    
    if (mean_dist > 30 && mean_dist < 200) {
      message("Conversion Distance (pouces) -> metres")
      data$Distance <- data$Distance * 0.0254  # inches -> m
    } else if (mean_dist > 200 && mean_dist <= 1000) {
      message("Conversion Distance (cm) -> metres")
      data$Distance <- data$Distance / 100  # cm -> m
    } else if (mean_dist > 1000) {
      message("Conversion Distance (mm) -> metres")
      data$Distance <- data$Distance / 1000  # mm -> m
    }
  }
  
  # Verifier Interval
  if (!"Interval" %in% names(data) || all(is.na(data$Interval))) {
    isotime_col <- if ("IsoTime" %in% names(data)) "IsoTime" else if ("isotime" %in% names(data)) "isotime" else NULL
    
    if (!is.null(isotime_col) && any(!is.na(data[[isotime_col]]))) {
      message(paste("Calcul de l'intervalle a partir de", isotime_col, "..."))
      data$Interval <- 1  # Valeur par defaut
    } else {
      message("Interval non trouve, utilisation de 1 seconde par defaut")
      data$Interval <- 1
    }
  }
  
  # Verifier Moisture
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
  
  # Verifier Pass
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
  
  # Calculer le rendement sec a partir du rendement humide et de l'humidite
  if ("Moisture" %in% names(data) && !all(is.na(data$Moisture))) {
    # Obtenir l'humidite standard selon la culture
    moisture_std <- get_standard_moisture(data)
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
  } else {
    # Pas d'humidite - le rendement sec est egal au rendement humide
    message("Pas d'humidite disponible - Yield_kg_ha = Yield_kg_ha_wet")
    if ("Yield_kg_ha_wet" %in% names(data) && !all(is.na(data$Yield_kg_ha_wet))) {
      data$Yield_kg_ha <- data$Yield_kg_ha_wet
      message(paste("Rendement utilise (pas de conversion humidite):", round(mean(data$Yield_kg_ha, na.rm = TRUE), 1), "kg/ha"))
    }
  }
  
  message("Conversion terminee")
  return(data)
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
    
    # Defaut : mais
    message(paste("GrainType non reconnu ('", paste(grain, collapse = ", "), "'), utilisation 15.5% (mais par defaut)"))
    return(15.5)
  }
  
  # Defaut si pas de GrainType
  message("Pas de colonne GrainType, utilisation 15.5% (mais par defaut)")
  return(15.5)
}
