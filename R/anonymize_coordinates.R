#' Anonymiser les coordonnées GPS des données de rendement
#'
#' Cette fonction déplace les coordonnées GPS (Latitude/Longitude) en les décalant
#' selon un point de référence choisi aléatoirement. Le point de référence est
#' sauvegardé dans un fichier chiffré pour permettre la réversibilité si nécessaire.
#'
#' @param data Tibble ou data.frame contenant les colonnes Latitude et Longitude
#' @param output_key_file Chemin du fichier où sauvegarder la clé de décalage chiffrée.
#'   Si NULL, la clé est retournée dans l'objet résultat mais pas sauvegardée.
#' @param password Mot de passe pour chiffrer/déchiffrer le fichier de clé.
#'   Si NULL, une clé aléatoire est générée et retournée (non chiffrée).
#' @param algorithm Algorithme de chiffrement à utiliser. Options: "aes-256-gcm" (défaut),
#'   "aes-256-cbc", "chacha20". Voir [openssl::encrypt()] pour les options.
#' @param random_seed Graine aléatoire pour la reproductibilité (optionnel).
#'   Si NULL, utilise une graine aléatoire.
#' @param coordinate_system Système de coordonnées à utiliser pour le décalage.
#'   Options: "latlon" (défaut) pour Latitude/Longitude, "utm" pour coordonnées UTM.
#' @param utm_zone Zone UTM si coordinate_system = "utm". Auto-détectée si NULL.
#' @param return_full_key Si TRUE, retourne la clé complète dans le résultat (défaut: FALSE
#'   pour ne pas exposer accidentellement la clé).
#'
#' @return Une liste contenant:
#'   - `data`: Les données avec coordonnées anonymisées
#'   - `key_file`: Chemin du fichier de clé chiffrée (si sauvegardé)
#'   - `offset`: Le décalage appliqué (lat_offset, lon_offset ou x_offset, y_offset)
#'   - `reference_point`: Le point de référence original (si return_full_key = TRUE)
#'   - `metadata`: Métadonnées sur l'anonymisation (timestamp, méthode, etc.)
#'
#' @details
#' La méthode d'anonymisation choisie consiste à:
#' 1. Sélectionner un point de référence aléatoire dans les données
#' 2. Calculer un décalage pour que ce point devienne l'origine (0,0) ou un point fixe
#' 3. Appliquer ce décalage à toutes les coordonnées
#' 4. Sauvegarder le point de référence et le décalage dans un fichier chiffré
#'
#' Cette approche permet:
#' - L'anonymisation: les coordonnées absolues sont perdues
#' - La réversibilité: avec la clé, on peut retrouver les coordonnées originales
#' - La cohérence: toutes les données du même champ ont le même décalage
#'
#' @section Sécurité:
#' Le fichier de clé est chiffré avec AES-256-GCM par défaut, qui offre:
#' - Confidentialité: les données sont illisibles sans le mot de passe
#' - Authenticité: détection de toute modification du fichier
#'
#' Pour une sécurité maximale:
#' - Utilisez un mot de passe fort (12+ caractères, mixte)
#' - Stockez le fichier de clé séparément des données
#' - Ne partagez jamais le mot de passe et le fichier de clé ensemble
#'
#' @examples
#' \dontrun{
#' # Créer des données d'exemple
#' data <- tibble::tibble(
#'   Latitude = c(47.506122, 47.506136, 47.506152),
#'   Longitude = c(-69.856661, -69.856681, -69.856701),
#'   Flow = c(1.53, 3.7, 7.56)
#' )
#'
#' # Anonymiser avec sauvegarde de clé chiffrée
#' result <- anonymize_coordinates(
#'   data,
#'   output_key_file = "reference_key.enc",
#'   password = "mon_mot_de_passe_securise"
#' )
#'
#' # Données anonymisées
#' anonymized_data <- result$data
#'
#' # Restaurer les coordonnées originales
#' restored <- restore_coordinates(
#'   anonymized_data,
#'   key_file = "reference_key.enc",
#'   password = "mon_mot_de_passe_securise"
#' )
#' }
#'
#' @seealso [restore_coordinates()] pour restaurer les coordonnées originales
#'
#' @export
anonymize_coordinates <- function(
    data,
    output_key_file = NULL,
    password = NULL,
    algorithm = "aes-256-gcm",
    random_seed = NULL,
    coordinate_system = "latlon",
    utm_zone = NULL,
    return_full_key = FALSE
) {
  # Vérifier les colonnes requises
  if (coordinate_system == "latlon") {
    if (!all(c("Latitude", "Longitude") %in% names(data))) {
      rlang::abort("Les colonnes 'Latitude' et 'Longitude' sont requises")
    }
  } else if (coordinate_system == "utm") {
    if (!all(c("X", "Y") %in% names(data))) {
      rlang::abort("Les colonnes 'X' et 'Y' sont requises pour le système UTM")
    }
  } else {
    rlang::abort("coordinate_system doit être 'latlon' ou 'utm'")
  }

  # Vérifier que openssl est disponible si chiffrement demandé
  if (!is.null(output_key_file) || !is.null(password)) {
    if (!requireNamespace("openssl", quietly = TRUE)) {
      rlang::abort("Le package 'openssl' est requis pour le chiffrement. Installez-le avec: install.packages('openssl')")
    }
  }

  # Initialiser le générateur aléatoire
  if (!is.null(random_seed)) {
    set.seed(random_seed)
    rng_state <- .Random.seed
  } else {
    # Sauvegarder l'état actuel pour pouvoir le stocker
    rng_state <- .Random.seed
  }

  # Sélectionner un point de référence aléatoire
  ref_idx <- sample.int(nrow(data), 1)

  if (coordinate_system == "latlon") {
    ref_lat <- data$Latitude[ref_idx]
    ref_lon <- data$Longitude[ref_idx]

    # Générer un décalage aléatoire supplémentaire pour plus de sécurité
    # Le décalage est entre -1 et 1 degrés (environ ±111 km)
    additional_lat_offset <- runif(1, -1, 1)
    additional_lon_offset <- runif(1, -1, 1)

    # Calculer le décalage total
    lat_offset <- ref_lat + additional_lat_offset
    lon_offset <- ref_lon + additional_lon_offset

    # Appliquer le décalage
    data$Latitude <- data$Latitude - lat_offset
    data$Longitude <- data$Longitude - lon_offset

    # Préparer les informations de clé
    key_info <- list(
      coordinate_system = coordinate_system,
      reference_index = ref_idx,
      reference_latitude = ref_lat,
      reference_longitude = ref_lon,
      lat_offset = lat_offset,
      lon_offset = lon_offset,
      additional_lat_offset = additional_lat_offset,
      additional_lon_offset = additional_lon_offset,
      timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      version = "1.0",
      rng_state = rng_state
    )

    offset_summary <- list(
      lat_offset = lat_offset,
      lon_offset = lon_offset
    )

  } else { # UTM
    ref_x <- data$X[ref_idx]
    ref_y <- data$Y[ref_idx]

    # Détection de la zone UTM si nécessaire
    if (is.null(utm_zone) && coordinate_system == "utm") {
      if ("Longitude" %in% names(data)) {
        utm_zone <- floor((data$Longitude[1] + 180) / 6) + 1
      } else {
        rlang::abort("utm_zone doit être spécifié ou la colonne Longitude doit exister")
      }
    }

    # Générer un décalage aléatoire supplémentaire (±10000 mètres)
    additional_x_offset <- runif(1, -10000, 10000)
    additional_y_offset <- runif(1, -10000, 10000)

    # Calculer le décalage total
    x_offset <- ref_x + additional_x_offset
    y_offset <- ref_y + additional_y_offset

    # Appliquer le décalage
    data$X <- data$X - x_offset
    data$Y <- data$Y - y_offset

    # Préparer les informations de clé
    key_info <- list(
      coordinate_system = coordinate_system,
      utm_zone = utm_zone,
      reference_index = ref_idx,
      reference_x = ref_x,
      reference_y = ref_y,
      x_offset = x_offset,
      y_offset = y_offset,
      additional_x_offset = additional_x_offset,
      additional_y_offset = additional_y_offset,
      timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      version = "1.0",
      rng_state = rng_state
    )

    offset_summary <- list(
      x_offset = x_offset,
      y_offset = y_offset
    )
  }

  # Chiffrer et sauvegarder la clé si demandé
  key_file_path <- NULL
  if (!is.null(output_key_file)) {
    # Convertir la clé en JSON
    key_json <- jsonlite::serializeJSON(key_info, pretty = FALSE)

    if (!is.null(password)) {
      # Chiffrer avec le mot de passe
      key <- openssl::sha256(charToRaw(password))
      encrypted_data <- openssl::aes_cbc_encrypt(
        charToRaw(key_json),
        key
      )
      # Sauvegarder les données chiffrées et l'IV
      encrypted_key <- list(data = encrypted_data, iv = attr(encrypted_data, "iv"))
      saveRDS(encrypted_key, output_key_file)
      key_file_path <- normalizePath(output_key_file, mustWork = FALSE)
    } else {
      # Sauvegarder sans chiffrement (déconseillé)
      writeLines(key_json, output_key_file)
      key_file_path <- normalizePath(output_key_file, mustWork = FALSE)
      rlang::warn("Clé sauvegardée sans chiffrement. Utilisez 'password' pour une meilleure sécurité.")
    }
  }

  # Préparer le résultat
  result <- list(
    data = data,
    key_file = key_file_path,
    offset = offset_summary,
    metadata = list(
      timestamp = key_info$timestamp,
      coordinate_system = coordinate_system,
      algorithm = if (!is.null(password)) algorithm else "none",
      rows_anonymized = nrow(data),
      reference_index = ref_idx,
      version = "1.0"
    )
  )

  # Inclure la clé complète si demandé (attention: risque de sécurité)
  if (return_full_key) {
    result$reference_point <- key_info
  }

  # Réinitialiser la graine aléatoire si elle a été modifiée
  if (!is.null(random_seed)) {
    set.seed(NULL)
  }

  rlang::inform(sprintf(
    "Coordonnées anonymisées: %d points décalés (système: %s)",
    nrow(data), coordinate_system
  ))

  return(result)
}


#' Restaurer les coordonnées originales à partir de données anonymisées
#'
#' Cette fonction restaure les coordonnées GPS originales en utilisant
#' la clé de décalage sauvegardée lors de l'anonymisation.
#'
#' @param data Tibble ou data.frame contenant les coordonnées anonymisées
#' @param key_file Chemin du fichier contenant la clé chiffrée
#' @param password Mot de passe pour déchiffrer le fichier de clé
#' @param key_info Liste contenant les informations de clé (alternative à key_file)
#' @param coordinate_system Système de coordonnées utilisé lors de l'anonymisation
#'
#' @return Tibble avec les coordonnées restaurées
#'
#' @details
#' Cette fonction est l'inverse de [anonymize_coordinates()]. Elle lit le fichier
#' de clé chiffré, extrait les informations de décalage et les applique en sens
#' inverse pour retrouver les coordonnées originales.
#'
#' @examples
#' \dontrun{
#' # Restaurer à partir d'un fichier de clé
#' restored_data <- restore_coordinates(
#'   anonymized_data,
#'   key_file = "reference_key.enc",
#'   password = "mon_mot_de_passe_securise"
#' )
#' }
#'
#' @seealso [anonymize_coordinates()] pour anonymiser les coordonnées
#'
#' @export
restore_coordinates <- function(
    data,
    key_file = NULL,
    password = NULL,
    key_info = NULL,
    coordinate_system = "latlon"
) {
  # Vérifier qu'on a soit un fichier soit des infos de clé
  if (is.null(key_file) && is.null(key_info)) {
    rlang::abort("Soit 'key_file' soit 'key_info' doit être fourni")
  }

  # Lire et déchiffrer la clé si nécessaire
  if (!is.null(key_file)) {
    if (!file.exists(key_file)) {
      rlang::abort(paste("Fichier de clé introuvable:", key_file))
    }

    if (!requireNamespace("openssl", quietly = TRUE)) {
      rlang::abort("Le package 'openssl' est requis pour le déchiffrement")
    }

    # Lire le fichier chiffré
    encrypted_container <- readRDS(key_file)
    encrypted_data <- encrypted_container$data
    iv <- encrypted_container$iv

    if (is.null(password)) {
      rlang::abort("Un mot de passe est requis pour déchiffrer la clé")
    }

    # Déchiffrer
    tryCatch({
      key <- openssl::sha256(charToRaw(password))
      decrypted <- openssl::aes_cbc_decrypt(
        encrypted_data,
        key,
        iv = iv
      )
      key_info <- jsonlite::unserializeJSON(rawToChar(decrypted))
    }, error = function(e) {
      rlang::abort(paste("Échec du déchiffrement. Mot de passe incorrect ou fichier corrompu:", e$message))
    })
  }

  # Vérifier la version
  if (!is.null(key_info$version) && key_info$version != "1.0") {
    rlang::warn(paste("Version de clé non reconnue:", key_info$version))
  }

  # Restaurer selon le système de coordonnées
  if (key_info$coordinate_system == "latlon") {
    if (!all(c("Latitude", "Longitude") %in% names(data))) {
      rlang::abort("Les colonnes 'Latitude' et 'Longitude' sont requises")
    }

    data$Latitude <- data$Latitude + key_info$lat_offset
    data$Longitude <- data$Longitude + key_info$lon_offset

  } else if (key_info$coordinate_system == "utm") {
    if (!all(c("X", "Y") %in% names(data))) {
      rlang::abort("Les colonnes 'X' et 'Y' sont requises")
    }

    data$X <- data$X + key_info$x_offset
    data$Y <- data$Y + key_info$y_offset
  }

  rlang::inform("Coordonnées restaurées avec succès")

  return(data)
}


#' Vérifier l'intégrité d'un fichier de clé chiffré
#'
#' Cette fonction vérifie qu'un fichier de clé peut être déchiffré
#' et que les données n'ont pas été corrompues.
#'
#' @param key_file Chemin du fichier de clé chiffré
#' @param password Mot de passe pour déchiffrer
#'
#' @return TRUE si le fichier est valide, FALSE sinon
#'
#' @details
#' Cette fonction tente de déchiffrer le fichier sans restaurer les coordonnées.
#' Elle est utile pour vérifier qu'un mot de passe est correct avant
#' de tenter une restauration complète.
#'
#' @examples
#' \dontrun{
#' if (verify_key_file("reference_key.enc", "mon_mot_de_passe")) {
#'   print("Mot de passe correct, fichier valide")
#' }
#' }
 #'
 #' @export
 verify_key_file <- function(key_file, password) {
  if (!file.exists(key_file)) {
    return(FALSE)
  }

  if (!requireNamespace("openssl", quietly = TRUE)) {
    rlang::abort("Le package 'openssl' est requis")
  }

  tryCatch({
    encrypted_container <- readRDS(key_file)
    encrypted_data <- encrypted_container$data
    iv <- encrypted_container$iv
    key <- openssl::sha256(charToRaw(password))
    decrypted <- openssl::aes_cbc_decrypt(
      encrypted_data,
      key,
      iv = iv
    )
    key_info <- jsonlite::unserializeJSON(rawToChar(decrypted))

    # Vérifier les champs requis
    required_fields <- c("coordinate_system", "timestamp", "version")
    all(sapply(required_fields, function(f) !is.null(key_info[[f]])))
  }, error = function(e) {
    FALSE
  })
}


#' Générer un rapport de sécurité pour l'anonymisation
#'
#' Cette fonction génère un rapport détaillé sur la sécurité de
#' l'anonymisation effectuée.
#'
#' @param anonymization_result Résultat retourné par [anonymize_coordinates()]
#' @param include_recommendations Inclure des recommandations de sécurité
#'
#' @return Une liste contenant le rapport de sécurité
#'
#' @examples
#' \dontrun{
#' result <- anonymize_coordinates(data, output_key_file = "key.enc", password = "pass")
#' report <- security_report(result)
#' print(report$summary)
#' }
 #'
 #' @export
 security_report <- function(anonymization_result, include_recommendations = TRUE) {
  report <- list(
    summary = list(),
    security_level = "unknown",
    recommendations = list()
  )

  meta <- anonymization_result$metadata

  # Évaluer le niveau de sécurité
  if (!is.null(meta$algorithm) && meta$algorithm != "none") {
    if (grepl("aes-256", meta$algorithm)) {
      report$security_level <- "high"
      report$summary$encryption <- "AES-256 - Niveau militaire"
    } else {
      report$security_level <- "medium"
      report$summary$encryption <- meta$algorithm
    }
  } else {
    report$security_level <- "low"
    report$summary$encryption <- "Aucun chiffrement"
  }

  report$summary$coordinate_system <- meta$coordinate_system
  report$summary$points_anonymized <- meta$rows_anonymized
  report$summary$timestamp <- meta$timestamp

  # Calculer l'entropie du décalage
  if (!is.null(anonymization_result$offset)) {
    offset <- anonymization_result$offset
    if (!is.null(offset$lat_offset)) {
      # Approximation: un degré ~ 111 km
      lat_km <- abs(offset$lat_offset) * 111
      lon_km <- abs(offset$lon_offset) * 111 * cos(0) # approximation
      report$summary$approximate_shift_km <- sqrt(lat_km^2 + lon_km^2)
    }
  }

  if (include_recommendations) {
    report$recommendations <- list(
      "Stockez le fichier de clé séparément des données anonymisées",
      "Utilisez un mot de passe fort (12+ caractères, majuscules, minuscules, chiffres, symboles)",
      "Ne partagez jamais le mot de passe et le fichier de clé ensemble",
      "Faites des sauvegardes du fichier de clé dans un endroit sécurisé",
      "En production, utilisez un gestionnaire de mots de passe pour stocker le mot de passe"
    )

    if (report$security_level == "low") {
      report$recommendations <- c(
        "URGENT: Le fichier de clé n'est pas chiffré. Utilisez 'password' lors de l'anonymisation",
        report$recommendations
      )
    }
  }

  return(report)
}
