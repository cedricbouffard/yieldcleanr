#' Supprimer les attributs sensibles des donnees de rendement
#'
#' Cette fonction supprime les colonnes identifiantes et sensibles des donnees
#' de rendement pour renforcer l'anonymisation. Elle peut egalement generaliser
#' certaines colonnes (arrondir, categoriser) plutot que de les supprimer completement.
#'
#' @param data Tibble ou data.frame avec les donnees de rendement
#' @param columns_to_remove Vecteur des noms de colonnes a supprimer.
#'   Par defaut: c("Serial", "FieldID", "LoadID", "GPS_Time")
#' @param generalize_cols Liste nommee definissant les colonnes a generaliser
#'   au lieu de supprimer. Ex: list(GPS_Time = "hour", Pass = "none")
#' @param keep_temporal Si TRUE, conserve une version temporelle relative
#'   (minutes depuis le debut) au lieu de supprimer GPS_Time completement
#' @param verbose Si TRUE, affiche des informations sur les colonnes traitees
#'
#' @return Tibble avec les colonnes sensibles supprimees ou generalisees
#'
#' @details
#' Les colonnes suivantes sont considerees comme sensibles par defaut:
#' - Serial: Numero de serie de la machine (identifiant unique)
#' - FieldID: Identifiant du champ (peut reveler la propriete)
#' - LoadID: Identifiant de la charge (lien avec d'autres donnees)
#' - GPS_Time: Horodatage precis (peut etre croise avec d'autres sources)
#'
#' Options de generalisation pour GPS_Time:
#' - "none": Supprime completement
#' - "hour": Garde uniquement l'heure (sans minutes/secondes)
#' - "day": Garde uniquement la date (sans heure)
#' - "relative": Convertit en minutes ecoulees depuis le debut
#'
#' @examples
#' \dontrun{
#' # Supprimer les colonnes sensibles par defaut
#' data_clean <- remove_sensitive_attributes(data)
#'
#' # Supprimer des colonnes specifiques
#' data_clean <- remove_sensitive_attributes(
#'   data,
#'   columns_to_remove = c("Serial", "FieldID", "Variety")
#' )
#'
#' # Generaliser plutot que supprimer
#' data_clean <- remove_sensitive_attributes(
#'   data,
#'   generalize_cols = list(GPS_Time = "hour", Pass = "none"),
#'   columns_to_remove = c("Serial", "FieldID", "LoadID")
#' )
#' }
#'
#' @seealso [anonymize_coordinates()] pour l'anonymisation spatiale
#'
#' @export
remove_sensitive_attributes <- function(
    data,
    columns_to_remove = c("Serial", "FieldID", "LoadID", "GPS_Time"),
    generalize_cols = NULL,
    keep_temporal = FALSE,
    verbose = TRUE
) {
  # Verifier que data est un data.frame ou tibble
  if (!is.data.frame(data)) {
    rlang::abort("'data' doit etre un data.frame ou tibble")
  }

  # Copie pour eviter de modifier l'original
  result <- data
  actions_taken <- list()

  # Traiter les colonnes a generaliser
  if (!is.null(generalize_cols)) {
    for (col_name in names(generalize_cols)) {
      if (col_name %in% names(result)) {
        method <- generalize_cols[[col_name]]

        if (method == "hour" && col_name == "GPS_Time") {
          # Convertir en heure uniquement (si c'est un timestamp)
          if (is.numeric(result[[col_name]])) {
            # Supposer que c'est un timestamp Unix
            result[[col_name]] <- as.POSIXct(result[[col_name]], origin = "1970-01-01")
          }
          if (inherits(result[[col_name]], "POSIXct")) {
            result[[col_name]] <- format(result[[col_name]], "%Y-%m-%d %H:00:00")
            actions_taken[[col_name]] <- "generalise a l'heure"
          }
        } else if (method == "day" && col_name == "GPS_Time") {
          # Garder uniquement la date
          if (is.numeric(result[[col_name]])) {
            result[[col_name]] <- as.POSIXct(result[[col_name]], origin = "1970-01-01")
          }
          if (inherits(result[[col_name]], "POSIXct")) {
            result[[col_name]] <- format(result[[col_name]], "%Y-%m-%d")
            actions_taken[[col_name]] <- "generalise au jour"
          }
        } else if (method == "relative" && col_name == "GPS_Time") {
          # Convertir en temps relatif (minutes depuis le debut)
          if (is.numeric(result[[col_name]])) {
            min_time <- min(result[[col_name]], na.rm = TRUE)
            result[[col_name]] <- (result[[col_name]] - min_time) / 60
            actions_taken[[col_name]] <- "converti en minutes relatives"
          }
        } else if (method == "none") {
          # Supprimer cette colonne
          result[[col_name]] <- NULL
          actions_taken[[col_name]] <- "supprime"
        }

        # Retirer des colonnes a supprimer si elle etait generalisee
        columns_to_remove <- setdiff(columns_to_remove, col_name)
      }
    }
  }

  # Traiter keep_temporal pour GPS_Time
  gps_time_processed <- FALSE
  if (keep_temporal && "GPS_Time" %in% columns_to_remove && "GPS_Time" %in% names(result)) {
    if (is.numeric(result$GPS_Time)) {
      min_time <- min(result$GPS_Time, na.rm = TRUE)
      result$Time_Relative_Min <- (result$GPS_Time - min_time) / 60
      actions_taken[["GPS_Time"]] <- "converti en Time_Relative_Min"
      gps_time_processed <- TRUE
    }
    # Retirer GPS_Time de columns_to_remove pour ne pas le supprimer deux fois
    # mais on le supprimera manuellement apres
    columns_to_remove <- setdiff(columns_to_remove, "GPS_Time")
  }

  # Supprimer les colonnes restantes
  cols_actually_removed <- intersect(columns_to_remove, names(result))
  for (col in cols_actually_removed) {
    result[[col]] <- NULL
    actions_taken[[col]] <- "supprime"
  }

  # Si GPS_Time a ete converti en temps relatif, le supprimer maintenant
  if (gps_time_processed && "GPS_Time" %in% names(result)) {
    result$GPS_Time <- NULL
  }

  # Afficher le resume si verbose
  if (verbose && length(actions_taken) > 0) {
    rlang::inform("Attributs sensibles traites:")
    for (col in names(actions_taken)) {
      rlang::inform(paste0("  - ", col, ": ", actions_taken[[col]]))
    }
  }

  # Ajouter un attribut pour tracer l'anonymisation
  attr(result, "sensitive_attrs_removed") <- names(actions_taken)
  attr(result, "anonymization_date") <- Sys.time()

  return(result)
}


#' Liste des colonnes considerees comme sensibles
#'
#' Cette fonction retourne une liste des colonnes typiquement considerees
#' comme sensibles dans les donnees de rendement, avec leur niveau de
#' sensibilite et des recommandations.
#'
#' @param detail_level Niveau de detail: "basic" (defaut), "full"
#'
#' @return Une liste ou data.frame avec les informations sur les colonnes sensibles
#'
#' @examples
#' \dontrun{
#' # Liste basique
#' sensitive_cols <- list_sensitive_columns()
#'
#' # Liste detaillee avec recommandations
#' sensitive_cols <- list_sensitive_columns(detail_level = "full")
#' print(sensitive_cols)
#' }
 #'
 #' @export
 list_sensitive_columns <- function(detail_level = "basic") {
  cols <- list(
    Serial = list(
      description = "Numero de serie de la moissonneuse",
      sensitivity = "eleve",
      reason = "Identifiant unique de la machine",
      recommendation = "Supprimer ou hasher"
    ),
    FieldID = list(
      description = "Identifiant du champ",
      sensitivity = "eleve",
      reason = "Peut reveler la propriete ou la localisation",
      recommendation = "Supprimer ou anonymiser"
    ),
    LoadID = list(
      description = "Identifiant de la charge",
      sensitivity = "moyen",
      reason = "Lien avec d'autres donnees (camions, silos)",
      recommendation = "Supprimer"
    ),
    GPS_Time = list(
      description = "Horodatage GPS",
      sensitivity = "moyen",
      reason = "Peut etre croise avec d'autres sources",
      recommendation = "Generaliser (heure/jour) ou rendre relatif"
    ),
    Pass = list(
      description = "Numero de passage",
      sensitivity = "faible",
      reason = "Information technique",
      recommendation = "Conserver ou generaliser"
    ),
    GrainType = list(
      description = "Type de culture",
      sensitivity = "faible",
      reason = "Information agronomique",
      recommendation = "Conserver"
    ),
    Variety = list(
      description = "Variete",
      sensitivity = "moyen",
      reason = "Peut etre specifique a l'exploitation",
      recommendation = "Generaliser ou supprimer"
    ),
    Machine = list(
      description = "Type de machine",
      sensitivity = "faible",
      reason = "Information technique",
      recommendation = "Conserver"
    )
  )

  if (detail_level == "basic") {
    return(names(cols))
  } else {
    # Convertir en data.frame pour un affichage plus facile
    df <- do.call(rbind, lapply(names(cols), function(name) {
      cbind(Column = name, as.data.frame(cols[[name]], stringsAsFactors = FALSE))
    }))
    rownames(df) <- NULL
    return(df)
  }
}


#' Pipeline complet d'anonymisation des donnees de rendement
#'
#' Cette fonction applique l'ensemble des etapes d'anonymisation:
#' 1. Anonymisation spatiale (coordonnees)
#' 2. Suppression des attributs sensibles
#' 3. Chiffrement et stockage securise de la cle
#'
#' @param data Tibble avec les donnees de rendement
#' @param key_file Chemin du fichier pour stocker la cle de deplacement
#' @param password Mot de passe pour chiffrer la cle (ou NULL pour utiliser
#'   une variable d'environnement)
#' @param env_var Nom de la variable d'environnement contenant le mot de passe
#'   (defaut: "YIELD_ANON_PASSWORD")
#' @param columns_to_remove Colonnes sensibles a supprimer
#' @param coordinate_system Systeme de coordonnees: "latlon" ou "utm"
#' @param output_dir Repertoire de sortie pour les donnees anonymisees
#' @param verbose Si TRUE, affiche les informations de progression
#'
#' @return Une liste contenant:
#'   - data: Donnees anonymisees
#'   - key_file: Chemin du fichier de cle
#'   - report: Rapport de securite
#'   - metadata: Metadonnees de l'anonymisation
#'
#' @examples
#' \dontrun{
#' # Anonymisation complete avec mot de passe depuis variable d'environnement
#' Sys.setenv(YIELD_ANON_PASSWORD = "mon_mot_de_passe_securise")
#'
#' result <- anonymize_yield_data(
#'   data,
#'   key_file = "chemin/vers/cle.enc",
#'   output_dir = "donnees/anonymisees"
#' )
#'
#' # Sauvegarder les donnees anonymisees
#' write.csv(result$data, file.path(result$output_dir, "rendement_anon.csv"))
#' }
#'
#' @seealso [anonymize_coordinates()], [remove_sensitive_attributes()]
#'
#' @export
anonymize_yield_data <- function(
    data,
    key_file = NULL,
    password = NULL,
    env_var = "YIELD_ANON_PASSWORD",
    columns_to_remove = c("Serial", "FieldID", "LoadID", "GPS_Time"),
    coordinate_system = "latlon",
    output_dir = NULL,
    verbose = TRUE
) {
  # Recuperer le mot de passe si non fourni
  if (is.null(password)) {
    password <- Sys.getenv(env_var)
    if (password == "") {
      rlang::abort(paste(
        "Aucun mot de passe fourni. Veuillez:",
        "1. Fournir 'password' directement, ou",
        "2. Definir la variable d'environnement", env_var,
        "avec: Sys.setenv(", env_var, " = 'votre_mot_de_passe')"
      ))
    }
    if (verbose) {
      rlang::inform(paste("Mot de passe recupere depuis la variable d'environnement:", env_var))
    }
  }

  # Creer le repertoire de sortie si necessaire
  if (!is.null(output_dir) && !dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    if (verbose) {
      rlang::inform(paste("Repertoire de sortie cree:", output_dir))
    }
  }

  # Etape 1: Anonymisation spatiale
  if (verbose) {
    rlang::inform("Etape 1/3: Anonymisation des coordonnees spatiales...")
  }

  if (is.null(key_file) && !is.null(output_dir)) {
    key_file <- file.path(output_dir, "coordinate_key.enc")
  }

  coord_result <- anonymize_coordinates(
    data = data,
    output_key_file = key_file,
    password = password,
    coordinate_system = coordinate_system
  )

  # Etape 2: Suppression des attributs sensibles
  if (verbose) {
    rlang::inform("Etape 2/3: Suppression des attributs sensibles...")
  }

  anonymized_data <- remove_sensitive_attributes(
    data = coord_result$data,
    columns_to_remove = columns_to_remove,
    verbose = verbose
  )

  # Etape 3: Generer le rapport de securite
  if (verbose) {
    rlang::inform("Etape 3/3: Generation du rapport de securite...")
  }

  security_rep <- security_report(coord_result)

  # Preparer le resultat
  result <- list(
    data = anonymized_data,
    key_file = coord_result$key_file,
    output_dir = output_dir,
    coordinate_offset = coord_result$offset,
    removed_columns = columns_to_remove,
    report = security_rep,
    metadata = list(
      timestamp = Sys.time(),
      rows_anonymized = nrow(anonymized_data),
      coordinate_system = coordinate_system,
      env_var_used = env_var,
      version = "1.0"
    )
  )

  if (verbose) {
    rlang::inform("Anonymisation complete!")
    rlang::inform(paste("  - Points anonymises:", nrow(anonymized_data)))
    rlang::inform(paste("  - Colonnes supprimees:", paste(columns_to_remove, collapse = ", ")))
    rlang::inform(paste("  - Niveau de securite:", security_rep$security_level))
    if (!is.null(coord_result$key_file)) {
      rlang::inform(paste("  - Fichier de cle:", coord_result$key_file))
    }
  }

  return(result)
}


#' Configurer le stockage securise des mots de passe
#'
#' Cette fonction aide a configurer le stockage securise des mots de passe
#' via des variables d'environnement ou des fichiers de configuration securises.
#'
#' @param method Methode de stockage: "env_var" (defaut), "keyring", "file"
#' @param env_var Nom de la variable d'environnement (pour method = "env_var")
#' @param password Mot de passe a stocker (si NULL, demande interactivement)
#' @param keyring_service Nom du service pour keyring (defaut: "yieldcleanr")
#' @param config_file Chemin du fichier de configuration (pour method = "file")
#'
#' @return TRUE si la configuration a reussi, FALSE sinon
#'
#' @details
#' Methodes disponibles:
#' - "env_var": Stocke dans une variable d'environnement (session uniquement)
#' - "keyring": Utilise le package keyring pour un stockage securise persistant
#' - "file": Stocke dans un fichier chiffre (necessite le package openssl)
#'
#' @examples
#' \dontrun{
#' # Configuration avec variable d'environnement
#' setup_secure_password_storage(
#'   method = "env_var",
#'   env_var = "YIELD_ANON_PASSWORD",
#'   password = "mon_mot_de_passe"
#' )
#'
#' # Configuration avec keyring (plus securise)
#' setup_secure_password_storage(
#'   method = "keyring",
#'   password = "mon_mot_de_passe"
#' )
#' }
 #'
 #' @export
 setup_secure_password_storage <- function(
    method = "env_var",
    env_var = "YIELD_ANON_PASSWORD",
    password = NULL,
    keyring_service = "yieldcleanr",
    config_file = NULL
) {
  # Demander le mot de passe interactivement si non fourni
  if (is.null(password)) {
    if (interactive()) {
      password <- readline("Entrez le mot de passe: ")
      confirm <- readline("Confirmez le mot de passe: ")
      if (password != confirm) {
        rlang::abort("Les mots de passe ne correspondent pas")
      }
    } else {
      rlang::abort("Mot de passe requis en mode non-interactif")
    }
  }

  switch(method,
         "env_var" = {
           # Stocker dans une variable d'environnement
           # Utiliser do.call pour passer dynamiquement le nom de variable
           args <- list(password)
           names(args) <- env_var
           do.call(Sys.setenv, args)
           rlang::inform(paste("Mot de passe stocke dans la variable d'environnement:", env_var))
           rlang::inform("Note: Ce stockage est temporaire (session uniquement)")
           rlang::inform(paste("Pour le rendre permanent, ajoutez a votre .Renviron:", env_var, "=votre_mot_de_passe"))
           return(TRUE)
         },

         "keyring" = {
           # Utiliser le package keyring
           if (!requireNamespace("keyring", quietly = TRUE)) {
             rlang::abort("Package 'keyring' requis. Installez avec: install.packages('keyring')")
           }
           keyring::key_set_with_value(
             service = keyring_service,
             username = "anonymization",
             password = password
           )
           rlang::inform("Mot de passe stocke dans le keyring systeme")
           return(TRUE)
         },

         "file" = {
           # Stocker dans un fichier chiffre
           if (!requireNamespace("openssl", quietly = TRUE)) {
             rlang::abort("Package 'openssl' requis. Installez avec: install.packages('openssl')")
           }

           if (is.null(config_file)) {
             config_file <- file.path(Sys.getenv("HOME"), ".yieldcleanr_config.enc")
           }

           # Generer une cle aleatoire pour le fichier
           file_key <- openssl::rand_bytes(32)

           # Chiffrer le mot de passe
           encrypted_pwd <- openssl::aes_cbc_encrypt(
             charToRaw(password),
             file_key
           )

           # Sauvegarder
           saveRDS(list(key = file_key, data = encrypted_pwd), config_file)

           rlang::inform(paste("Mot de passe stocke dans:", config_file))
           rlang::warn("IMPORTANT: La cle de chiffrement est stockee dans le meme fichier!")
           rlang::warn("Cette methode est moins securisee que keyring.")
           return(TRUE)
         },

         {
           rlang::abort(paste("Methode inconnue:", method))
         }
  )
}


#' Recuperer un mot de passe stocke de maniere securisee
#'
#' @param method Methode de stockage utilisee: "env_var", "keyring", "file"
#' @param env_var Nom de la variable d'environnement
#' @param keyring_service Nom du service keyring
#' @param config_file Chemin du fichier de configuration
#'
#' @return Le mot de passe recupere ou NULL si non trouve
 #'
 #' @export
 get_secure_password <- function(
    method = "env_var",
    env_var = "YIELD_ANON_PASSWORD",
    keyring_service = "yieldcleanr",
    config_file = NULL
) {
  switch(method,
         "env_var" = {
           pwd <- Sys.getenv(env_var)
           if (pwd == "") {
             return(NULL)
           }
           return(pwd)
         },

         "keyring" = {
           if (!requireNamespace("keyring", quietly = TRUE)) {
             return(NULL)
           }
           tryCatch({
             keyring::key_get(service = keyring_service, username = "anonymization")
           }, error = function(e) {
             NULL
           })
         },

         "file" = {
           if (!requireNamespace("openssl", quietly = TRUE)) {
             return(NULL)
           }

           if (is.null(config_file)) {
             config_file <- file.path(Sys.getenv("HOME"), ".yieldcleanr_config.enc")
           }

           if (!file.exists(config_file)) {
             return(NULL)
           }

           tryCatch({
             container <- readRDS(config_file)
             decrypted <- openssl::aes_cbc_decrypt(
               container$data,
               container$key
             )
             rawToChar(decrypted)
           }, error = function(e) {
             NULL
           })
         },

         {
           return(NULL)
         }
  )
}
