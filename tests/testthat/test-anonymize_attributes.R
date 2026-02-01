# Tests pour les fonctions de suppression d'attributs sensibles et stockage securise

library(testthat)
library(yieldcleanr)

# Donnees de test
create_test_data_with_attrs <- function(n = 50) {
  set.seed(42)
  tibble::tibble(
    Latitude = 47.5 + runif(n, -0.01, 0.01),
    Longitude = -69.85 + runif(n, -0.01, 0.01),
    Flow = runif(n, 1, 10),
    Moisture = runif(n, 10, 20),
    Serial = sample(1000:9999, n, replace = TRUE),
    FieldID = rep("CHAMP_A", n),
    LoadID = rep("LOAD_001", n),
    GPS_Time = as.numeric(Sys.time()) + 1:n,
    Pass = sample(1:5, n, replace = TRUE),
    GrainType = rep("Mais", n),
    .row_id = 1:n
  )
}

describe("remove_sensitive_attributes", {
  it("devrait supprimer les colonnes par defaut", {
    data <- create_test_data_with_attrs(50)

    result <- remove_sensitive_attributes(data, verbose = FALSE)

    # Verifier que les colonnes sensibles sont supprimees
    expect_false("Serial" %in% names(result))
    expect_false("FieldID" %in% names(result))
    expect_false("LoadID" %in% names(result))
    expect_false("GPS_Time" %in% names(result))

    # Verifier que les autres colonnes sont conservees
    expect_true("Latitude" %in% names(result))
    expect_true("Longitude" %in% names(result))
    expect_true("Flow" %in% names(result))
    expect_true("Moisture" %in% names(result))
    expect_true("Pass" %in% names(result))
    expect_true("GrainType" %in% names(result))
  })

  it("devrait supprimer uniquement les colonnes specifiees", {
    data <- create_test_data_with_attrs(50)

    result <- remove_sensitive_attributes(
      data,
      columns_to_remove = c("Serial", "Variety"),
      verbose = FALSE
    )

    # Serial devrait etre supprime
    expect_false("Serial" %in% names(result))
    # Variety n'existe pas dans les donnees, donc pas d'erreur
    # Les autres colonnes par defaut devraient etre conservees
    expect_true("FieldID" %in% names(result))
    expect_true("LoadID" %in% names(result))
    expect_true("GPS_Time" %in% names(result))
  })

  it("devrait conserver le nombre de lignes", {
    data <- create_test_data_with_attrs(50)
    result <- remove_sensitive_attributes(data, verbose = FALSE)

    expect_equal(nrow(result), 50)
  })

  it("devrait ajouter les attributs d'anonymisation", {
    data <- create_test_data_with_attrs(50)
    result <- remove_sensitive_attributes(data, verbose = FALSE)

    expect_true(!is.null(attr(result, "sensitive_attrs_removed")))
    expect_true(!is.null(attr(result, "anonymization_date")))
    expect_true("Serial" %in% attr(result, "sensitive_attrs_removed"))
  })

  it("devrait generaliser GPS_Time avec keep_temporal", {
    data <- create_test_data_with_attrs(50)

    result <- remove_sensitive_attributes(
      data,
      columns_to_remove = c("Serial", "FieldID", "LoadID", "GPS_Time"),
      keep_temporal = TRUE,
      verbose = FALSE
    )

    # GPS_Time devrait etre remplace par Time_Relative_Min
    expect_false("GPS_Time" %in% names(result))
    expect_true("Time_Relative_Min" %in% names(result))

    # Le temps relatif devrait commencer a 0
    expect_equal(min(result$Time_Relative_Min), 0, tolerance = 0.1)
  })

  it("devrait echouer avec un objet non-data.frame", {
    expect_error(
      remove_sensitive_attributes(list(a = 1, b = 2)),
      "data.frame"
    )
  })
})

describe("list_sensitive_columns", {
  it("devrait retourner un vecteur de noms en mode basic", {
    cols <- list_sensitive_columns(detail_level = "basic")

    expect_true(is.character(cols))
    expect_true("Serial" %in% cols)
    expect_true("FieldID" %in% cols)
    expect_true("LoadID" %in% cols)
    expect_true("GPS_Time" %in% cols)
  })

  it("devrait retourner un data.frame en mode full", {
    cols <- list_sensitive_columns(detail_level = "full")

    expect_true(is.data.frame(cols))
    expect_true("Column" %in% names(cols))
    expect_true("sensitivity" %in% names(cols))
    expect_true("recommendation" %in% names(cols))
  })
})

describe("anonymize_yield_data", {
  it("devrait anonymiser completement avec variable d'environnement", {
    skip_if_not_installed("openssl")

    # Configurer la variable d'environnement
    Sys.setenv(YIELD_ANON_PASSWORD = "test_password_123")

    data <- create_test_data_with_attrs(50)
    temp_dir <- tempfile()
    dir.create(temp_dir, recursive = TRUE, showWarnings = FALSE)
    temp_key <- file.path(temp_dir, "key.enc")

    result <- anonymize_yield_data(
      data,
      key_file = temp_key,
      password = NULL,  # Utiliser la variable d'environnement
      env_var = "YIELD_ANON_PASSWORD",
      columns_to_remove = c("Serial", "FieldID", "LoadID"),
      verbose = FALSE
    )

    # Verifier la structure du resultat
    expect_true("data" %in% names(result))
    expect_true("key_file" %in% names(result))
    expect_true("report" %in% names(result))
    expect_true("metadata" %in% names(result))

    # Verifier que les donnees sont anonymisees
    expect_false(identical(result$data$Latitude, data$Latitude))
    expect_false("Serial" %in% names(result$data))
    expect_false("FieldID" %in% names(result$data))

    # Verifier le rapport de securite
    expect_equal(result$report$security_level, "high")

    # Nettoyer
    unlink(temp_dir, recursive = TRUE)
    Sys.unsetenv("YIELD_ANON_PASSWORD")
  })

  it("devrait echouer si aucun mot de passe n'est fourni", {
    # S'assurer que la variable d'environnement n'est pas definie
    Sys.unsetenv("YIELD_ANON_PASSWORD")

    data <- create_test_data_with_attrs(50)

    expect_error(
      anonymize_yield_data(data, password = NULL, verbose = FALSE),
      "Aucun mot de passe"
    )
  })

  it("devrait creer le repertoire de sortie si necessaire", {
    skip_if_not_installed("openssl")

    Sys.setenv(YIELD_ANON_PASSWORD = "test_password_123")

    data <- create_test_data_with_attrs(50)
    temp_dir <- tempfile()

    result <- anonymize_yield_data(
      data,
      output_dir = temp_dir,
      password = NULL,
      env_var = "YIELD_ANON_PASSWORD",
      verbose = FALSE
    )

    expect_true(dir.exists(temp_dir))
    expect_true(file.exists(result$key_file))

    # Nettoyer
    unlink(temp_dir, recursive = TRUE)
    Sys.unsetenv("YIELD_ANON_PASSWORD")
  })
})

describe("setup_secure_password_storage", {
  it("devrait configurer une variable d'environnement", {
    # S'assurer que la variable n'existe pas
    Sys.unsetenv("TEST_YIELD_PASSWORD")

    result <- setup_secure_password_storage(
      method = "env_var",
      env_var = "TEST_YIELD_PASSWORD",
      password = "mon_mot_de_passe_test"
    )

    expect_true(result)
    expect_equal(Sys.getenv("TEST_YIELD_PASSWORD"), "mon_mot_de_passe_test")

    # Nettoyer
    Sys.unsetenv("TEST_YIELD_PASSWORD")
  })

  it("devrait demander interactivement si pas de mot de passe", {
    # En mode non-interactif, devrait echouer
    expect_error(
      setup_secure_password_storage(
        method = "env_var",
        env_var = "TEST_PASSWORD",
        password = NULL
      ),
      "Mot de passe requis"
    )
  })
})

describe("get_secure_password", {
  it("devrait recuperer depuis une variable d'environnement", {
    Sys.setenv("TEST_GET_PASSWORD" = "password_123")

    pwd <- get_secure_password(
      method = "env_var",
      env_var = "TEST_GET_PASSWORD"
    )

    expect_equal(pwd, "password_123")

    Sys.unsetenv("TEST_GET_PASSWORD")
  })

  it("devrait retourner NULL si variable non definie", {
    Sys.unsetenv("TEST_NONEXISTENT")

    pwd <- get_secure_password(
      method = "env_var",
      env_var = "TEST_NONEXISTENT"
    )

    expect_null(pwd)
  })
})

describe("workflow complet avec stockage securise", {
  it("devrait anonymiser avec configuration securisee complete", {
    skip_if_not_installed("openssl")

    # Etape 1: Configurer le stockage securise
    setup_secure_password_storage(
      method = "env_var",
      env_var = "YIELD_SECURE_PASSWORD",
      password = "mon_super_mot_de_passe_123!"
    )

    # Etape 2: Creer les donnees
    data <- create_test_data_with_attrs(100)
    original_lat <- data$Latitude
    original_lon <- data$Longitude

    # Etape 3: Anonymiser
    temp_dir <- tempfile()
    result <- anonymize_yield_data(
      data,
      output_dir = temp_dir,
      password = NULL,  # Utilise la variable d'environnement
      env_var = "YIELD_SECURE_PASSWORD",
      columns_to_remove = c("Serial", "FieldID", "LoadID", "GPS_Time"),
      coordinate_system = "latlon",
      verbose = FALSE
    )

    # Verifier l'anonymisation
    expect_false(identical(result$data$Latitude, original_lat))
    expect_false("Serial" %in% names(result$data))
    expect_false("GPS_Time" %in% names(result$data))
    expect_equal(nrow(result$data), 100)

    # Verifier le fichier de cle
    expect_true(file.exists(result$key_file))

    # Etape 4: Restaurer les coordonnees
    restored <- restore_coordinates(
      result$data,
      key_file = result$key_file,
      password = get_secure_password(
        method = "env_var",
        env_var = "YIELD_SECURE_PASSWORD"
      )
    )

    # Verifier la restauration
    expect_equal(restored$Latitude, original_lat, tolerance = 1e-10)
    expect_equal(restored$Longitude, original_lon, tolerance = 1e-10)

    # Nettoyer
    unlink(temp_dir, recursive = TRUE)
    Sys.unsetenv("YIELD_SECURE_PASSWORD")
  })
})
