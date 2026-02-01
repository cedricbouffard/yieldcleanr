# Tests pour les fonctions d'anonymisation des coordonnées

library(testthat)
library(yieldcleanr)

# Créer des données de test
create_test_data <- function(n = 100) {
  set.seed(42)
  tibble::tibble(
    Latitude = 47.5 + runif(n, -0.01, 0.01),
    Longitude = -69.85 + runif(n, -0.01, 0.01),
    Flow = runif(n, 1, 10),
    Moisture = runif(n, 10, 20),
    .row_id = 1:n
  )
}

create_test_data_utm <- function(n = 100) {
  set.seed(42)
  tibble::tibble(
    X = 435000 + runif(n, -1000, 1000),
    Y = 5262000 + runif(n, -1000, 1000),
    Longitude = -69.85 + runif(n, -0.01, 0.01),  # Pour détection zone UTM
    Flow = runif(n, 1, 10),
    .row_id = 1:n
  )
}

describe("anonymize_coordinates", {
  it("devrait anonymiser les coordonnées lat/lon correctement", {
    data <- create_test_data(50)
    original_lat <- data$Latitude
    original_lon <- data$Longitude
    
    # Anonymiser sans chiffrement
    result <- anonymize_coordinates(data, password = NULL)
    
    # Vérifier que les coordonnées ont changé
    expect_false(identical(result$data$Latitude, original_lat))
    expect_false(identical(result$data$Longitude, original_lon))
    
    # Vérifier que les autres colonnes sont préservées
    expect_equal(result$data$Flow, data$Flow)
    expect_equal(result$data$Moisture, data$Moisture)
    
    # Vérifier la structure du résultat
    expect_true("data" %in% names(result))
    expect_true("offset" %in% names(result))
    expect_true("metadata" %in% names(result))
    expect_null(result$key_file)  # Pas de fichier sans output_key_file
  })
  
  it("devrait générer un décalage différent à chaque appel (sans graine)", {
    data <- create_test_data(50)
    
    result1 <- anonymize_coordinates(data, password = NULL)
    result2 <- anonymize_coordinates(data, password = NULL)
    
    # Les décalages devraient être différents
    expect_false(identical(result1$offset, result2$offset))
  })
  
  it("devrait produire le même décalage avec la même graine", {
    data <- create_test_data(50)
    
    result1 <- anonymize_coordinates(data, password = NULL, random_seed = 123)
    result2 <- anonymize_coordinates(data, password = NULL, random_seed = 123)
    
    # Les décalages devraient être identiques
    expect_equal(result1$offset, result2$offset)
  })
  
  it("devrait sauvegarder la clé dans un fichier si demandé", {
    skip_if_not_installed("openssl")
    
    data <- create_test_data(50)
    temp_file <- tempfile(fileext = ".enc")
    
    result <- anonymize_coordinates(
      data,
      output_key_file = temp_file,
      password = "test_password_123"
    )
    
    # Vérifier que le fichier a été créé
    expect_true(file.exists(temp_file))
    expect_equal(result$key_file, normalizePath(temp_file))
    
    # Nettoyer
    unlink(temp_file)
  })
  
  it("devrait émettre un avertissement si clé sauvegardée sans chiffrement", {
    data <- create_test_data(50)
    temp_file <- tempfile(fileext = ".json")
    
    expect_warning(
      result <- anonymize_coordinates(
        data,
        output_key_file = temp_file,
        password = NULL
      ),
      "sans chiffrement"
    )
    
    unlink(temp_file)
  })
  
  it("devrait fonctionner avec le système UTM", {
    data <- create_test_data_utm(50)
    original_x <- data$X
    original_y <- data$Y
    
    result <- anonymize_coordinates(
      data,
      coordinate_system = "utm",
      password = NULL
    )
    
    # Vérifier que les coordonnées ont changé
    expect_false(identical(result$data$X, original_x))
    expect_false(identical(result$data$Y, original_y))
    
    # Vérifier les métadonnées
    expect_equal(result$metadata$coordinate_system, "utm")
  })
  
  it("devrait retourner la clé complète si return_full_key = TRUE", {
    data <- create_test_data(50)
    
    result <- anonymize_coordinates(
      data,
      password = NULL,
      return_full_key = TRUE
    )
    
    expect_true("reference_point" %in% names(result))
    expect_true("reference_latitude" %in% names(result$reference_point))
    expect_true("reference_longitude" %in% names(result$reference_point))
  })
  
  it("devrait échouer sans les colonnes requises", {
    data <- tibble::tibble(Flow = c(1, 2, 3))
    
    expect_error(
      anonymize_coordinates(data),
      "Latitude.*Longitude"
    )
  })
  
  it("devrait échouer avec un système de coordonnées invalide", {
    data <- create_test_data(50)
    
    expect_error(
      anonymize_coordinates(data, coordinate_system = "invalid"),
      "latlon.*utm"
    )
  })
})

describe("restore_coordinates", {
  it("devrait restaurer les coordonnées lat/lon correctement", {
    skip_if_not_installed("openssl")
    
    data <- create_test_data(50)
    original_lat <- data$Latitude
    original_lon <- data$Longitude
    temp_file <- tempfile(fileext = ".enc")
    
    # Anonymiser
    result <- anonymize_coordinates(
      data,
      output_key_file = temp_file,
      password = "test_password_123"
    )
    
    # Restaurer
    restored <- restore_coordinates(
      result$data,
      key_file = temp_file,
      password = "test_password_123"
    )
    
    # Vérifier que les coordonnées originales sont restaurées
    expect_equal(restored$Latitude, original_lat, tolerance = 1e-10)
    expect_equal(restored$Longitude, original_lon, tolerance = 1e-10)
    
    unlink(temp_file)
  })
  
  it("devrait restaurer les coordonnées UTM correctement", {
    skip_if_not_installed("openssl")
    
    data <- create_test_data_utm(50)
    original_x <- data$X
    original_y <- data$Y
    temp_file <- tempfile(fileext = ".enc")
    
    # Anonymiser
    result <- anonymize_coordinates(
      data,
      output_key_file = temp_file,
      password = "test_password_123",
      coordinate_system = "utm"
    )
    
    # Restaurer
    restored <- restore_coordinates(
      result$data,
      key_file = temp_file,
      password = "test_password_123"
    )
    
    # Vérifier que les coordonnées originales sont restaurées
    expect_equal(restored$X, original_x, tolerance = 1e-10)
    expect_equal(restored$Y, original_y, tolerance = 1e-10)
    
    unlink(temp_file)
  })
  
  it("devrait échouer avec un mot de passe incorrect", {
    skip_if_not_installed("openssl")
    
    data <- create_test_data(50)
    temp_file <- tempfile(fileext = ".enc")
    
    result <- anonymize_coordinates(
      data,
      output_key_file = temp_file,
      password = "correct_password"
    )
    
    expect_error(
      restore_coordinates(result$data, key_file = temp_file, password = "wrong_password"),
      "Échec du déchiffrement"
    )
    
    unlink(temp_file)
  })
  
  it("devrait échouer si le fichier de clé n'existe pas", {
    data <- create_test_data(50)
    
    expect_error(
      restore_coordinates(data, key_file = "fichier_inexistant.enc", password = "test"),
      "introuvable"
    )
  })
  
  it("devrait fonctionner avec key_info au lieu de key_file", {
    data <- create_test_data(50)
    original_lat <- data$Latitude
    
    # Anonymiser avec return_full_key
    result <- anonymize_coordinates(
      data,
      password = NULL,
      return_full_key = TRUE
    )
    
    # Restaurer avec key_info
    restored <- restore_coordinates(
      result$data,
      key_info = result$reference_point
    )
    
    expect_equal(restored$Latitude, original_lat, tolerance = 1e-10)
  })
})

describe("workflow complet", {
  it("devrait anonymiser et restaurer correctement dans un workflow complet", {
    skip_if_not_installed("openssl")
    
    # Données originales
    data <- create_test_data(100)
    original_lat <- data$Latitude
    original_lon <- data$Longitude
    temp_file <- tempfile(fileext = ".enc")
    
    # Étape 1: Anonymiser
    result <- anonymize_coordinates(
      data,
      output_key_file = temp_file,
      password = "mon_mot_de_passe_securise_123!",
      coordinate_system = "latlon"
    )
    
    # Vérifier que les données sont bien anonymisées
    expect_false(identical(result$data$Latitude, original_lat))
    
    # Étape 2: Vérifier que le fichier de clé existe
    expect_true(file.exists(temp_file))
    
    # Étape 3: Vérifier le niveau de sécurité dans les métadonnées
    expect_equal(result$metadata$algorithm, "aes-256-gcm")
    
    # Étape 4: Restaurer
    restored <- restore_coordinates(
      result$data,
      key_file = temp_file,
      password = "mon_mot_de_passe_securise_123!"
    )
    
    # Vérifier que les données sont restaurées
    expect_equal(restored$Latitude, original_lat, tolerance = 1e-10)
    expect_equal(restored$Longitude, original_lon, tolerance = 1e-10)
    
    # Nettoyer
    unlink(temp_file)
  })
})
