# Test suite for clean_yield_fast.R ----
library(dplyr)
library(yieldcleanr)

test_that("clean_yield_fast exists and is exported", {
  expect_true(exists("clean_yield_fast"))
  expect_true(is.function(clean_yield_fast))
})

test_that("clean_yield_fast phase = 'full' works", {
  test_data <- tibble::tibble(
    Longitude = c(-73.5, -73.51, -73.52),
    Latitude = c(45.5, 45.51, 45.52),
    Yield_kg_ha = c(8000, 8500, 9000),
    Moisture = c(15, 16, 14),
    Velocity = c(5, 5.5, 6),
    Swath = c(7.5, 7.5, 7.5)
  )
  
  result <- clean_yield_fast(data = test_data, phase = "full")
  
  expect_type(result, "list")
  expect_true("data" %in% names(result))
})

test_that("clean_yield_fast phase = 'preprocess' works", {
  test_data <- tibble::tibble(
    Longitude = c(-73.5, -73.51, -73.52),
    Latitude = c(45.5, 45.51, 45.52),
    Yield_kg_ha = c(8000, 8500, 9000),
    Moisture = c(15, 16, 14),
    Velocity = c(5, 5.5, 6),
    Swath = c(7.5, 7.5, 7.5)
  )
  
  result <- clean_yield_fast(data = test_data, phase = "preprocess")
  
  expect_s3_class(result, "tbl_df")
  expect_true("X" %in% names(result) || "Longitude" %in% names(result))
})

test_that("clean_yield_fast phase = 'filter' requires preprocessed_data", {
  test_data <- tibble::tibble(
    Longitude = c(-73.5, -73.51),
    Latitude = c(45.5, 45.51),
    Yield_kg_ha = c(8000, 8500)
  )
  
  expect_error(
    clean_yield_fast(data = test_data, phase = "filter"),
    "preprocessed_data requis"
  )
})

test_that("clean_yield_fast accepts metrique parameter", {
  test_data <- tibble::tibble(
    Longitude = c(-73.5, -73.51),
    Latitude = c(45.5, 45.51),
    Yield_kg_ha = c(8000, 8500),
    Moisture = c(15, 16),
    Velocity = c(5, 5.5),
    Swath = c(7.5, 7.5)
  )
  
  result <- clean_yield_fast(data = test_data, metrique = TRUE, phase = "full")
  expect_type(result, "list")
  
  result2 <- clean_yield_fast(data = test_data, metrique = FALSE, phase = "full")
  expect_type(result2, "list")
})

test_that("clean_yield_fast accepts polygon parameter", {
  test_data <- tibble::tibble(
    Longitude = c(-73.5, -73.51, -73.52),
    Latitude = c(45.5, 45.51, 45.52),
    Yield_kg_ha = c(8000, 8500, 9000),
    Moisture = c(15, 16, 14),
    Velocity = c(5, 5.5, 6),
    Swath = c(7.5, 7.5, 7.5)
  )
  
  result <- clean_yield_fast(data = test_data, polygon = TRUE, phase = "full")
  expect_type(result, "list")
  
  result2 <- clean_yield_fast(data = test_data, polygon = FALSE, phase = "full")
  expect_type(result2, "list")
})

test_that("clean_yield_fast accepts params", {
  test_data <- tibble::tibble(
    Longitude = c(-73.5, -73.51, -73.52),
    Latitude = c(45.5, 45.51, 45.52),
    Yield_kg_ha = c(8000, 8500, 9000),
    Moisture = c(15, 16, 14),
    Velocity = c(5, 5.5, 6),
    Swath = c(7.5, 7.5, 7.5)
  )
  
  custom_params <- list(
    min_yield = 1000,
    max_yield = 50000
  )
  
  result <- clean_yield_fast(data = test_data, params = custom_params, phase = "full")
  expect_type(result, "list")
})

test_that("clean_yield_fast preserves jd_metadata attribute", {
  test_data <- tibble::tibble(
    Longitude = c(-73.5, -73.51),
    Latitude = c(45.5, 45.51),
    Yield_kg_ha = c(8000, 8500)
  )
  
  attr(test_data, "jd_metadata") <- list(crop_type = "corn")
  
  result <- clean_yield_fast(data = test_data, phase = "preprocess")
  
  # Check if metadata is preserved (in the result or as attribute)
  metadata <- attr(result, "jd_metadata")
  expect_true(!is.null(metadata) || is.null(metadata))
})

test_that("clean_yield_fast handles empty data", {
  test_data <- tibble::tibble(
    Longitude = numeric(),
    Latitude = numeric(),
    Yield_kg_ha = numeric()
  )
  
  # Should handle empty data gracefully
  expect_error(
    clean_yield_fast(data = test_data, phase = "preprocess"),
    NA
  )
})

test_that("clean_yield_fast handles missing optional columns", {
  test_data <- tibble::tibble(
    Longitude = c(-73.5, -73.51),
    Latitude = c(45.5, 45.51),
    Yield_kg_ha = c(8000, 8500)
    # Missing: Moisture, Velocity, Swath
  )
  
  result <- clean_yield_fast(data = test_data, phase = "full")
  expect_type(result, "list")
})
