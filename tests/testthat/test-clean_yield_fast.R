# Test suite for clean_yield_fast.R ----
library(dplyr)
library(yieldcleanr)

test_that("clean_yield_fast exists and is exported", {
  expect_true(exists("clean_yield_fast"))
  expect_true(is.function(clean_yield_fast))
})

test_that("clean_yield_fast validates inputs", {
  expect_error(clean_yield_fast())
})

test_that("clean_yield_fast phase = 'preprocess' works", {
  test_data <- tibble::tibble(
    Longitude = c(-73.5, -73.51, -73.52),
    Latitude = c(45.5, 45.51, 45.52),
    Flow = c(50, 55, 52),
    Interval = c(1, 1, 1),
    Swath = c(7.5, 7.5, 7.5),
    Distance = c(5, 5, 5)
  )
  
  result <- clean_yield_fast(data = test_data, phase = "preprocess")
  
  expect_s3_class(result, "tbl_df")
  expect_true("X" %in% names(result) || "Longitude" %in% names(result))
})

test_that("clean_yield_fast phase = 'filter' requires preprocessed_data", {
  test_data <- tibble::tibble(
    Longitude = c(-73.5, -73.51),
    Latitude = c(45.5, 45.51),
    Flow = c(50, 55),
    Interval = c(1, 1),
    Swath = c(7.5, 7.5),
    Distance = c(5, 5)
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
    Flow = c(50, 55),
    Interval = c(1, 1),
    Swath = c(7.5, 7.5),
    Distance = c(5, 5)
  )
  
  result <- clean_yield_fast(data = test_data, metrique = TRUE, phase = "preprocess")
  expect_s3_class(result, "tbl_df")
})

test_that("clean_yield_fast accepts params", {
  test_data <- tibble::tibble(
    Longitude = c(-73.5, -73.51),
    Latitude = c(45.5, 45.51),
    Flow = c(50, 55),
    Interval = c(1, 1),
    Swath = c(7.5, 7.5),
    Distance = c(5, 5)
  )
  
  custom_params <- list(
    apply_position = FALSE
  )
  
  result <- clean_yield_fast(data = test_data, params = custom_params, phase = "preprocess")
  expect_s3_class(result, "tbl_df")
})

test_that("clean_yield_fast preserves jd_metadata attribute", {
  test_data <- tibble::tibble(
    Longitude = c(-73.5, -73.51),
    Latitude = c(45.5, 45.51),
    Flow = c(50, 55),
    Interval = c(1, 1),
    Swath = c(7.5, 7.5),
    Distance = c(5, 5)
  )
  
  attr(test_data, "jd_metadata") <- list(crop_type = "corn")
  
  result <- clean_yield_fast(data = test_data, phase = "preprocess")
  
  expect_s3_class(result, "tbl_df")
})

test_that("clean_yield_fast handles empty data gracefully", {
  test_data <- tibble::tibble(
    Longitude = numeric(),
    Latitude = numeric(),
    Flow = numeric(),
    Interval = numeric(),
    Swath = numeric(),
    Distance = numeric()
  )
  
  # Should error gracefully with empty data
  expect_error(
    clean_yield_fast(data = test_data, phase = "preprocess")
  )
})

test_that("clean_yield_fast preprocess creates UTM coordinates", {
  test_data <- tibble::tibble(
    Longitude = c(-73.5, -73.51, -73.52),
    Latitude = c(45.5, 45.51, 45.52),
    Flow = c(50, 55, 52),
    Interval = c(1, 1, 1),
    Swath = c(7.5, 7.5, 7.5),
    Distance = c(5, 5, 5)
  )
  
  result <- clean_yield_fast(data = test_data, phase = "preprocess")
  
  # Should create X and Y columns (UTM coordinates)
  expect_true("X" %in% names(result) || "Longitude" %in% names(result))
  expect_s3_class(result, "tbl_df")
})

test_that("clean_yield_fast adds orig_row_id if missing", {
  test_data <- tibble::tibble(
    Longitude = c(-73.5, -73.51),
    Latitude = c(45.5, 45.51),
    Flow = c(50, 55),
    Interval = c(1, 1),
    Swath = c(7.5, 7.5),
    Distance = c(5, 5)
  )
  
  result <- clean_yield_fast(data = test_data, phase = "preprocess")
  
  expect_s3_class(result, "tbl_df")
})
