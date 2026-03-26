# Test suite for ayce_filters.R ----
library(dplyr)
library(yieldcleanr)

test_that("apply_delay_adjustment exists", {
  expect_true(exists("apply_delay_adjustment"))
  expect_true(is.function(apply_delay_adjustment))
})

test_that("detect_anomalies works", {
  test_data <- tibble::tibble(
    Longitude = c(-73.5, -73.51, -73.52, -73.53),
    Latitude = c(45.5, 45.51, 45.52, 45.53),
    Yield_kg_ha = c(8000, 8500, 9000, 8200),
    Velocity = c(5, 5.5, 6, 5.2)
  )
  
  result <- detect_anomalies(test_data, type = "velocity_jump")
  
  expect_s3_class(result, "tbl_df")
  expect_true(nrow(result) > 0)
})

test_that("detect_anomalies with overlap detection", {
  test_data <- tibble::tibble(
    Longitude = c(-73.5, -73.51, -73.52, -73.5001),
    Latitude = c(45.5, 45.51, 45.52, 45.5001),
    Yield_kg_ha = c(8000, 8500, 9000, 8200),
    Velocity = c(5, 5.5, 6, 5.2),
    Pass = c(1, 1, 2, 2)
  )
  
  result <- detect_anomalies(test_data, type = "overlap", action = "detect")
  
  expect_s3_class(result, "tbl_df")
})

test_that("detect_anomalies with local_sd detection", {
  test_data <- tibble::tibble(
    Longitude = c(-73.5, -73.51, -73.52, -73.53),
    Latitude = c(45.5, 45.51, 45.52, 45.53),
    Yield_kg_ha = c(8000, 8500, 9000, 15000),  # One outlier
    Velocity = c(5, 5.5, 6, 5.2),
    Pass = c(1, 1, 1, 1)
  )
  
  result <- detect_anomalies(test_data, type = "local_sd", action = "detect")
  
  expect_s3_class(result, "tbl_df")
})
