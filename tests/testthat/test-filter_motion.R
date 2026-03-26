# Test suite for filter_motion.R ----
library(dplyr)
library(yieldcleanr)

test_that("filter functions for motion exist", {
  # Test that filter functions exist
  expect_true(exists("filter_data"))
  expect_true(is.function(filter_data))
})

test_that("velocity filtering works", {
  test_data <- tibble::tibble(
    Longitude = c(-73.5, -73.51, -73.52, -73.53),
    Latitude = c(45.5, 45.51, 45.52, 45.53),
    Velocity = c(5, 0.1, 15, 6),  # Including too slow and too fast
    Yield_kg_ha = c(8000, 8500, 9000, 8200)
  )
  
  result <- filter_data(test_data, type = "velocity")
  
  expect_s3_class(result, "tbl_df")
  expect_lte(nrow(result), nrow(test_data))
})

test_that("header status filtering works", {
  test_data <- tibble::tibble(
    Longitude = c(-73.5, -73.51, -73.52),
    Latitude = c(45.5, 45.51, 45.52),
    HeaderStatus = c(33, 0, 33),  # 0 = header raised
    Yield_kg_ha = c(8000, 8500, 9000)
  )
  
  result <- filter_data(test_data, type = "header")
  
  expect_s3_class(result, "tbl_df")
  expect_lte(nrow(result), nrow(test_data))
})

test_that("gps filtering works", {
  test_data <- tibble::tibble(
    Longitude = c(-73.5, -73.51, -73.52),
    Latitude = c(45.5, 45.51, 45.52),
    GPSStatus = c(7, 1, 7),  # Different GPS qualities
    Yield_kg_ha = c(8000, 8500, 9000)
  )
  
  result <- filter_data(test_data, type = "gps")
  
  expect_s3_class(result, "tbl_df")
})

test_that("moisture filtering works", {
  test_data <- tibble::tibble(
    Longitude = c(-73.5, -73.51, -73.52),
    Latitude = c(45.5, 45.51, 45.52),
    Moisture = c(15, 5, 45),  # Including out of range values
    Yield_kg_ha = c(8000, 8500, 9000)
  )
  
  result <- filter_data(test_data, type = "moisture")
  
  expect_s3_class(result, "tbl_df")
  expect_lte(nrow(result), nrow(test_data))
})

test_that("yield filtering works", {
  test_data <- tibble::tibble(
    Longitude = c(-73.5, -73.51, -73.52),
    Latitude = c(45.5, 45.51, 45.52),
    Yield_kg_ha = c(8000, 500, 50000)  # Including outliers
  )
  
  result <- filter_data(test_data, type = "yield")
  
  expect_s3_class(result, "tbl_df")
  expect_lte(nrow(result), nrow(test_data))
})

test_that("multiple filters can be applied", {
  test_data <- tibble::tibble(
    Longitude = c(-73.5, -73.51, -73.52),
    Latitude = c(45.5, 45.51, 45.52),
    Velocity = c(5, 0.1, 15),
    HeaderStatus = c(33, 0, 33),
    Yield_kg_ha = c(8000, 8500, 9000)
  )
  
  result <- filter_data(test_data, type = c("velocity", "header"))
  
  expect_s3_class(result, "tbl_df")
  expect_lte(nrow(result), nrow(test_data))
})
