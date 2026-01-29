library(testthat)
library(dplyr)
library(yieldcleanr)

# Test suite for coordinate_conversion.R ----

test_that("calculate_distances computes inter-point distances", {
  data <- tibble::tibble(
    X = c(0, 10, 25),
    Y = c(0, 0, 15),
    .row_id = 1:3
  )

  result <- yieldcleanr:::calculate_distances(data)

  expect_true("distance_calc" %in% names(result))
  expect_equal(result$distance_calc[1], 0)  # First point has no previous
  expect_equal(result$distance_calc[2], 10)  # Distance from (0,0) to (10,0)
  expect_equal(result$distance_calc[3], sqrt(15^2 + 15^2))  # Diagonal
})

test_that("calculate_distances requires X and Y columns", {
  data <- tibble(Flow = c(1, 2, 3))

  expect_error(yieldcleanr:::calculate_distances(data))
})

test_that("utm_to_latlon converts to geographic coordinates", {
  # Create data with X, Y columns in UTM zone 19N
  data <- data.frame(
    X = c(435000, 435050),
    Y = c(5262000, 5262050)
  )

  result <- yieldcleanr:::utm_to_latlon(data, zone = 19, hemisphere = "N")

  expect_true("Longitude" %in% names(result))
  expect_true("Latitude" %in% names(result))
})

test_that("utm_to_latlon handles southern hemisphere", {
  data <- data.frame(
    X = c(500000, 500050),
    Y = c(8000000, 8000050)  # Southern hemisphere
  )

  result <- yieldcleanr:::utm_to_latlon(data, zone = 32, hemisphere = "S")

  expect_true("Longitude" %in% names(result))
  expect_true("Latitude" %in% names(result))
  expect_true(all(result$Latitude < 0))
})

test_that("utm_to_latlon requires X and Y columns", {
  data <- data.frame(Flow = c(1, 2))

  expect_error(yieldcleanr:::utm_to_latlon(data, zone = 19))
})

test_that("latlon_to_utm converts to UTM coordinates", {
  data <- tibble::tibble(
    Latitude = c(47.506122, 47.506136),
    Longitude = c(-69.856661, -69.856681)
  )

  result <- latlon_to_utm(data)

  expect_true("X" %in% names(result))
  expect_true("Y" %in% names(result))
  expect_true(all(result$X > 0))
  expect_true(all(result$Y > 0))
})

test_that("latlon_to_utm auto-detects UTM zone", {
  data <- tibble::tibble(
    Latitude = c(47.506122, 47.506136),
    Longitude = c(-69.856661, -69.856681)
  )

  # Should auto-detect zone 19 for these coordinates
  result <- latlon_to_utm(data)

  # X should be in range for UTM zone 19N (easting: 0-1000000)
  expect_true(all(result$X >= 0 & result$X <= 1000000))
  # Y should be in range for northern hemisphere (northing: 0-10000000)
  expect_true(all(result$Y >= 0 & result$Y <= 10000000))
})

test_that("latlon_to_utm uses specified zone", {
  data <- tibble::tibble(
    Latitude = c(47.5, 47.6),
    Longitude = c(-69.8, -69.7)
  )

  # Force zone 18
  result <- latlon_to_utm(data, zone = 18)

  expect_true(all(result$X >= 0))
  expect_true(all(result$Y >= 0))
})

test_that("latlon_to_utm requires Latitude and Longitude columns", {
  data <- tibble(Flow = c(1, 2))

  expect_error(latlon_to_utm(data))
})