# Tests for helper functions in R/utils.R ----
library(testthat)
library(yieldcleanr)

test_that("validate_yield_dataframe checks for non-null and data frame", {
  expect_error(validate_yield_dataframe(NULL), "doit etre fourni")
  expect_error(validate_yield_dataframe("not_a_df"), "doit etre un data frame")
  expect_error(validate_yield_dataframe(data.frame()), "ne contient aucune ligne")
})

test_that("validate_yield_dataframe checks required columns", {
  df <- data.frame(Longitude = -73.5, Latitude = 45.5, Flow = 10)
  expect_true(validate_yield_dataframe(df, required_cols = c("Longitude", "Latitude")))
  expect_error(
    validate_yield_dataframe(df, required_cols = c("Longitude", "MissingCol")),
    "Colonne\\(s\\) requise\\(s\\) manquante\\(s\\)"
  )
})

test_that("safe_divide handles zeros and infinities", {
  expect_equal(safe_divide(10, 2), 5)
  expect_equal(safe_divide(10, 0, default = 0), 0)
  expect_true(is.na(safe_divide(10, 0, default = NA)))
  expect_equal(safe_divide(c(10, 20), c(2, 0), default = -1), c(5, -1))
})

test_that("is_valid_coordinates detects valid and invalid GPS ranges", {
  lons <- c(-73.5, -190, 180, 0)
  lats <- c(45.5, 50, 95, -90)
  expected <- c(TRUE, FALSE, FALSE, TRUE)
  expect_equal(is_valid_coordinates(lons, lats), expected)

  # Unequal lengths
  expect_error(is_valid_coordinates(c(-73.5), c(45.5, 46.5)))
})

test_that("safe_mean and safe_median handle empty and valid data", {
  expect_equal(safe_mean(c(10, 20, 30)), 20)
  expect_equal(safe_mean(numeric(0), default = 99), 99)
  expect_equal(safe_mean(c(NA, NA), na.rm = TRUE, default = 5), 5)

  expect_equal(safe_median(c(10, 20, 30)), 20)
  expect_equal(safe_median(numeric(0), default = 99), 99)
  expect_equal(safe_median(c(NA, NA), na.rm = TRUE, default = 5), 5)
})

test_that("detect_column finds columns flexibly", {
  df <- data.frame(
    Yield_kg_ha = c(1, 2),
    gps_time = c(10, 20),
    Moisture_pct = c(15, 16)
  )

  expect_equal(detect_column(df, c("yield", "yield_kg_ha")), "Yield_kg_ha")
  expect_equal(detect_column(df, c("gpstime", "time")), "gps_time")
  expect_equal(detect_column(df, c("nonexistent"), default = "fallback"), "fallback")
})

test_that("create_mock_yield_data generates expected dataset", {
  mock <- create_mock_yield_data(n = 15, with_outliers = TRUE)
  expect_s3_class(mock, "tbl_df")
  expect_equal(nrow(mock), 15)
  expect_true(all(c("Longitude", "Latitude", "Flow", "GrainType") %in% names(mock)))
  expect_true(mock$Flow[1] < 1)
  expect_true(mock$Flow[15] > 50)
})
