library(testthat)
library(dplyr)
library(yieldcleanr)

# Test suite for ayce_clean.R and ayce_pcdi.R ----

# Helper function to create test data
create_test_data <- function(n = 50) {
  set.seed(42)
  tibble::tibble(
    .row_id = 1:n,
    Longitude = -69.856661 + seq(0, length.out = n, by = 0.0001),
    Latitude = 47.506122 + seq(0, length.out = n, by = 0.00005),
    Flow = c(rnorm(floor(n/2), mean = 5, sd = 1), rnorm(ceiling(n/2), mean = 100, sd = 10)),
    GPS_Time = as.integer(1762958157 + seq(0, length.out = n, by = 2)),
    Interval = rep(2L, n),
    Distance = rep(87L, n),
    Swath = rep(240L, n),
    Moisture = rep(30.8, n),
    HeaderStatus = rep(33L, n),
    Pass = rep(1L, n),
    Serial = rep(2410019049L, n),
    FieldID = rep("F0:1", n),
    LoadID = rep("L0:<1>", n),
    GrainType = rep("Maïs", n),
    GPSStatus = rep(7L, n),
    DOP = rep(0, n),
    Altitude = rep(61.3, n)
  )
}

test_that("apply_moisture_delay shifts moisture values", {
  data <- create_test_data(10) |>
    mutate(Moisture = 1:10)

  result <- apply_moisture_delay(data, delay = 1)

  expect_equal(nrow(result), nrow(data) - 1)
})

test_that("apply_moisture_delay with zero delay returns original", {
  data <- create_test_data(5) |>
    mutate(Moisture_raw = Moisture)

  result <- apply_moisture_delay(data, delay = 0)

  expect_equal(nrow(result), nrow(data))
})

test_that("calculate_auto_ranges returns list with quantiles", {
  data <- create_test_data(50) |>
    mutate(
      X = 435000 + seq(0, length.out = 50, by = 10),
      Y = 5262000 + seq(0, length.out = 50, by = 10)
    )

  ranges <- calculate_auto_ranges(data)

  expect_type(ranges, "list")
  expect_true("yield" %in% names(ranges))
  expect_true("easting" %in% names(ranges))
  expect_true("northing" %in% names(ranges))
  expect_true("velocity" %in% names(ranges))
})

test_that("calculate_auto_ranges handles missing columns gracefully", {
  data <- create_test_data(5)
  data$X <- NULL
  data$Y <- NULL

  ranges <- calculate_auto_ranges(data)

  expect_type(ranges, "list")
  expect_true("yield" %in% names(ranges))
})

test_that("calculate_auto_ranges returns list with quantiles", {
  data <- create_test_data(50) |>
    mutate(
      X = 435000 + seq(0, length.out = 50, by = 10),
      Y = 5262000 + seq(0, length.out = 50, by = 10)
    )

  ranges <- calculate_auto_ranges(data)

  expect_type(ranges, "list")
  expect_true("yield" %in% names(ranges))
  expect_true("easting" %in% names(ranges))
  expect_true("northing" %in% names(ranges))
  expect_true("velocity" %in% names(ranges))
})

test_that("ayce_validate returns validation metrics", {
  data_raw <- create_test_data(100)
  data_clean <- create_test_data(80) |>
    mutate(Flow = Flow * 0.8)  # Reduced variance

  validation <- ayce_validate(data_clean, data_raw)

  expect_type(validation, "list")
  expect_true("retention_rate" %in% names(validation))
  expect_true(validation$retention_rate > 0)
  expect_true(validation$retention_rate <= 1)
})

test_that("ayce_validate warns on low retention", {
  data_raw <- create_test_data(100)
  data_clean <- create_test_data(10)  # Only 10% retained

  validation <- ayce_validate(data_clean, data_raw)

  expect_true(validation$retention_rate < 0.5)
  expect_false(is.null(validation$warning))
})

test_that("ayce_validate warns on no points removed", {
  data_raw <- create_test_data(100)
  data_clean <- data_raw  # 100% retained

  validation <- ayce_validate(data_clean, data_raw)

  expect_true(validation$retention_rate > 0.99)
  expect_false(is.null(validation$warning))
})

# Test calculate_auto_thresholds
test_that("calculate_auto_thresholds calculates thresholds", {
  skip_if_not_installed("sf")

  data <- create_test_data(50) |>
    mutate(
      X = 435000 + seq(0, length.out = 50, by = 10),
      Y = 5262000 + seq(0, length.out = 50, by = 10)
    )

  thresholds <- calculate_auto_thresholds(data)

  expect_type(thresholds, "list")
  expect_true("min_yield" %in% names(thresholds))
  expect_true("max_yield" %in% names(thresholds))
  expect_true("min_velocity" %in% names(thresholds))
  expect_true("max_velocity" %in% names(thresholds))
})

test_that("calculate_auto_thresholds handles edge cases", {
  data <- create_test_data(5)

  thresholds <- calculate_auto_thresholds(data)

  expect_type(thresholds, "list")
  expect_true(thresholds$min_yield <= thresholds$max_yield)
})
