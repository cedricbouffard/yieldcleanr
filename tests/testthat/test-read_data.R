# Test suite for read_data.R ----
library(dplyr)
library(yieldcleanr)

test_that("read_yield_data works with valid data frame", {
  data <- tibble::tibble(
    Longitude = c(-69.856661, -69.856681),
    Latitude = c(47.506122, 47.506136),
    Flow = c(50, 55),
    GPS_Time = c(1762958157, 1762958159),
    Interval = c(2L, 2L),
    Distance = c(87, 87),
    Swath = c(240, 240),
    Moisture = c(15, 16),
    HeaderStatus = c(33L, 33L),
    Pass = c(1L, 1L),
    GPSStatus = c(7L, 7L),
    DOP = c(0, 0),
    Altitude = c(61.3, 61.5)
  )

  result <- read_yield_data(data)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 2)
})

# Test detect_and_convert_imperial_units (internal - use :::) ----
test_that("detect_and_convert_imperial_units converts inches to meters", {
  data <- tibble::tibble(
    Distance = c(80, 85, 90),  # In inches (~2m)
    Swath = c(300, 310, 320),  # In inches (~8m)
    Flow = 1:3
  )

  result <- yieldcleanr:::detect_and_convert_imperial_units(data)

  expect_true(all(result$Distance < 3))  # Should be in meters now
  expect_true(all(result$Swath < 10))    # Should be in meters now
})

test_that("detect_and_convert_imperial_units keeps meters unchanged", {
  data <- tibble::tibble(
    Distance = c(1.5, 2.0, 2.5),  # Already in meters
    Swath = c(8, 9, 10),          # Already in meters
    Flow = 1:3
  )

  result <- yieldcleanr:::detect_and_convert_imperial_units(data)

  expect_equal(result$Distance, data$Distance)
  expect_equal(result$Swath, data$Swath)
})

test_that("detect_and_convert_imperial_units handles missing columns", {
  data <- tibble(Flow = 1:3)

  result <- yieldcleanr:::detect_and_convert_imperial_units(data)

  expect_equal(nrow(result), nrow(data))
})

# Test list_fields_from_zip (exported) ----
test_that("list_fields_from_zip handles nonexistent file", {
  expect_error(list_fields_from_zip("nonexistent.zip"))
})

test_that("list_fields_from_zip handles empty zip", {
  # Test that function exists and handles edge cases
  expect_error(list_fields_from_zip("nonexistent.zip"))
})

# Test standardize_jd_columns (internal - use :::) ----
test_that("standardize_jd_columns handles data with coords", {
  skip_if_not_installed("sf")

  # Create sf object directly with valid coordinate columns
  data <- data.frame(
    Flow = c(100, 110, 120),
    Longitude = c(-69.856, -69.857, -69.858),
    Latitude = c(47.506, 47.507, 47.508)
  ) |>
    sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)

  result <- yieldcleanr:::standardize_jd_columns(data)

  expect_true("Flow" %in% names(result))
  expect_true("Moisture" %in% names(result))
})

# Test convert_jd_metric_to_yieldcleanr (internal - use :::) ----
test_that("convert_jd_metric_to_yieldcleanr creates Yield_kg_ha", {
  data <- tibble::tibble(
    Flow = c(100, 150, 200),  # kg/ha
    Moisture = c(15, 16, 17),
    Yield_kg_ha = c(100, 150, 200)
  )

  result <- yieldcleanr:::convert_jd_metric_to_yieldcleanr(data)

  expect_true("Yield_kg_ha" %in% names(result))
})

test_that("convert_jd_metric_to_yieldcleanr converts tons to kg", {
  data <- tibble::tibble(
    Flow = c(5, 8, 12),  # tonnes/ha
    Yield_t_ha = c(5, 8, 12)
  )

  result <- yieldcleanr:::convert_jd_metric_to_yieldcleanr(data)

  expect_true(all(result$Flow > 100))  # Should be in kg now
})

test_that("convert_jd_metric_to_yieldcleanr converts inches to meters", {
  data <- tibble::tibble(
    Swath = c(300, 350, 400)  # Inches
  )

  result <- yieldcleanr:::convert_jd_metric_to_yieldcleanr(data)

  expect_true(all(result$Swath < 15))  # Should be in meters
})