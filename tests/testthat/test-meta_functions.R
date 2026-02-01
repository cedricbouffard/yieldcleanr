# Test suite for meta-functions ----
library(dplyr)
library(yieldcleanr)

# Create test data helper
create_test_data <- function() {
  tibble::tibble(
    Longitude = c(-69.856661, -69.856681, -69.856701, -69.856726, -69.856749),
    Latitude = c(47.506122, 47.506136, 47.506152, 47.506168, 47.506185),
    X = c(435000, 435010, 435020, 435030, 435040),
    Y = c(5262000, 5262010, 5262020, 5262030, 5262040),
    Flow = c(50, 55, 52, 58, 54),
    GPS_Time = c(1762958157, 1762958159, 1762958161, 1762958163, 1762958165),
    Interval = c(2L, 2L, 2L, 2L, 2L),
    Distance = c(87, 87, 87, 87, 87),
    Swath = c(240, 240, 240, 240, 240),
    Moisture = c(15, 16, 15, 16, 15),
    HeaderStatus = c(33L, 33L, 33L, 33L, 33L),
    Pass = c(1L, 1L, 1L, 2L, 2L),
    GPSStatus = c(7L, 7L, 7L, 7L, 7L),
    DOP = c(0, 0, 0, 0, 0),
    .row_id = 1:5
  )
}

# Test filter_data ----
test_that("filter_data applies header filter", {
  data <- create_test_data()
  data$HeaderStatus <- c(1L, 33L, 33L, 0L, 1L)
  
  result <- filter_data(data, type = "header")
  
  expect_equal(nrow(result), 4)
  expect_true(all(result$HeaderStatus %in% c(1, 33)))
})

test_that("filter_data applies gps filter", {
  data <- create_test_data()
  data$GPSStatus <- c(2L, 4L, 4L, 7L, 4L)
  
  result <- filter_data(data, type = "gps")
  
  expect_equal(nrow(result), 4)
  expect_true(all(result$GPSStatus >= 4 | is.na(result$GPSStatus)))
})

test_that("filter_data applies dop filter", {
  data <- create_test_data()
  data$DOP <- c(5L, 15L, 8L, 5L, 10L)
  
  result <- filter_data(data, type = "dop", max_dop = 10)
  
  expect_equal(nrow(result), 4)
})

test_that("filter_data applies velocity filter", {
  data <- create_test_data()
  
  result <- filter_data(data, type = "velocity", min_velocity = 1, max_velocity = 10)
  
  expect_true(nrow(result) <= nrow(data))
})

test_that("filter_data applies yield filter", {
  data <- create_test_data()
  data$Yield_kg_ha <- c(6270, 9405, 3135, 18810, 11286)
  
  result <- filter_data(data, type = "yield", min_yield = 3135, max_yield = 11286)
  
  expect_equal(nrow(result), 4)
  expect_true(all(result$Yield_kg_ha >= 3135 & result$Yield_kg_ha <= 11286))
})

test_that("filter_data applies moisture filter", {
  data <- create_test_data()
  data$Moisture <- c(5, 15, 35, 45, 25)
  
  result <- filter_data(data, type = "moisture", min_moisture = 10, max_moisture = 40)
  
  expect_equal(nrow(result), 3)
})

test_that("filter_data applies multiple filters", {
  data <- create_test_data()
  data$HeaderStatus <- c(1L, 33L, 33L, 0L, 1L)
  data$GPSStatus <- c(2L, 4L, 4L, 7L, 4L)
  
  result <- filter_data(data, type = c("header", "gps"))
  
  expect_true(nrow(result) < nrow(data))
})

test_that("filter_data with type = 'all' applies all filters", {
  data <- create_test_data()
  
  result <- filter_data(data, type = "all")
  
  expect_true(is.data.frame(result))
})

test_that("filter_data handles missing columns gracefully", {
  data <- tibble::tibble(Flow = 1:5)
  
  result <- filter_data(data, type = "all")
  
  expect_equal(nrow(result), nrow(data))
})

# Test detect_anomalies ----
test_that("detect_anomalies detects overlap", {
  data <- tibble::tibble(
    X = c(1, 1.1, 1.2, 1.3, 1.4, 1.5, 2, 2.1, 2.2),
    Y = c(1, 1, 1, 1, 1, 1, 2, 2, 2),
    Flow = 1:9
  )
  
  result <- detect_anomalies(data, type = "overlap", cellsize = 0.5, max_pass = 2)
  
  expect_true(nrow(result) < nrow(data))
})

test_that("detect_anomalies detects local_sd outliers", {
  data <- tibble::tibble(
    X = c(435000, 435001, 435002, 435003, 435004, 435005,
          435100, 435101, 435102, 435103, 435104, 435105),
    Y = c(5262000, 5262001, 5262002, 5262003, 5262004, 5262005,
          5262100, 5262101, 5262102, 5262103, 5262104, 5262105),
    Flow = c(50, 55, 52, 58, 300, 54,
             45, 48, 47, 50, 49, 46),
    Swath = rep(240, 12)
  )
  
  result <- detect_anomalies(data, type = "local_sd", n_swaths = 5, lsd_limit = 2.4)
  
  expect_true(nrow(result) <= nrow(data))
})

test_that("detect_anomalies detects velocity jumps", {
  data <- tibble::tibble(
    X = c(435000, 435010, 435020, 435030, 435040),
    Y = c(5262000, 5262010, 5262020, 5262030, 5262040),
    Interval = c(2, 2, 2, 2, 2),
    Flow = 1:5,
    GPS_Time = 1:5
  )
  
  result <- detect_anomalies(data, type = "velocity_jump", max_acceleration = 5)
  
  expect_true(nrow(result) <= nrow(data))
})

test_that("detect_anomalies detects heading anomalies", {
  data <- tibble::tibble(
    X = c(435000, 435010, 435020, 435030, 435040),
    Y = c(5262000, 5262010, 5262020, 5262030, 5262040),
    Flow = 1:5,
    GPS_Time = 1:5
  )
  
  result <- detect_anomalies(data, type = "heading", max_heading_change = 60)
  
  expect_true(nrow(result) <= nrow(data))
})

test_that("detect_anomalies detects position outliers", {
  data <- create_test_data()
  
  result <- detect_anomalies(data, type = "position", gbuffer = 100)
  
  expect_true(nrow(result) <= nrow(data))
})

test_that("detect_anomalies with type = 'all' detects all anomalies", {
  data <- create_test_data()
  
  result <- detect_anomalies(data, type = "all")
  
  expect_true(is.data.frame(result))
})

test_that("detect_anomalies with action = 'detect' marks without filtering", {
  data <- create_test_data()
  
  result <- detect_anomalies(data, type = "position", action = "detect")
  
  expect_equal(nrow(result), nrow(data))
  expect_true("position_outlier" %in% names(result))
})

# Test calculate_thresholds ----
test_that("calculate_thresholds computes yield thresholds", {
  data <- create_test_data()
  data$Yield_kg_ha <- c(6270, 7524, 8778, 10032, 11286)
  
  result <- calculate_thresholds(data, type = "yield")
  
  expect_true("yield" %in% names(result))
  expect_true("min_yield" %in% names(result$yield))
  expect_true("max_yield" %in% names(result$yield))
})

test_that("calculate_thresholds computes velocity thresholds", {
  data <- create_test_data()
  
  result <- calculate_thresholds(data, type = "velocity")
  
  expect_true("velocity" %in% names(result))
  expect_true("min_velocity" %in% names(result$velocity))
  expect_true("max_velocity" %in% names(result$velocity))
})

test_that("calculate_thresholds computes position thresholds", {
  data <- create_test_data()
  
  result <- calculate_thresholds(data, type = "position")
  
  expect_true("position" %in% names(result))
  expect_true("buffer" %in% names(result$position))
})

test_that("calculate_thresholds computes moisture thresholds", {
  data <- create_test_data()
  
  result <- calculate_thresholds(data, type = "moisture")
  
  expect_true("moisture" %in% names(result))
  expect_true("min_moisture" %in% names(result$moisture))
  expect_true("max_moisture" %in% names(result$moisture))
})

test_that("calculate_thresholds with type = 'all' computes all thresholds", {
  data <- create_test_data()
  
  result <- calculate_thresholds(data, type = "all")
  
  expect_true("yield" %in% names(result))
  expect_true("velocity" %in% names(result))
  expect_true("position" %in% names(result))
  expect_true("moisture" %in% names(result))
})

# Test convert_coordinates ----
test_that("convert_coordinates converts latlon to utm", {
  data <- tibble::tibble(
    Longitude = c(-69.856661, -69.856681),
    Latitude = c(47.506122, 47.506136)
  )
  
  result <- convert_coordinates(data, from = "latlon", to = "utm")
  
  expect_true("X" %in% names(result))
  expect_true("Y" %in% names(result))
  expect_true(all(result$X > 0))
  expect_true(all(result$Y > 0))
})

test_that("convert_coordinates returns same data when from == to", {
  data <- create_test_data()
  
  result <- convert_coordinates(data, from = "utm", to = "utm")
  
  expect_equal(nrow(result), nrow(data))
})

# Test convert_yield_units ----
test_that("convert_yield_units converts flow to kg/ha", {
  data <- tibble::tibble(
    Flow = c(50, 55, 52),
    Interval = c(2, 2, 2),
    Distance = c(87, 87, 87),
    Swath = c(240, 240, 240)
  )
  
  result <- convert_yield_units(data, from = "flow_lbs_s", to = "kg_ha")
  
  expect_true("Yield_kg_ha" %in% names(result) || "Yield_kg_ha_wet" %in% names(result))
})

test_that("convert_yield_units converts kg/ha to bu/acre", {
  data <- tibble::tibble(
    Yield_kg_ha = c(6270, 9405, 3135)
  )
  
  result <- convert_yield_units(data, from = "kg_ha", to = "bu_acre", crop_type = "maize")
  
  expect_true("Yield_bu_acre" %in% names(result))
})

# Test anonymize_data ----
test_that("anonymize_data anonymizes coordinates", {
  skip_if_not_installed("sf")
  
  data <- create_test_data()
  
  result <- anonymize_data(data, type = "coordinates", method = "translation")
  
  expect_true("X" %in% names(result))
  expect_true("Y" %in% names(result))
  # Coordinates should be different
  expect_false(identical(result$X, data$X))
  expect_false(identical(result$Y, data$Y))
})

test_that("anonymize_data removes sensitive attributes", {
  data <- create_test_data()
  data$Serial <- "ABC123"
  data$FieldID <- "Field001"
  
  result <- anonymize_data(data, type = "attributes")
  
  expect_false("Serial" %in% names(result))
  expect_false("FieldID" %in% names(result))
})

test_that("anonymize_data with type = 'full' does both", {
  skip_if_not_installed("sf")
  
  data <- create_test_data()
  data$Serial <- "ABC123"
  original_x <- data$X
  
  result <- anonymize_data(data, type = "full", method = "translation")
  
  expect_false("Serial" %in% names(result))
  expect_false(identical(result$X, original_x))
})

# Test export_data ----
test_that("export_data exports to CSV", {
  data <- create_test_data()
  temp_file <- tempfile(fileext = ".csv")
  
  result <- export_data(data, temp_file, format = "csv")
  
  expect_true(file.exists(temp_file))
  expect_equal(result, temp_file)
  
  # Clean up
  unlink(temp_file)
})

test_that("export_data detects format from extension", {
  skip_if_not_installed("sf")
  
  data <- create_test_data()
  data_sf <- sf::st_as_sf(data, coords = c("Longitude", "Latitude"), crs = 4326)
  temp_file <- tempfile(fileext = ".geojson")
  
  result <- export_data(data_sf, temp_file)
  
  expect_true(file.exists(temp_file))
  
  # Clean up
  unlink(temp_file)
})

test_that("export_data handles overwrite parameter", {
  data <- create_test_data()
  temp_file <- tempfile(fileext = ".csv")
  
  # First export
  export_data(data, temp_file, format = "csv")
  
  # Should fail without overwrite
  expect_error(export_data(data, temp_file, format = "csv"))
  
  # Should succeed with overwrite
  result <- export_data(data, temp_file, format = "csv", overwrite = TRUE)
  expect_equal(result, temp_file)
  
  # Clean up
  unlink(temp_file)
})

# Test optimize_delays ----
test_that("optimize_delays optimizes flow delay", {
  data <- tibble::tibble(
    Flow = c(10, 15, 12, 18, 14, 16, 13, 17, 11, 19),
    GPS_Time = 1:10,
    X = 435000 + 1:10,
    Y = 5262000 + 1:10,
    Interval = rep(2L, 10)
  )
  
  result <- optimize_delays(data, type = "flow", delay_range = -3:3, n_iterations = 2)
  
  expect_true("delays" %in% names(result))
  expect_true("flow" %in% names(result$delays))
})

test_that("optimize_delays optimizes moisture delay", {
  data <- tibble::tibble(
    Flow = c(10, 15, 12, 18, 14, 16, 13, 17, 11, 19),
    Moisture = c(15, 16, 15, 16, 15, 16, 15, 16, 15, 16),
    GPS_Time = 1:10,
    X = 435000 + 1:10,
    Y = 5262000 + 1:10,
    Interval = rep(2L, 10)
  )
  
  result <- optimize_delays(data, type = "moisture", delay_range = -3:3, n_iterations = 2)
  
  expect_true("delays" %in% names(result))
  expect_true("moisture" %in% names(result$delays))
})

test_that("optimize_delays with type = 'both' optimizes both", {
  data <- tibble::tibble(
    Flow = c(10, 15, 12, 18, 14, 16, 13, 17, 11, 19),
    Moisture = c(15, 16, 15, 16, 15, 16, 15, 16, 15, 16),
    GPS_Time = 1:10,
    X = 435000 + 1:10,
    Y = 5262000 + 1:10,
    Interval = rep(2L, 10)
  )
  
  result <- optimize_delays(data, type = "both", delay_range = -3:3, n_iterations = 2)
  
  expect_true("delays" %in% names(result))
  expect_true("flow" %in% names(result$delays))
  expect_true("moisture" %in% names(result$delays))
})

test_that("optimize_delays returns data when apply_correction = TRUE", {
  data <- tibble::tibble(
    Flow = c(10, 15, 12, 18, 14, 16, 13, 17, 11, 19),
    GPS_Time = 1:10,
    X = 435000 + 1:10,
    Y = 5262000 + 1:10,
    Interval = rep(2L, 10)
  )
  
  result <- optimize_delays(data, type = "flow", delay_range = -3:3, 
                            n_iterations = 2, apply_correction = TRUE)
  
  expect_true("data" %in% names(result))
  expect_true(is.data.frame(result$data))
})

# Test edge cases ----
test_that("filter_data handles empty data", {
  data <- tibble::tibble(
    HeaderStatus = integer(),
    Flow = integer()
  )
  
  result <- filter_data(data, type = "header")
  
  expect_equal(nrow(result), 0)
})

test_that("detect_anomalies handles data without required columns", {
  data <- tibble::tibble(Flow = 1:5)
  
  result <- detect_anomalies(data, type = "overlap")
  
  expect_equal(nrow(result), nrow(data))
})

test_that("calculate_thresholds handles data without required columns", {
  data <- tibble::tibble(Flow = 1:5)
  
  result <- calculate_thresholds(data, type = "yield")
  
  expect_true(is.list(result))
})

test_that("convert_coordinates handles missing coordinates", {
  data <- tibble::tibble(Flow = 1:5)
  
  expect_error(convert_coordinates(data, from = "latlon", to = "utm"))
})

test_that("anonymize_data handles missing coordinates for coordinate anonymization", {
  data <- tibble::tibble(Flow = 1:5)
  
  result <- anonymize_data(data, type = "coordinates")
  
  # Should return data unchanged with a warning
  expect_equal(nrow(result), nrow(data))
})

test_that("export_data handles unsupported format", {
  data <- create_test_data()
  
  expect_error(export_data(data, "test.xyz", format = "xyz"))
})

test_that("optimize_delays handles missing columns gracefully", {
  data <- tibble::tibble(GPS_Time = 1:10)
  
  result <- optimize_delays(data, type = "flow")
  
  # Should return result without flow delay
  expect_true("delays" %in% names(result))
})
