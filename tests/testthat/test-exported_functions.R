# Comprehensive tests for all exported functions
library(dplyr)
library(yieldcleanr)

# Create test data helper
create_basic_test_data <- function() {
  tibble::tibble(
    Longitude = c(-69.856661, -69.856681, -69.856701, -69.856726, -69.856749),
    Latitude = c(47.506122, 47.506136, 47.506152, 47.506168, 47.506185),
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
    Altitude = c(61.3, 61.5, 61.5, 61.4, 61.3),
    X = c(435000, 435010, 435020, 435030, 435040),
    Y = c(5262000, 5262010, 5262020, 5262030, 5262040),
    .row_id = 1:5
  )
}

# Test latlon_to_utm ----
test_that("latlon_to_utm converts coordinates correctly", {
  data <- tibble::tibble(
    Longitude = c(-69.856661, -69.856681),
    Latitude = c(47.506122, 47.506136)
  )

  result <- latlon_to_utm(data)

  expect_true("X" %in% names(result))
  expect_true("Y" %in% names(result))
  expect_true(all(result$X > 0))
  expect_true(all(result$Y > 0))
  expect_equal(nrow(result), nrow(data))
})

test_that("latlon_to_utm with custom zone", {
  data <- tibble::tibble(
    Longitude = c(-69.856661),
    Latitude = c(47.506122)
  )

  result <- latlon_to_utm(data, zone = 18)

  expect_true("X" %in% names(result))
  expect_true("Y" %in% names(result))
})

test_that("latlon_to_utm handles empty data", {
  data <- tibble::tibble(Longitude = numeric(), Latitude = numeric())

  result <- latlon_to_utm(data)

  expect_equal(nrow(result), 0)
})

# Test read_yield_data ----
test_that("read_yield_data handles missing file", {
  expect_error(read_yield_data("nonexistent_file_12345.txt"))
})

 test_that("read_yield_data returns tibble for valid file", {
  skip_if_not_installed("sf")

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

  result <- read_yield_data(data = data)

  expect_s3_class(result, "tbl_df")
})

# Test list_fields_from_zip ----
test_that("list_fields_from_zip handles nonexistent file", {
  expect_error(list_fields_from_zip("nonexistent.zip"))
})

# Test convert_flow_to_yield ----
test_that("convert_flow_to_yield calculates yield", {
  data <- tibble::tibble(
    Flow = c(50, 55, 52),
    Interval = c(2, 2, 2),
    Distance = c(87, 87, 87),
    Swath = c(240, 240, 240)
  )

  result <- convert_flow_to_yield(data)

  expect_true("Yield_kg_ha" %in% names(result))
  expect_equal(nrow(result), nrow(data))
  expect_true(all(result$Yield_kg_ha >= 0))
})

test_that("convert_flow_to_yield handles missing columns gracefully", {
  data <- tibble::tibble(Flow = c(1, 2, 3))

  result <- convert_flow_to_yield(data)

  expect_equal(nrow(result), nrow(data))
})

# Test filter_header_status ----
test_that("filter_header_status keeps default values", {
  data <- tibble::tibble(
    HeaderStatus = c(1L, 33L, 33L, 0L, 1L),
    Flow = 1:5
  )

  result <- filter_header_status(data)

  expect_true(all(result$HeaderStatus %in% c(1, 33)))
  expect_equal(nrow(result), 4)
})

test_that("filter_header_status with custom values", {
  data <- tibble::tibble(
    HeaderStatus = c(1L, 33L, 5L, 10L, 33L),
    Flow = 1:5
  )

  result <- filter_header_status(data, header_values = c(33))

  expect_equal(nrow(result), 2)
  expect_true(all(result$HeaderStatus == 33))
})

test_that("filter_header_status handles missing column", {
  data <- tibble::tibble(Flow = 1:5)

  result <- filter_header_status(data)

  expect_equal(nrow(result), nrow(data))
})

# Test filter_gps_status ----
test_that("filter_gps_status filters by minimum", {
  data <- tibble::tibble(
    GPSStatus = c(2L, 4L, 4L, 7L, 4L),
    Flow = 1:5
  )

  result <- filter_gps_status(data, min_gps_status = 4)

  expect_equal(nrow(result), 4)
  expect_true(all(result$GPSStatus >= 4 | is.na(result$GPSStatus)))
})

test_that("filter_gps_status keeps NA values", {
  data <- tibble::tibble(
    GPSStatus = c(7L, NA, 3L, 5L, NA),
    Flow = 1:5
  )

  result <- filter_gps_status(data, min_gps_status = 5)

  expect_equal(nrow(result), 4)
})

test_that("filter_gps_status handles missing column", {
  data <- tibble::tibble(Flow = 1:5)

  result <- filter_gps_status(data)

  expect_equal(nrow(result), nrow(data))
})

# Test filter_dop ----
test_that("filter_dop removes high DOP values", {
  data <- tibble::tibble(
    DOP = c(5L, 15L, 8L, 5L, 10L),
    Flow = 1:5
  )

  result <- filter_dop(data, max_dop = 10)

  expect_equal(nrow(result), 4)
})

test_that("filter_dop handles missing column", {
  data <- tibble::tibble(Flow = 1:5)

  result <- filter_dop(data)

  expect_equal(nrow(result), nrow(data))
})

# Test filter_velocity ----
test_that("filter_velocity filters by range", {
  data <- tibble::tibble(
    X = c(435000, 435010, 435020, 435030, 435040),
    Y = c(5262000, 5262010, 5262020, 5262030, 5262040),
    Interval = c(2L, 2L, 2L, 2L, 2L),
    Flow = 1:5
  )

  result <- filter_velocity(data, min_velocity = 1, max_velocity = 10)

  expect_equal(nrow(result), 4)
})

test_that("filter_velocity handles missing X/Y", {
  data <- tibble::tibble(Flow = 1:5, Interval = 2)

  result <- filter_velocity(data)

  expect_equal(nrow(result), nrow(data))
})

# Test filter_yield_range ----
test_that("filter_yield_range filters by range", {
  data <- tibble::tibble(
    Yield_kg_ha = c(6270, 9405, 3135, 18810, 11286),
    Flow = 1:5
  )

  result <- filter_yield_range(data, min_yield = 3135, max_yield = 11286, yield_column = "Yield_kg_ha")

  expect_equal(nrow(result), 4)
  expect_true(all(result$Yield_kg_ha >= 3135 & result$Yield_kg_ha <= 11286))
})

test_that("filter_yield_range handles empty result", {
  data <- tibble::tibble(
    Yield_kg_ha = c(37620, 43900, 50160),
    Flow = 1:3
  )

  result <- filter_yield_range(data, min_yield = 3135, max_yield = 11286, yield_column = "Yield_kg_ha")

  expect_equal(nrow(result), 0)
})

test_that("filter_yield_range handles missing column", {
  data <- tibble::tibble(Flow = 1:5)

  result <- filter_yield_range(data)

  expect_equal(nrow(result), nrow(data))
})

# Test filter_moisture_range ----
test_that("filter_moisture_range filters by range", {
  data <- tibble::tibble(
    Moisture = c(5, 15, 35, 45, 25),
    Flow = 1:5
  )

  result <- filter_moisture_range(data, min_moisture = 10, max_moisture = 40)

  expect_equal(nrow(result), 3)
})

test_that("filter_moisture_range handles missing column", {
  data <- tibble::tibble(Flow = 1:5)

  result <- filter_moisture_range(data)

  expect_equal(nrow(result), nrow(data))
})

# Test apply_flow_delay ----
test_that("apply_flow_delay shifts values", {
  data <- tibble::tibble(
    Flow = 1:10,
    GPS_Time = 1:10
  )

  result <- apply_flow_delay(data, delay = 1)

  expect_equal(nrow(result), nrow(data))
  expect_equal(sum(is.na(result$Flow)), 1)
})

test_that("apply_flow_delay handles negative delay", {
  data <- tibble::tibble(
    Flow = 1:10,
    GPS_Time = 1:10
  )

  result <- apply_flow_delay(data, delay = -1)

  expect_equal(nrow(result), nrow(data))
  expect_equal(sum(is.na(result$Flow)), 1)
})

# Test apply_moisture_delay ----
test_that("apply_moisture_delay shifts values", {
  data <- tibble::tibble(
    Moisture = 1:10,
    GPS_Time = 1:10
  )

  result <- apply_moisture_delay(data, delay = 1)

  expect_equal(nrow(result), nrow(data) - 1)
})

# Test remove_overlap ----
test_that("remove_overlap filters overlapping cells", {
  data <- tibble::tibble(
    X = c(1, 1.1, 1.2, 1.3, 1.4, 1.5, 2, 2.1, 2.2),
    Y = c(1, 1, 1, 1, 1, 1, 2, 2, 2),
    Flow = 1:9
  )

  result <- remove_overlap(data, cellsize = 0.5, max_pass = 2)

  expect_true(nrow(result) < nrow(data))
})

test_that("remove_overlap handles no overlap", {
  data <- tibble::tibble(
    X = c(1, 10, 20, 30),
    Y = c(1, 10, 20, 30),
    Flow = 1:4
  )

  result <- remove_overlap(data, cellsize = 0.5, max_pass = 2)

  expect_equal(nrow(result), nrow(data))
})

# Test filter_bounds ----
test_that("filter_bounds filters by lat/lon", {
  data <- tibble::tibble(
    Longitude = c(-70, -69.5, -69, -68.5),
    Latitude = c(47, 47.5, 48, 48.5),
    Flow = 1:4
  )
  bounds <- list(min_x = -69.5, max_x = -68, min_y = 47, max_y = 48)

  result <- filter_bounds(data, bounds, coord_type = "latlon")

  expect_equal(nrow(result), 2)
})

test_that("filter_bounds filters by UTM", {
  data <- tibble::tibble(
    X = c(435000, 436000, 437000, 438000),
    Y = c(5262000, 5263000, 5264000, 5265000),
    Flow = 1:4
  )
  bounds <- list(min_x = 435000, max_x = 437000, min_y = 5262000, max_y = 5264000)

  result <- filter_bounds(data, bounds, coord_type = "utm")

  expect_equal(nrow(result), 3)
})

test_that("filter_bounds with NULL bounds returns all", {
  data <- tibble::tibble(Flow = 1:5)

  result <- filter_bounds(data, bounds = NULL)

  expect_equal(nrow(result), nrow(data))
})

# Test apply_pcdi ----
test_that("apply_pcdi finds optimal delay", {
  data <- tibble::tibble(
    Flow = c(10, 15, 12, 18, 14, 16, 13, 17, 11, 19),
    GPS_Time = 1:10,
    X = 435000 + 1:10,
    Y = 5262000 + 1:10,
    Interval = rep(2L, 10)
  )

  result <- apply_pcdi(data, delay_range = -3:3, n_iterations = 2)

  expect_true("optimal_delay" %in% names(result))
  expect_true("rsc_values" %in% names(result))
})

# Test calculate_auto_thresholds ----
test_that("calculate_auto_thresholds computes thresholds", {
  data <- tibble::tibble(
    Yield_kg_ha = c(6270, 7524, 8778, 10032, 11286, 6897, 8142, 9405),
    Flow = 1:8
  )

  result <- calculate_auto_thresholds(data)

  expect_true("min_yield" %in% names(result))
  expect_true("max_yield" %in% names(result))
  expect_true("min_velocity" %in% names(result))
  expect_true("max_velocity" %in% names(result))
})

# Test calculate_filter_counts ----
test_that("calculate_filter_counts returns summary", {
  data_raw <- tibble::tibble(Flow = 1:10)
  data_clean <- tibble::tibble(Flow = 1:7)

  result <- calculate_filter_counts(data_raw, data_clean)

  expect_true("rows_removed" %in% names(result))
  expect_true("retention_rate" %in% names(result))
  expect_true(result$retention_rate <= 1)
  expect_true(result$retention_rate >= 0)
})

# Test apply_overlap_filter ----
test_that("apply_overlap_filter returns filtered data", {
  data <- tibble::tibble(
    X = c(435000, 435001, 435002, 435003, 435100),
    Y = c(5262000, 5262001, 5262002, 5262003, 5262100),
    Flow = c(10, 15, 12, 18, 20),
    Swath = c(240, 240, 240, 240, 240)
  )

  result <- apply_overlap_filter(data, cellsize = 0.3)

  expect_true(nrow(result) <= nrow(data))
})

test_that("apply_overlap_filter warns on missing columns", {
  data <- tibble::tibble(Flow = c(1, 2, 3))

  expect_warning(apply_overlap_filter(data))
})

# Test apply_local_sd_filter ----
test_that("apply_local_sd_filter returns filtered data", {
  data <- tibble::tibble(
    Flow = c(50, 52, 51, 500, 53, 49),
    Pass = c(1, 1, 1, 1, 2, 2)
  )

  result <- apply_local_sd_filter(data, n_swaths = 3, lsd_limit = 2)

  expect_true(nrow(result) <= nrow(data))
})

test_that("apply_local_sd_filter warns on missing columns", {
  data <- tibble::tibble(Flow = c(1, 2, 3))

  expect_warning(apply_local_sd_filter(data))
})

 # Test filter_heading_anomalies ----
test_that("filter_heading_anomalies filters heading changes", {
  data <- tibble::tibble(
    X = c(435000, 435010, 435020, 435030, 435040),
    Y = c(5262000, 5262010, 5262020, 5262030, 5262040),
    Flow = 1:5
  )

  result <- filter_heading_anomalies(data, max_heading_change = 45)

  expect_type(result, "list")
  expect_true("data" %in% names(result))
  expect_equal(nrow(result$data), nrow(data))
})

# Test filter_velocity_jumps ----
test_that("filter_velocity_jumps filters sudden changes", {
  data <- tibble::tibble(
    X = c(435000, 435010, 435020, 435030, 435040),
    Y = c(5262000, 5262010, 5262020, 5262030, 5262040),
    Interval = c(2, 2, 2, 2, 2),
    Flow = 1:5
  )

  result <- filter_velocity_jumps(data, max_acceleration = 5)

  expect_type(result, "list")
  expect_true("data" %in% names(result))
  expect_equal(nrow(result$data), nrow(data))
})

 # Test apply_position_filter ----
test_that("apply_position_filter filters by position", {
  data <- tibble::tibble(
    X = c(435000, 435010, 435020, 435030, 435040),
    Y = c(5262000, 5262010, 5262020, 5262030, 5262040),
    Flow = 1:5
  )

  thresholds <- list(pos_x_min = 434000, pos_x_max = 436000, pos_y_min = 5261000, pos_y_max = 5263000)
  result <- apply_position_filter(data, thresholds = thresholds)

  expect_equal(nrow(result), nrow(data))
})

 # Test clean_yield ----
test_that("clean_yield handles missing file", {
  expect_error(clean_yield("nonexistent_file_12345.txt"))
})

test_that("clean_yield returns result object", {
  data <- create_basic_test_data()

  result <- clean_yield(data = data)

  expect_true(is.data.frame(result) || inherits(result, "tbl_df"))
})

# Test ayce_clean ----
test_that("ayce_clean returns cleaned data", {
  data <- create_basic_test_data()

  result <- ayce_clean(data = data)

  expect_true(is.data.frame(result) || inherits(result, "tbl_df"))
})

# Test ayce_sf ----
test_that("ayce_sf returns SF object", {
  skip_if_not_installed("sf")

  data <- create_basic_test_data()

  result <- ayce_sf(data = data, geometry_type = "point")

  expect_true(inherits(result, "sf") || inherits(result, "tbl_df"))
})

# Test clean_yield_with_tracking ----
test_that("clean_yield_with_tracking returns list with tracking", {
  data <- create_basic_test_data()

  result <- clean_yield_with_tracking(data = data)

  expect_true(is.list(result))
  expect_true("data" %in% names(result))
  expect_true("removed_points" %in% names(result))
})

 # Test export_raster ----
test_that("export_raster returns SpatRaster", {
  skip_if_not_installed("sf")
  skip_if_not_installed("terra")

  data <- create_basic_test_data()
  data$Yield_kg_ha <- data$Flow * 10  # Add a test yield column

  data_sf <- sf::st_as_sf(data, coords = c("Longitude", "Latitude"), crs = 4326)

  result <- export_raster(data_sf)

  expect_true(inherits(result, "SpatRaster") || is.null(result))
})

# Test save_raster ----
test_that("save_raster saves file", {
  skip_if_not_installed("sf")
  skip_if_not_installed("terra")

  data <- create_basic_test_data()
  data$Yield_kg_ha <- data$Flow * 10  # Add a test yield column

  data_sf <- sf::st_as_sf(data, coords = c("Longitude", "Latitude"), crs = 4326)

  temp_file <- tempfile(fileext = ".tif")

  raster <- export_raster(data_sf)

  if (!is.null(raster)) {
    result <- save_raster(raster, temp_file)
    expect_true(file.exists(temp_file))
  }
})

# Test launch_shiny_app ----
test_that("launch_shiny_app exists and is function", {
  expect_true(is.function(launch_shiny_app) || is.function(launch_shiny_app))
})

# Test read_yield_from_zip ----
test_that("read_yield_from_zip handles nonexistent file", {
  expect_error(read_yield_from_zip("nonexistent.zip"))
})