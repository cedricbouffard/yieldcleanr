# Test filter_functions ----
library(dplyr)
library(yieldcleanr)

# Test filter_header_status ----
test_that("filter_header_status keeps active harvesting (default: 1 and 33)", {
  data <- tibble::tibble(
    HeaderStatus = c(1L, 33L, 33L, 0L, 1L),
    Flow = 1:5
  )

  result <- filter_header_status(data)

  expect_equal(nrow(result), 4)
  expect_true(all(result$HeaderStatus %in% c(1, 33)))
})

test_that("filter_header_status with only header down value (33)", {
  data <- tibble::tibble(
    HeaderStatus = c(1L, 33L, 33L, 0L, 33L),
    Flow = 1:5
  )

  result <- filter_header_status(data, header_values = c(33))

  expect_equal(nrow(result), 3)
  expect_true(all(result$HeaderStatus == 33))
})

test_that("filter_header_status keeps all when all header active", {
  data <- tibble::tibble(
    HeaderStatus = rep(33L, 5),
    Flow = 1:5
  )

  result <- filter_header_status(data)

  expect_equal(nrow(result), nrow(data))
})

# Test filter_gps_status ----
test_that("filter_gps_status filters by minimum status", {
  data <- tibble::tibble(
    GPSStatus = c(2L, 4L, 4L, 7L, 4L),
    Flow = 1:5
  )

  result <- filter_gps_status(data, min_gps_status = 4)

  expect_equal(nrow(result), 4)
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

# Test filter_velocity ----
test_that("filter_velocity filters by speed range", {
  data <- tibble::tibble(
    X = c(435000, 435010, 435020, 435030, 435040),
    Y = c(5262000, 5262010, 5262020, 5262030, 5262040),
    Interval = c(2L, 2L, 2L, 2L, 2L),
    Flow = 1:5
  )

  result <- filter_velocity(data, min_velocity = 1, max_velocity = 10)

  expect_equal(nrow(result), 4)
})

# Test filter_yield_range ----
test_that("filter_yield_range keeps values within range", {
  data <- tibble::tibble(
    Yield_buacre = c(100, 150, 50, 300, 180),
    Flow = 1:5
  )

  result <- filter_yield_range(data, min_yield = 50, max_yield = 200)

  expect_equal(nrow(result), 4)
  expect_true(all(result$Yield_buacre >= 50 & result$Yield_buacre <= 200))
})

test_that("filter_yield_range removes all when none in range", {
  data <- tibble::tibble(
    Yield_buacre = c(600, 700, 800, 900, 1000),
    Flow = 1:5
  )

  result <- filter_yield_range(data, min_yield = 50, max_yield = 200)

  expect_equal(nrow(result), 0)
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

# Test apply_flow_delay ----
test_that("apply_flow_delay shifts flow values", {
  data <- tibble::tibble(
    Flow = 1:10,
    GPS_Time = 1:10
  )

  result <- apply_flow_delay(data, delay = 1)

  expect_equal(nrow(result), nrow(data) - 1)
})

# Test apply_moisture_delay ----
test_that("apply_moisture_delay shifts moisture values", {
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

# Test latlon_to_utm ----
test_that("latlon_to_utm converts coordinates", {
  data <- tibble::tibble(
    Longitude = c(-69.856661, -69.856681),
    Latitude = c(47.506122, 47.506136)
  )

  result <- latlon_to_utm(data)

  expect_true(all(c("X", "Y") %in% names(result)))
})

# Test filter_bounds ----
test_that("filter_bounds keeps points within lat/lon limits", {
  data <- tibble::tibble(
    Longitude = c(-70, -69.5, -69, -68.5),
    Latitude = c(47, 47.5, 48, 48.5),
    Flow = 1:4
  )
  bounds <- list(min_x = -69.5, max_x = -68, min_y = 47, max_y = 48)

  result <- filter_bounds(data, bounds, coord_type = "latlon")

  expect_equal(nrow(result), 2)
  expect_true(all(result$Longitude >= -69.5))
  expect_true(all(result$Longitude <= -68))
})

test_that("filter_bounds keeps points within UTM limits", {
  data <- tibble::tibble(
    X = c(435000, 436000, 437000, 438000),
    Y = c(5262000, 5263000, 5264000, 5265000),
    Flow = 1:4
  )
  bounds <- list(min_x = 435000, max_x = 437000, min_y = 5262000, max_y = 5264000)

  result <- filter_bounds(data, bounds, coord_type = "utm")

  expect_equal(nrow(result), 3)
})

test_that("filter_bounds returns original data when bounds is NULL", {
  data <- tibble::tibble(Flow = 1:5)

  result <- filter_bounds(data, bounds = NULL)

  expect_equal(nrow(result), nrow(data))
})

# Test edge cases ----
test_that("filter_velocity returns original when X,Y missing", {
  data <- tibble::tibble(Flow = 1:5, Interval = 2)

  result <- filter_velocity(data)

  expect_equal(nrow(result), nrow(data))
})

test_that("filter_velocity handles NA values", {
  data <- tibble::tibble(
    X = c(435000, 435010, NA, 435030, 435040),
    Y = c(5262000, 5262010, NA, 5262030, 5262040),
    Interval = c(2, 2, 2, 2, 2),
    Flow = 1:5
  )

  result <- filter_velocity(data, min_velocity = 1, max_velocity = 10)

  expect_true(nrow(result) <= 4)
})

test_that("filter_header_status handles missing column", {
  data <- tibble::tibble(Flow = 1:5)

  result <- filter_header_status(data)

  expect_equal(nrow(result), nrow(data))
})

test_that("filter_gps_status handles missing column", {
  data <- tibble::tibble(Flow = 1:5)

  result <- filter_gps_status(data)

  expect_equal(nrow(result), nrow(data))
})

test_that("filter_dop handles missing column", {
  data <- tibble::tibble(Flow = 1:5)

  result <- filter_dop(data)

  expect_equal(nrow(result), nrow(data))
})

test_that("filter_yield_range handles missing column", {
  data <- tibble::tibble(Flow = 1:5)

  result <- filter_yield_range(data)

  expect_equal(nrow(result), nrow(data))
})

test_that("filter_moisture_range handles missing column", {
  data <- tibble::tibble(Flow = 1:5)

  result <- filter_moisture_range(data)

  expect_equal(nrow(result), nrow(data))
})