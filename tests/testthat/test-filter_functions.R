# Test filter_functions - Tests pour les méta-fonctions de filtrage ----
library(dplyr)
library(yieldcleanr)

# Test filter_data - méta-fonction unifiée ----
test_that("filter_data applies header filter correctly", {
  data <- tibble::tibble(
    HeaderStatus = c(1L, 33L, 33L, 0L, 1L),
    Flow = 1:5
  )

  result <- filter_data(data, type = "header")

  expect_equal(nrow(result), 4)
  expect_true(all(result$HeaderStatus %in% c(1, 33)))
})

test_that("filter_data applies gps filter correctly", {
  data <- tibble::tibble(
    GPSStatus = c(2L, 4L, 4L, 7L, 4L),
    Flow = 1:5
  )

  result <- filter_data(data, type = "gps", min_gps_status = 4)

  expect_equal(nrow(result), 4)
})

test_that("filter_data applies dop filter correctly", {
  data <- tibble::tibble(
    DOP = c(5L, 15L, 8L, 5L, 10L),
    Flow = 1:5
  )

  result <- filter_data(data, type = "dop", max_dop = 10)

  expect_equal(nrow(result), 4)
})

test_that("filter_data applies velocity filter correctly", {
  data <- tibble::tibble(
    X = c(435000, 435010, 435020, 435030, 435040),
    Y = c(5262000, 5262010, 5262020, 5262030, 5262040),
    Interval = c(2L, 2L, 2L, 2L, 2L),
    Flow = 1:5
  )

  result <- filter_data(data, type = "velocity", min_velocity = 1, max_velocity = 10)

  expect_equal(nrow(result), 4)
})

test_that("filter_data applies yield filter correctly", {
  data <- tibble::tibble(
    Yield_kg_ha = c(6270, 9405, 3135, 18810, 11286),
    Flow = 1:5
  )

  result <- filter_data(data, type = "yield", min_yield = 3135, max_yield = 11286)

  expect_equal(nrow(result), 4)
  expect_true(all(result$Yield_kg_ha >= 3135 & result$Yield_kg_ha <= 11286))
})

test_that("filter_data removes all yield values when none in range", {
  data <- tibble::tibble(
    Yield_kg_ha = c(37620, 43900, 50160, 56430, 62700),
    Flow = 1:5
  )

  result <- filter_data(data, type = "yield", min_yield = 3135, max_yield = 11286)

  expect_equal(nrow(result), 0)
})

test_that("filter_data applies moisture filter correctly", {
  data <- tibble::tibble(
    Moisture = c(5, 15, 35, 45, 25),
    Flow = 1:5
  )

  result <- filter_data(data, type = "moisture", min_moisture = 10, max_moisture = 40)

  expect_equal(nrow(result), 3)
})

test_that("filter_data applies multiple filters in sequence", {
  data <- tibble::tibble(
    HeaderStatus = c(1L, 33L, 33L, 0L, 1L),
    GPSStatus = c(2L, 4L, 4L, 7L, 4L),
    Flow = 1:5
  )

  result <- filter_data(data, type = c("header", "gps"))

  expect_true(nrow(result) < nrow(data))
})

test_that("filter_data with type = 'all' applies all available filters", {
  data <- tibble::tibble(
    X = c(435000, 435010, 435020, 435030, 435040),
    Y = c(5262000, 5262010, 5262020, 5262030, 5262040),
    Interval = c(2L, 2L, 2L, 2L, 2L),
    Yield_kg_ha = c(6270, 9405, 3135, 18810, 11286),
    Moisture = c(15, 16, 15, 16, 15),
    HeaderStatus = c(33L, 33L, 33L, 33L, 33L),
    GPSStatus = c(7L, 7L, 7L, 7L, 7L),
    DOP = c(0, 0, 0, 0, 0),
    Flow = 1:5
  )

  result <- filter_data(data, type = "all")

  expect_true(is.data.frame(result))
  expect_true(nrow(result) <= nrow(data))
})

# Test latlon_to_utm ----
test_that("latlon_to_utm converts coordinates correctly", {
  data <- tibble::tibble(
    Longitude = c(-69.856661, -69.856681),
    Latitude = c(47.506122, 47.506136)
  )

  result <- latlon_to_utm(data)

  expect_true(all(c("X", "Y") %in% names(result)))
})

# Test convert_coordinates - méta-fonction ----
test_that("convert_coordinates converts latlon to utm", {
  data <- tibble::tibble(
    Longitude = c(-69.856661, -69.856681),
    Latitude = c(47.506122, 47.506136)
  )

  result <- convert_coordinates(data, from = "latlon", to = "utm")

  expect_true(all(c("X", "Y") %in% names(result)))
})

test_that("convert_coordinates returns same data when from == to", {
  data <- tibble::tibble(
    X = c(435000, 435010),
    Y = c(5262000, 5262010),
    Flow = 1:2
  )

  result <- convert_coordinates(data, from = "utm", to = "utm")

  expect_equal(nrow(result), nrow(data))
})

# Test edge cases ----
test_that("filter_data handles missing columns gracefully", {
  data <- tibble::tibble(Flow = 1:5)

  result <- filter_data(data, type = "all")

  expect_equal(nrow(result), nrow(data))
})

test_that("filter_data handles empty data", {
  data <- tibble::tibble(
    HeaderStatus = integer(),
    Flow = integer()
  )

  result <- filter_data(data, type = "header")

  expect_equal(nrow(result), 0)
})

test_that("filter_data handles NA values in coordinates", {
  data <- tibble::tibble(
    X = c(435000, 435010, NA, 435030, 435040),
    Y = c(5262000, 5262010, NA, 5262030, 5262040),
    Interval = c(2, 2, 2, 2, 2),
    Flow = 1:5
  )

  result <- filter_data(data, type = "velocity", min_velocity = 1, max_velocity = 10)

  expect_true(nrow(result) <= 4)
})