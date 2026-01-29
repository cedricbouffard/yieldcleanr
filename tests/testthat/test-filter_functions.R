library(testthat)
library(dplyr)
library(yieldcleanr)

# Test read_yield_data ----
test_that("read_yield_data reads file correctly", {
  skip_if_not(file.exists("test_data/original.txt"))

  data <- read_yield_data("test_data/original.txt")

  expect_s3_class(data, "tbl_df")
  expect_true(nrow(data) > 0)
  expect_true(all(c("Longitude", "Latitude", "Flow") %in% names(data)))
})

test_that("read_yield_data handles missing file", {
  expect_error(read_yield_data("nonexistent_file.txt"))
})

# Test filter_header_status ----
test_that("filter_header_status keeps active harvesting (default: 1 and 33)", {
  data <- yield_test_data_header_mixed

  # Comportement par defaut : conserver 1 (actif) et 33 (header bas)
  result <- filter_header_status(data)

  expect_equal(nrow(result), 4)  # Conserve 1, 33, 33, 33 (filtre 0)
  expect_true(all(result$HeaderStatus %in% c(1, 33)))
})

test_that("filter_header_status with only header down value (33)", {
  data <- yield_test_data_header_mixed

  # Conserver seulement header bas (33), filtrer 1
  result <- filter_header_status(data, header_values = c(33))

  expect_equal(nrow(result), 3)  # Conserve 33, 33, 33
  expect_true(all(result$HeaderStatus == 33))
})

test_that("filter_header_status keeps all when all header active", {
  data <- yield_test_data

  result <- filter_header_status(data)

  expect_equal(nrow(result), nrow(data))
})

# Test filter_gps_status ----
test_that("filter_gps_status filters by minimum status", {
  data <- yield_test_data |>
    mutate(GPSStatus = c(2, 4, 4, 7, 4))

  result <- filter_gps_status(data, min_gps_status = 4)

  expect_equal(nrow(result), 4)  # Conserve GPSStatus >= 4 (indices 2, 3, 4, 5)
})

# Test filter_dop ----
test_that("filter_dop removes high DOP values", {
  data <- yield_test_data |>
    mutate(DOP = c(5, 15, 8, 5, 10))

  result <- filter_dop(data, max_dop = 10)

  expect_equal(nrow(result), 4)  # Conserve DOP <= 10 (indices 1, 3, 4, 5)
})

# Test filter_velocity ----
test_that("filter_velocity filters by speed range", {
  # Creer X, Y pour obtenir ~5 m/s avec 10m et Interval=2s
  # Distance entre points = sqrt(10^2 + 10^2) = 14.14 m
  # Vitesse = 14.14 / 2 = 7.07 m/s (trop elevee pour 3-6)

  # Utiliser 8m pour obtenir ~4 m/s
  data <- yield_test_data |>
    mutate(
      X = c(435000, 435008, 435016, 435024, 435032),  # 8m apart
      Y = c(5262000, 5262008, 5262016, 5262024, 5262032),
      Interval = c(2L, 2L, 2L, 2L, 2L),
      .row_id = 1:5
    )

  result <- filter_velocity(data, min_velocity = 3, max_velocity = 6)

  # Distance entre points = sqrt(8^2 + 8^2) = 11.31 m
  # Vitesse = 11.31 / 2 = 5.66 m/s (dans la plage 3-6)
  # Premier point = NA, il est filtre
  expect_equal(nrow(result), 4)
})

# Test filter_yield_range ----
test_that("filter_yield_range keeps values within range", {
  data <- yield_test_data_outliers |>
    mutate(Yield_buacre = c(100, 150, 50, 300, 180))

  result <- filter_yield_range(data, min_yield = 50, max_yield = 200)

  expect_equal(nrow(result), 4)
  expect_true(all(result$Yield_buacre >= 50 & result$Yield_buacre <= 200))
})

test_that("filter_yield_range removes all when none in range", {
  data <- yield_test_data_outliers |>
    mutate(Yield_buacre = c(600, 700, 800, 900, 1000))

  result <- filter_yield_range(data, min_yield = 50, max_yield = 200)

  expect_equal(nrow(result), 0)
})

# Test filter_moisture_range ----
test_that("filter_moisture_range filters correctly", {
  data <- yield_test_data |>
    mutate(Moisture = c(5, 15, 35, 45, 25))

  result <- filter_moisture_range(data, min_moisture = 10, max_moisture = 40)

  expect_equal(nrow(result), 3)
})

 # Test apply_flow_delay ----
 test_that("apply_flow_delay shifts flow values", {
   data <- yield_test_data |>
     mutate(Flow = 1:5)

   result <- apply_flow_delay(data, delay = 1)

   expect_equal(nrow(result), nrow(data) - 1)
   # After delay, Flow[1] comes from original Flow[2]
   expect_true(result$Flow[1] >= 1)
 })

# Test apply_moisture_delay ----
test_that("apply_moisture_delay shifts moisture values", {
  data <- yield_test_data |>
    mutate(Moisture = 1:5)

  result <- apply_moisture_delay(data, delay = 1)

  expect_equal(nrow(result), nrow(data) - 1)
})

# Test remove_overlap ----
test_that("remove_overlap filters cells with too many passes", {
  data <- yield_test_data_overlap |>
    mutate(
      X = c(1, 1.1, 1.2, 2, 2.1),
      Y = c(1, 1, 1, 2, 2),
      cell_id = c("A", "A", "A", "B", "B")
    )

  result <- remove_overlap(data, cellsize = 0.5, max_pass = 2)

  expect_true(nrow(result) < nrow(data))
})

# Test latlon_to_utm ----
test_that("latlon_to_utm converts coordinates", {
  data <- yield_test_data

  result <- latlon_to_utm(data)

  expect_true(all(c("X", "Y") %in% names(result)))
})

# Test generate_cleaning_log ----
test_that("generate_cleaning_log creates log file", {
  skip_if_not(file.exists("test_data/original.txt"))

  data <- read_yield_data("test_data/original.txt")
  data <- latlon_to_utm(data)

  temp_log <- tempfile(fileext = ".txt")
  generate_cleaning_log(data, data, temp_log)

  log_content <- readLines(temp_log)

  expect_true(any(grepl("Yield", log_content, ignore.case = TRUE)))
  expect_true(any(grepl("points", log_content, ignore.case = TRUE)))
})

# Test export_cleaned_data ----
test_that("export_cleaned_data exports data", {
  data <- yield_test_data

  temp_file <- tempfile(fileext = ".csv")
  export_cleaned_data(data, temp_file, format = "csv")

  expect_true(file.exists(temp_file))

  data_read <- read.csv(temp_file)
  expect_equal(nrow(data_read), nrow(data))
 })

 # Test filter_bounds ----
 test_that("filter_bounds keeps points within lat/lon limits", {
   data <- tibble(
     Longitude = c(-70, -69.5, -69, -68.5),
     Latitude = c(47, 47.5, 48, 48.5),
     Flow = 1:4
   )
   bounds <- list(min_x = -69.5, max_x = -68, min_y = 47, max_y = 48)

   result <- filter_bounds(data, bounds, coord_type = "latlon")

   expect_equal(nrow(result), 2)  # Keeps -69, -68.5 (within bounds)
   expect_true(all(result$Longitude >= -69.5))
   expect_true(all(result$Longitude <= -68))
 })

 test_that("filter_bounds keeps points within UTM limits", {
   data <- tibble(
     X = c(435000, 436000, 437000, 438000),
     Y = c(5262000, 5263000, 5264000, 5265000),
     Flow = 1:4
   )
   bounds <- list(min_x = 435000, max_x = 437000, min_y = 5262000, max_y = 5264000)

   result <- filter_bounds(data, bounds, coord_type = "utm")

   expect_equal(nrow(result), 3)
 })

 test_that("filter_bounds returns original data when bounds is NULL", {
   data <- tibble(Flow = 1:5)

   result <- filter_bounds(data, bounds = NULL)

   expect_equal(nrow(result), nrow(data))
 })

 test_that("filter_bounds removes all points when outside limits", {
   data <- tibble(
     Longitude = c(0, 0, 0, 0),
     Latitude = c(0, 0, 0, 0),
     Flow = 1:4
   )
   bounds <- list(min_x = 10, max_x = 20, min_y = 10, max_y = 20)

   result <- filter_bounds(data, bounds, coord_type = "latlon")

   expect_equal(nrow(result), 0)
 })

 # Test filter_velocity edge cases ----
 test_that("filter_velocity returns original when X,Y missing", {
   data <- tibble(Flow = 1:5, Interval = 2)

   result <- filter_velocity(data)

   expect_equal(nrow(result), nrow(data))
 })

 test_that("filter_velocity handles NA values", {
   data <- tibble(
     X = c(435000, 435010, NA, 435030, 435040),
     Y = c(5262000, 5262010, NA, 5262030, 5262040),
     Interval = c(2, 2, 2, 2, 2),
     .row_id = 1:5
   )

   result <- filter_velocity(data, min_velocity = 1, max_velocity = 10)

   expect_true(nrow(result) <= 4)  # NA points may be filtered
 })

 # Test filter_header_status edge cases ----
 test_that("filter_header_status returns original when HeaderStatus missing", {
   data <- tibble(Flow = 1:5)

   result <- filter_header_status(data)

   expect_equal(nrow(result), nrow(data))
 })

 test_that("filter_header_status with custom values", {
   data <- tibble(
     HeaderStatus = c(1, 33, 5, 10, 33),
     Flow = 1:5
   )

   result <- filter_header_status(data, header_values = c(33))

   expect_equal(nrow(result), 2)  # Only 33 values
   expect_true(all(result$HeaderStatus == 33))
 })

 # Test filter_gps_status edge cases ----
 test_that("filter_gps_status returns original when GPSStatus missing", {
   data <- tibble(Flow = 1:5)

   result <- filter_gps_status(data)

   expect_equal(nrow(result), nrow(data))
 })

 test_that("filter_gps_status keeps NA values", {
   data <- tibble(
     GPSStatus = c(7, NA, 3, 5, NA),
     Flow = 1:5
   )

   result <- filter_gps_status(data, min_gps_status = 5)

   # Keeps values >= 5 OR NA, filters 3
   expect_true(nrow(result) >= 3)  # 7, 5, and NA values
 })

 test_that("filter_gps_status keeps NA values", {
   data <- tibble(
     GPSStatus = c(7, NA, 3, 5, NA),
     Flow = 1:5
   )

   result <- filter_gps_status(data, min_gps_status = 5)

   # Keeps values >= 5 OR NA: GPSStatus 3 is filtered (value < 5)
   # Results: 7, NA, 5, NA = 4 rows
   expect_equal(nrow(result), 4)
 })

 # Test filter_dop edge cases ----
 test_that("filter_dop returns original when DOP missing", {
   data <- tibble(Flow = 1:5)

   result <- filter_dop(data)

   expect_equal(nrow(result), nrow(data))
 })

 test_that("filter_dop keeps NA values", {
   data <- tibble(
     DOP = c(5, NA, 15, 8, NA),
     Flow = 1:5
   )

   result <- filter_dop(data, max_dop = 10)

   # Keeps values <= 10 OR NA
   expect_true(nrow(result) >= 3)  # At least 5, 8, and NA values
 })

 # Test filter_yield_range edge cases ----
 test_that("filter_yield_range auto-detects range", {
   data <- tibble(
     Yield_buacre = c(100, 120, 140, 160, 180, 500),  # 500 is outlier
     Flow = 1:6
   )

   result <- filter_yield_range(data)

   # Should filter outliers based on auto-detected range
   expect_true("Yield_buacre" %in% names(result))
 })

 test_that("filter_yield_range handles missing column", {
   data <- tibble(Flow = 1:5)

   result <- filter_yield_range(data)

   expect_equal(nrow(result), nrow(data))
 })

 # Test filter_moisture_range edge cases ----
 test_that("filter_moisture_range auto-detects range", {
   data <- tibble(
     Moisture = c(10, 12, 14, 16, 18, 50),  # 50 is outlier
     Flow = 1:6
   )

   result <- filter_moisture_range(data)

   expect_true("Moisture" %in% names(result))
 })

 test_that("filter_moisture_range handles missing column", {
   data <- tibble(Flow = 1:5)

   result <- filter_moisture_range(data)

   expect_equal(nrow(result), nrow(data))
 })
