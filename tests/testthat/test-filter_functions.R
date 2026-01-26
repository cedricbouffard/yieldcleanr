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

  # Default behavior: keep both 1 (harvesting) and 33 (header down)
  result <- filter_header_status(data)

  expect_equal(nrow(result), 4)  # Keeps points with HeaderStatus 1, 33, 33, 33 (filters out 0)
  expect_true(all(result$HeaderStatus %in% c(1, 33)))
})

test_that("filter_header_status with only header down value (33)", {
  data <- yield_test_data_header_mixed

  # Keep only header down (33), filter out harvesting (1)
  result <- filter_header_status(data, header_values = c(33))

  expect_equal(nrow(result), 3)  # Keeps points with HeaderStatus 33, 33, 33
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

  expect_equal(nrow(result), 4)  # Keep GPSStatus >= 4 (indices 2, 3, 4, 5)
})

# Test filter_dop ----
test_that("filter_dop removes high DOP values", {
  data <- yield_test_data |>
    mutate(DOP = c(5, 15, 8, 5, 10))

  result <- filter_dop(data, max_dop = 10)

  expect_equal(nrow(result), 4)  # Keep DOP <= 10 (indices 1, 3, 4, 5)
})

# Test filter_velocity ----
test_that("filter_velocity filters by speed range", {
  # Create X, Y that give velocity ~5 m/s when 10m apart and Interval=2s
  # With X = 435000, 435010, 435020... (10m apart) and Y similar
  # Distance between consecutive points = sqrt(10^2 + 10^2) = 14.14 m
  # Velocity = 14.14 / 2 = 7.07 m/s (in range 3-6? No, too high)

  # Let me use points that are 8m apart to get ~4 m/s
  data <- yield_test_data |>
    mutate(
      X = c(435000, 435008, 435016, 435024, 435032),  # 8m apart
      Y = c(5262000, 5262008, 5262016, 5262024, 5262032),
      Interval = c(2L, 2L, 2L, 2L, 2L),
      .row_id = 1:5
    )

  result <- filter_velocity(data, min_velocity = 3, max_velocity = 6)

  # Distance between consecutive points = sqrt(8^2 + 8^2) = 11.31 m
  # Velocity = 11.31 / 2 = 5.66 m/s (in range 3-6)
  # First point has NA velocity and is filtered
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
  expect_equal(result$Flow[1], 1)  # First value is the old Flow[1] (lagged)
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
