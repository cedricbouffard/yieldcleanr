# Additional tests for read_data.R ----
library(dplyr)
library(yieldcleanr)

test_that("read_yield_from_zip handles nonexistent file", {
  expect_error(
    read_yield_from_zip("nonexistent.zip", "field1"),
    "ZIP n'existe pas"
  )
})

test_that("list_fields_from_zip handles nonexistent file", {
  expect_error(
    list_fields_from_zip("nonexistent.zip"),
    "ZIP n'existe pas"
  )
})

test_that("list_fields_from_zip handles empty zip", {
  # Create empty zip
  temp_zip <- tempfile(fileext = ".zip")
  temp_dir <- tempfile()
  dir.create(temp_dir)
  file.create(file.path(temp_dir, "dummy.txt"))
  utils::zip(temp_zip, file.path(temp_dir, "dummy.txt"), flags = "-j")
  on.exit({
    unlink(temp_zip)
    unlink(temp_dir, recursive = TRUE)
  })
  
  result <- list_fields_from_zip(temp_zip)
  
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
})

test_that("detect_and_convert_imperial_units with inches", {
  data <- tibble::tibble(
    Distance = c(80, 85, 90),  # In inches
    Swath = c(300, 310, 320)   # In inches
  )
  
  result <- yieldcleanr:::detect_and_convert_imperial_units(data)
  
  # Should convert to meters (0.0254 factor)
  expect_true(all(result$Distance < 3))
  expect_true(all(result$Swath < 10))
})

test_that("detect_and_convert_imperial_units with meters", {
  data <- tibble::tibble(
    Distance = c(1.5, 2.0, 2.5),  # Already in meters
    Swath = c(8, 9, 10)           # Already in meters
  )
  
  result <- yieldcleanr:::detect_and_convert_imperial_units(data)
  
  # Should keep as meters
  expect_equal(result$Distance, data$Distance)
  expect_equal(result$Swath, data$Swath)
})

test_that("detect_and_convert_imperial_units handles missing columns", {
  data <- tibble::tibble(Flow = 1:3)
  
  result <- yieldcleanr:::detect_and_convert_imperial_units(data)
  
  expect_equal(nrow(result), 3)
})

test_that("detect_and_convert_imperial_units with small swath values", {
  data <- tibble::tibble(
    Distance = c(2, 2.5, 3),
    Swath = c(2.5, 2.8, 2.2)  # Small values, probably in inches
  )
  
  result <- yieldcleanr:::detect_and_convert_imperial_units(data)
  
  # Should convert
  expect_true(all(result$Swath < 1))
})

test_that("detect_and_convert_imperial_units with large swath values", {
  data <- tibble::tibble(
    Distance = c(100, 110, 120),  # Inches
    Swath = c(150, 160, 170)      # Inches (100-200 range)
  )
  
  result <- yieldcleanr:::detect_and_convert_imperial_units(data)
  
  # Should convert to meters
  expect_true(all(result$Distance < 5))
  expect_true(all(result$Swath < 5))
})

test_that("read_yield_data handles data frame input", {
  data <- tibble::tibble(
    Longitude = c(-69.856661, -69.856681),
    Latitude = c(47.506122, 47.506136),
    Flow = c(50, 55),
    GPS_Time = c(1762958157, 1762958159),
    Interval = c(2, 2),
    Distance = c(87, 87),
    Swath = c(240, 240),
    Moisture = c(15, 16),
    HeaderStatus = c(33, 33),
    Pass = c(1, 1),
    GPSStatus = c(7, 7),
    DOP = c(0, 0),
    Altitude = c(61.3, 61.5)
  )
  
  result <- read_yield_data(data)
  
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 2)
})

test_that("standardize_jd_columns handles data with coords", {
  skip_if_not_installed("sf")
  
  data <- data.frame(
    Flow = c(100, 110, 120),
    Longitude = c(-69.856, -69.857, -69.858),
    Latitude = c(47.506, 47.507, 47.508)
  ) |> sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)
  
  result <- yieldcleanr:::standardize_jd_columns(data)
  
  expect_true("Flow" %in% names(result))
})

test_that("convert_jd_metric_to_yieldcleanr creates Yield_kg_ha", {
  data <- tibble::tibble(
    Flow = c(100, 150, 200),
    Moisture = c(15, 16, 17),
    Yield_kg_ha = c(100, 150, 200)
  )
  
  result <- yieldcleanr:::convert_jd_metric_to_yieldcleanr(data)
  
  expect_true("Yield_kg_ha" %in% names(result))
})

test_that("convert_jd_metric_to_yieldcleanr converts tons to kg", {
  data <- tibble::tibble(
    Flow = c(5, 8, 12),
    Yield_t_ha = c(5, 8, 12)
  )
  
  result <- yieldcleanr:::convert_jd_metric_to_yieldcleanr(data)
  
  expect_true(all(result$Flow > 100))
})

test_that("convert_jd_metric_to_yieldcleanr converts inches to meters", {
  data <- tibble::tibble(
    Swath = c(300, 350, 400)
  )
  
  result <- yieldcleanr:::convert_jd_metric_to_yieldcleanr(data)
  
  expect_true(all(result$Swath < 15))
})
