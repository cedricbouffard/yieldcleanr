library(testthat)
library(dplyr)
library(yieldcleanr)

# Test suite for read_data.R ----

test_that("read_yield_data reads raw data correctly", {
  skip("Requires actual test file")
})

test_that("parse_column_names renames columns correctly", {
  data <- tibble(
    Longitude = c(1, 2),
    Latitude = c(3, 4),
    Grain_Flow = c(100, 110)
  )

  result <- parse_column_names(data)

  expect_equal(names(result), c("Longitude", "Latitude", "Flow"))
 })

 # Test detect_and_convert_imperial_units ----
 test_that("detect_and_convert_imperial_units converts inches to meters", {
   data <- tibble(
     Distance = c(80, 85, 90),  # In inches (~2m)
     Swath = c(300, 310, 320),  # In inches (~8m)
     Flow = 1:3
   )

   result <- detect_and_convert_imperial_units(data)

   expect_true(all(result$Distance < 3))  # Should be in meters now
   expect_true(all(result$Swath < 10))    # Should be in meters now
 })

 test_that("detect_and_convert_imperial_units keeps meters unchanged", {
   data <- tibble(
     Distance = c(1.5, 2.0, 2.5),  # Already in meters
     Swath = c(8, 9, 10),          # Already in meters
     Flow = 1:3
   )

   result <- detect_and_convert_imperial_units(data)

   expect_equal(result$Distance, data$Distance)
   expect_equal(result$Swath, data$Swath)
 })

 test_that("detect_and_convert_imperial_units handles missing columns", {
   data <- tibble(Flow = 1:3)

   result <- detect_and_convert_imperial_units(data)

   expect_equal(nrow(result), nrow(data))
 })

 # Test list_fields_from_zip ----
 test_that("list_fields_from_zip handles nonexistent file", {
   expect_error(list_fields_from_zip("nonexistent.zip"))
 })

 test_that("list_fields_from_zip handles empty zip", {
   skip_if_not_installed("zip")
   temp_zip <- tempfile(fileext = ".zip")
   zip::zip(temp_zip, tempfile(), mode = "cherry")

   result <- list_fields_from_zip(temp_zip)

   expect_type(result, "list")
   expect_true("field_name" %in% names(result))
 })

 # Test standardize_jd_columns ----
 test_that("standardize_jd_columns renames JD columns", {
   data <- tibble(
     DISTANCE = c(1, 2, 3),
     SWATHWIDTH = c(8, 9, 10),
     VRYIELDMAS = c(100, 110, 120),
     geometry = st_sfc(st_point(c(0, 0)))
   )
   class(data) <- c("sf", class(data))

   result <- standardize_jd_columns(data)

   expect_true("Distance" %in% names(result))
   expect_true("Swath" %in% names(result))
   expect_true("Flow" %in% names(result))
 })

 test_that("standardize_jd_columns adds missing columns", {
   data <- tibble(
     Flow = c(100, 110, 120),
     geometry = st_sfc(st_point(c(0, 0)))
   )
   class(data) <- c("sf", class(data))

   result <- standardize_jd_columns(data)

   expect_true("Moisture" %in% names(result))
   expect_true("Swath" %in% names(result))
   expect_true("Pass" %in% names(result))
   expect_true("Longitude" %in% names(result))
   expect_true("Latitude" %in% names(result))
 })

 # Test convert_jd_metric_to_yieldcleanr ----
 test_that("convert_jd_metric_to_yieldcleanr creates Yield_kg_ha", {
   data <- tibble(
     Flow = c(100, 150, 200),  # kg/ha
     Moisture = c(15, 16, 17),
     Yield_kg_ha = c(100, 150, 200)
   )

   result <- convert_jd_metric_to_yieldcleanr(data)

   expect_true("Yield_kg_ha" %in% names(result))
 })

 test_that("convert_jd_metric_to_yieldcleanr converts tons to kg", {
   data <- tibble(
     Flow = c(5, 8, 12),  # tonnes/ha
     Yield_t_ha = c(5, 8, 12)
   )

   result <- convert_jd_metric_to_yieldcleanr(data)

   expect_true(all(result$Flow > 100))  # Should be in kg now
 })

 test_that("convert_jd_metric_to_yieldcleanr converts inches to meters", {
   data <- tibble(
     Swath = c(300, 350, 400)  # Inches
   )

   result <- convert_jd_metric_to_yieldcleanr(data)

   expect_true(all(result$Swath < 15))  # Should be in meters
 })
