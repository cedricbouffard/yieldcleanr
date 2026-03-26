# Test suite for output.R ----
library(dplyr)
library(yieldcleanr)

test_that("export_data exists", {
  expect_true(exists("export_data"))
  expect_true(is.function(export_data))
})

test_that("export_data validates format parameter", {
  test_data <- tibble::tibble(
    Longitude = c(-73.5, -73.51),
    Latitude = c(45.5, 45.51),
    Yield_kg_ha = c(8000, 8500)
  )
  
  # Test with invalid format
  expect_error(
    export_data(test_data, "test.invalid", format = "invalid")
  )
})

test_that("export_data handles csv format", {
  test_data <- tibble::tibble(
    Longitude = c(-73.5, -73.51),
    Latitude = c(45.5, 45.51),
    Yield_kg_ha = c(8000, 8500)
  )
  
  temp_file <- tempfile(fileext = ".csv")
  on.exit(unlink(temp_file))
  
  expect_no_error(
    export_data(test_data, temp_file, format = "csv")
  )
  
  expect_true(file.exists(temp_file))
})
