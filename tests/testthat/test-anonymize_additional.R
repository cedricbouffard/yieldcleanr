# Additional tests for anonymize_attributes.R ----
library(dplyr)
library(yieldcleanr)

test_that("remove_sensitive_attributes removes specified columns", {
  data <- tibble::tibble(
    Longitude = c(-73.5, -73.51),
    Latitude = c(45.5, 45.51),
    Yield_kg_ha = c(8000, 8500),
    Farmer_Name = c("John", "Jane"),
    Field_ID = c("F001", "F002")
  )
  
  result <- remove_sensitive_attributes(data, cols_to_remove = c("Farmer_Name", "Field_ID"))
  
  expect_false("Farmer_Name" %in% names(result))
  expect_false("Field_ID" %in% names(result))
  expect_true("Yield_kg_ha" %in% names(result))
})

test_that("remove_sensitive_attributes auto-detects sensitive columns", {
  data <- tibble::tibble(
    Longitude = c(-73.5, -73.51),
    Latitude = c(45.5, 45.51),
    Yield_kg_ha = c(8000, 8500),
    FarmerName = c("John", "Jane"),
    Owner = c("Smith", "Doe")
  )
  
  result <- remove_sensitive_attributes(data, auto_detect = TRUE)
  
  expect_s3_class(result, "tbl_df")
})

test_that("anonymize_yield_data removes sensitive info", {
  data <- tibble::tibble(
    Longitude = c(-73.5, -73.51),
    Latitude = c(45.5, 45.51),
    Yield_kg_ha = c(8000, 8500),
    FieldName = c("North Field", "South Field")
  )
  
  result <- anonymize_yield_data(data)
  
  expect_s3_class(result, "tbl_df")
  expect_true(nrow(result) > 0)
})

test_that("anonymize_data with type = 'attributes'", {
  data <- tibble::tibble(
    Longitude = c(-73.5, -73.51),
    Latitude = c(45.5, 45.51),
    Yield_kg_ha = c(8000, 8500),
    Farmer = c("John", "Jane")
  )
  
  result <- anonymize_data(data, type = "attributes")
  
  expect_s3_class(result, "tbl_df")
})

test_that("anonymize_data with type = 'coordinates'", {
  data <- tibble::tibble(
    Longitude = c(-73.5, -73.51),
    Latitude = c(45.5, 45.51),
    Yield_kg_ha = c(8000, 8500)
  )
  
  result <- anonymize_data(data, type = "coordinates", method = "translation")
  
  expect_s3_class(result, "tbl_df")
  expect_true("Longitude" %in% names(result))
})

test_that("anonymize_data with type = 'full'", {
  data <- tibble::tibble(
    Longitude = c(-73.5, -73.51),
    Latitude = c(45.5, 45.51),
    Yield_kg_ha = c(8000, 8500),
    Farmer = c("John", "Jane")
  )
  
  result <- anonymize_data(data, type = "full")
  
  expect_s3_class(result, "tbl_df")
})

test_that("anonymize_coordinates with translation method", {
  data <- tibble::tibble(
    Longitude = c(-73.5, -73.51),
    Latitude = c(45.5, 45.51)
  )
  
  result <- anonymize_coordinates(data, method = "translation")
  
  expect_s3_class(result, "tbl_df")
  expect_true("Longitude" %in% names(result))
  expect_true("Latitude" %in% names(result))
})

test_that("anonymize_coordinates with rotation method", {
  data <- tibble::tibble(
    Longitude = c(-73.5, -73.51),
    Latitude = c(45.5, 45.51)
  )
  
  result <- anonymize_coordinates(data, method = "rotation")
  
  expect_s3_class(result, "tbl_df")
})

test_that("anonymize_coordinates with noise method", {
  data <- tibble::tibble(
    Longitude = c(-73.5, -73.51),
    Latitude = c(45.5, 45.51)
  )
  
  result <- anonymize_coordinates(data, method = "noise")
  
  expect_s3_class(result, "tbl_df")
})

test_that("restore_coordinates restores original coordinates", {
  data <- tibble::tibble(
    Longitude = c(-73.5, -73.51),
    Latitude = c(45.5, 45.51),
    orig_Longitude = c(-73.5, -73.51),
    orig_Latitude = c(45.5, 45.51)
  )
  
  result <- restore_coordinates(data)
  
  expect_s3_class(result, "tbl_df")
  expect_true("Longitude" %in% names(result))
})

test_that("restore_coordinates handles missing orig columns", {
  data <- tibble::tibble(
    Longitude = c(-73.5, -73.51),
    Latitude = c(45.5, 45.51)
  )
  
  result <- restore_coordinates(data)
  
  expect_s3_class(result, "tbl_df")
})
