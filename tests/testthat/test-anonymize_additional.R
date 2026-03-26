# Additional tests for anonymize_attributes.R ----
library(dplyr)
library(yieldcleanr)

test_that("remove_sensitive_attributes removes specified columns", {
  data <- tibble::tibble(
    Longitude = c(-73.5, -73.51),
    Latitude = c(45.5, 45.51),
    Yield_kg_ha = c(8000, 8500),
    Serial = c("SN001", "SN002"),
    FieldID = c("F001", "F002")
  )
  
  result <- remove_sensitive_attributes(data, columns_to_remove = c("Serial", "FieldID"))
  
  expect_false("Serial" %in% names(result))
  expect_false("FieldID" %in% names(result))
  expect_true("Yield_kg_ha" %in% names(result))
})

test_that("remove_sensitive_attributes keeps default columns", {
  data <- tibble::tibble(
    Longitude = c(-73.5, -73.51),
    Latitude = c(45.5, 45.51),
    Yield_kg_ha = c(8000, 8500),
    Serial = c("SN001", "SN002"),
    FieldID = c("F001", "F002"),
    LoadID = c("L001", "L002")
  )
  
  result <- remove_sensitive_attributes(data)
  
  # Default columns should be removed
  expect_false("Serial" %in% names(result))
  expect_false("FieldID" %in% names(result))
  expect_false("LoadID" %in% names(result))
  expect_true("Yield_kg_ha" %in% names(result))
})

test_that("anonymize_data with type = 'coordinates' requires X,Y columns", {
  data <- tibble::tibble(
    Longitude = c(-73.5, -73.51),
    Latitude = c(45.5, 45.51),
    Yield_kg_ha = c(8000, 8500)
  )
  
  # Should warn but not error when X,Y columns missing
  result <- anonymize_data(data, type = "coordinates", method = "translation")
  
  expect_s3_class(result, "tbl_df")
})

test_that("anonymize_data with type = 'coordinates' works with X,Y", {
  data <- tibble::tibble(
    X = c(100, 200, 300),
    Y = c(100, 200, 300),
    Yield_kg_ha = c(8000, 8500, 9000)
  )
  
  result <- anonymize_data(data, type = "coordinates", method = "translation")
  
  expect_s3_class(result, "tbl_df")
  expect_true("X" %in% names(result))
  expect_true("Y" %in% names(result))
  # Coordinates should be shifted
  expect_false(identical(result$X, data$X))
})

test_that("anonymize_data with method = 'rotation' works", {
  data <- tibble::tibble(
    X = c(100, 200, 300),
    Y = c(100, 200, 300),
    Yield_kg_ha = c(8000, 8500, 9000)
  )
  
  result <- anonymize_data(data, type = "coordinates", method = "rotation")
  
  expect_s3_class(result, "tbl_df")
  expect_true("X" %in% names(result))
})

test_that("anonymize_data with type = 'attributes' removes sensitive columns", {
  data <- tibble::tibble(
    X = c(100, 200),
    Y = c(100, 200),
    Yield_kg_ha = c(8000, 8500),
    OperatorName = c("John", "Jane"),
    FieldName = c("North", "South")
  )
  
  result <- anonymize_data(data, type = "attributes")
  
  expect_s3_class(result, "tbl_df")
  # Sensitive columns should be removed
  expect_false("OperatorName" %in% names(result))
  expect_false("FieldName" %in% names(result))
})

test_that("anonymize_data with type = 'full' does both", {
  data <- tibble::tibble(
    X = c(100, 200),
    Y = c(100, 200),
    Yield_kg_ha = c(8000, 8500),
    OperatorName = c("John", "Jane")
  )
  
  result <- anonymize_data(data, type = "full", method = "translation")
  
  expect_s3_class(result, "tbl_df")
  expect_false("OperatorName" %in% names(result))
})

test_that("anonymize_coordinates function exists", {
  expect_true(exists("anonymize_coordinates"))
  expect_true(is.function(anonymize_coordinates))
})

test_that("restore_coordinates requires key_file or key_info", {
  data <- tibble::tibble(
    Longitude = c(-73.5, -73.51),
    Latitude = c(45.5, 45.51)
  )
  
  expect_error(
    restore_coordinates(data),
    "key_file|key_info"
  )
})
