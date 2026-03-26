# Test suite for filter_yield.R ----
library(dplyr)
library(yieldcleanr)

test_that("calculate_filter_counts exists", {
  expect_true(exists("calculate_filter_counts"))
  expect_true(is.function(calculate_filter_counts))
})

test_that("calculate_thresholds works", {
  test_data <- tibble::tibble(
    Yield_kg_ha = c(8000, 8500, 9000, 8200, 8800),
    Velocity = c(5, 5.5, 6, 5.2, 5.8)
  )
  
  thresholds <- calculate_thresholds(test_data, type = "all")
  
  expect_type(thresholds, "list")
  expect_true(length(thresholds) > 0)
})

test_that("calculate_auto_thresholds works", {
  test_data <- tibble::tibble(
    Yield_kg_ha = c(8000, 8500, 9000, 8200, 8800),
    Velocity = c(5, 5.5, 6, 5.2, 5.8)
  )
  
  thresholds <- calculate_auto_thresholds(test_data)
  
  expect_type(thresholds, "list")
  expect_true(length(thresholds) > 0)
})

test_that("apply_moisture_delay exists", {
  expect_true(exists("apply_moisture_delay"))
  expect_true(is.function(apply_moisture_delay))
})

test_that("optimize_delays exists", {
  expect_true(exists("optimize_delays"))
  expect_true(is.function(optimize_delays))
})
