library(testthat)
library(dplyr)
library(yieldcleanr)

# Test suite for unit_conversion.R ----

# Test convert_flow_units (internal - use :::) ----
test_that("convert_flow_units converts flow correctly", {
  data <- tibble(Flow = c(56, 112, 168))  # 56, 112, 168 lbs/sec

  result <- yieldcleanr:::convert_flow_units(data, density = 56, scale_factor = 1)

  expect_equal(result$Flow, c(1, 2, 3))  # 56/56 = 1, 112/56 = 2, etc.
})

test_that("convert_flow_units applies scale factor", {
  data <- tibble(Flow = c(56, 112))

  result <- yieldcleanr:::convert_flow_units(data, density = 56, scale_factor = 100)

  expect_equal(result$Flow, c(100, 200))
})

test_that("convert_flow_units handles missing Flow column", {
  data <- tibble(X = 1:3)

  result <- yieldcleanr:::convert_flow_units(data)

  expect_equal(nrow(result), nrow(data))
})

# Test convert_to_yield (internal - use :::) ----
test_that("convert_to_yield calculates yield", {
  data <- tibble(
    Flow = c(56, 112, 168),  # lbs/sec
    Swath = c(8, 8, 8),       # meters (already converted from inches)
    velocity = c(2, 2, 2)      # m/s
  )

  result <- yieldcleanr:::convert_to_yield(data, density = 56)

  expect_true("Flow_yield" %in% names(result))
  expect_true(all(result$Flow_yield > 0))
})

test_that("convert_to_yield handles missing Flow column", {
  data <- tibble(Swath = 8)

  result <- yieldcleanr:::convert_to_yield(data)

  expect_equal(nrow(result), nrow(data))
})

test_that("convert_to_yield calculates velocity when missing", {
  data <- tibble(
    X = c(0, 10, 20, 30),
    Y = c(0, 0, 0, 0),
    Interval = rep(2, 4),
    Swath = rep(8, 4),
    Flow = rep(56, 4)
  )

  result <- yieldcleanr:::convert_to_yield(data)

  expect_true("Flow_yield" %in% names(result))
})

test_that("convert_to_yield uses simplified conversion without velocity", {
  data <- tibble(
    Flow = c(56, 112),
    Swath = c(8, 8)  # No velocity column
  )

  result <- yieldcleanr:::convert_to_yield(data)

  expect_true("Flow_yield" %in% names(result))
})