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
