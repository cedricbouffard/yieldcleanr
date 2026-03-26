# Test suite for clean_yield.R ----
library(dplyr)
library(yieldcleanr)

test_that("clean_yield function exists", {
  expect_true(exists("clean_yield"))
  expect_true(is.function(clean_yield))
})

test_that("clean_yield validates inputs", {
  # Test that it requires either file_path or data
  expect_error(
    clean_yield(),
    "file_path|data"
  )
})
