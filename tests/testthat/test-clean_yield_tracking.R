# Test suite for clean_yield_tracking.R ----
library(dplyr)
library(yieldcleanr)

test_that("clean_yield_with_tracking function exists", {
  expect_true(exists("clean_yield_with_tracking"))
  expect_true(is.function(clean_yield_with_tracking))
})

test_that("clean_yield_with_tracking validates inputs", {
  expect_error(clean_yield_with_tracking())
})
