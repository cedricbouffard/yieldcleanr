# Test suite for clean_yield_fast.R ----
library(dplyr)
library(yieldcleanr)

test_that("clean_yield_fast function exists", {
  expect_true(exists("clean_yield_fast"))
  expect_true(is.function(clean_yield_fast))
})

test_that("clean_yield_fast validates inputs", {
  expect_error(clean_yield_fast())
})
