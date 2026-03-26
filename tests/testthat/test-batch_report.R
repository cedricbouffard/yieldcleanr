# Test suite for generate_batch_report.R ----
library(dplyr)
library(yieldcleanr)

test_that("generate_batch_report handles nonexistent files", {
  expect_error(
    generate_batch_report("nonexistent.zip"),
    "Fichier"
  )
})

test_that("generate_batch_report validates file extensions", {
  # Create a temp file with invalid extension
  temp_file <- tempfile(fileext = ".invalid")
  file.create(temp_file)
  on.exit(unlink(temp_file))
  
  expect_error(
    generate_batch_report(temp_file),
    "Format"
  )
})

test_that("generate_batch_report_html exists and is exported", {
  expect_true(exists("generate_batch_report_html"))
  expect_true(is.function(generate_batch_report_html))
})

# Skip tests that require zip functionality on CI
# These tests may fail on different platforms due to zip utility differences
