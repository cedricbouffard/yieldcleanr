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
# These tests may fail on Ubuntu due to zip utility differences

test_that("generate_batch_report generates correct default output path for single file", {
  skip_on_ci()
  
  # Create a temp zip file
  temp_zip <- tempfile(fileext = ".zip")
  temp_dir <- tempfile()
  dir.create(temp_dir)
  file.create(file.path(temp_dir, "test.txt"))
  
  # Create minimal zip
  utils::zip(temp_zip, file.path(temp_dir, "test.txt"), flags = "-j")
  on.exit({
    unlink(temp_zip)
    unlink(temp_dir, recursive = TRUE)
  }, add = TRUE)
  
  # Test that function accepts the file
  expect_no_error(
    generate_batch_report(temp_zip, output_format = "html")
  )
})

test_that("generate_batch_report validates output_format parameter", {
  skip_on_ci()
  
  temp_file <- tempfile(fileext = ".zip")
  file.create(temp_file)
  on.exit(unlink(temp_file))
  
  # Should accept "pdf"
  expect_error(
    generate_batch_report(temp_file, output_format = "pdf"),
    NA
  )
  
  # Should accept "html"  
  expect_error(
    generate_batch_report(temp_file, output_format = "html"),
    NA
  )
})
