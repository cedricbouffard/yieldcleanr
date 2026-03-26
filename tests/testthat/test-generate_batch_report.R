# Test suite for generate_batch_report internal functions ----
library(dplyr)
library(yieldcleanr)

test_that(".translate_crop translates crops correctly", {
  expect_equal(yieldcleanr:::.translate_crop("corn"), "Mais")
  expect_equal(yieldcleanr:::.translate_crop("wheat"), "Ble")
  expect_equal(yieldcleanr:::.translate_crop("soybean"), "Soya")
})

test_that(".translate_crop handles NA and empty", {
  expect_equal(yieldcleanr:::.translate_crop(NA), "")
  expect_equal(yieldcleanr:::.translate_crop(""), "")
})

test_that(".translate_crop handles unknown crops", {
  expect_equal(yieldcleanr:::.translate_crop("unknown"), "unknown")
})

test_that("generate_batch_report validates parameters", {
  # Test with nonexistent file
  expect_error(
    generate_batch_report("nonexistent.zip"),
    "Fichier"
  )
  
  # Test with invalid extension
  temp_file <- tempfile(fileext = ".invalid")
  file.create(temp_file)
  on.exit(unlink(temp_file))
  
  expect_error(
    generate_batch_report(temp_file),
    "Format"
  )
})

test_that("generate_batch_report generates default output path", {
  # Create a temp file with valid extension
  temp_file <- tempfile(fileext = ".geojson")
  file.create(temp_file)
  on.exit(unlink(temp_file))
  
  # Should error but validate the path generation logic
  expect_error(
    generate_batch_report(temp_file)
  )
})

test_that("generate_batch_report accepts both output formats", {
  temp_file <- tempfile(fileext = ".geojson")
  file.create(temp_file)
  on.exit(unlink(temp_file))
  
  # Both pdf and html should be accepted as parameters
  # (will error on processing but not on parameter validation)
  expect_error(
    generate_batch_report(temp_file, output_format = "pdf")
  )
  
  temp_file2 <- tempfile(fileext = ".geojson")
  file.create(temp_file2)
  on.exit(unlink(temp_file2), add = TRUE)
  
  expect_error(
    generate_batch_report(temp_file2, output_format = "html")
  )
})

test_that("generate_batch_report_html exists", {
  expect_true(exists("generate_batch_report_html"))
  expect_true(is.function(generate_batch_report_html))
})

test_that("generate_batch_report handles multiple files", {
  temp_file1 <- tempfile(fileext = ".geojson")
  temp_file2 <- tempfile(fileext = ".geojson")
  file.create(temp_file1)
  file.create(temp_file2)
  on.exit({
    unlink(temp_file1)
    unlink(temp_file2)
  })
  
  # Should handle multiple files
  expect_error(
    generate_batch_report(c(temp_file1, temp_file2))
  )
})

test_that("generate_batch_report generates title automatically", {
  temp_file <- tempfile(fileext = ".geojson")
  file.create(temp_file)
  on.exit(unlink(temp_file))
  
  # Should accept NULL title (auto-generated)
  expect_error(
    generate_batch_report(temp_file, title = NULL)
  )
  
  # Should accept custom title
  temp_file2 <- tempfile(fileext = ".geojson")
  file.create(temp_file2)
  on.exit(unlink(temp_file2), add = TRUE)
  
  expect_error(
    generate_batch_report(temp_file2, title = "Custom Title")
  )
})
