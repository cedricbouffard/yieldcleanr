# Test suite for filter_advanced internal functions ----
library(dplyr)
library(yieldcleanr)

test_that("remove_overlap handles missing columns", {
  data <- tibble::tibble(Flow = 1:5)
  
  expect_error(
    yieldcleanr:::remove_overlap(data),
    "X et Y"
  )
})

test_that("remove_overlap removes overlapping points", {
  # Create data with overlap in same cell
  data <- tibble::tibble(
    X = c(1, 1.1, 1.2, 10, 10.1),  # First 3 in same cell, last 2 in another
    Y = c(1, 1.1, 1.2, 10, 10.1),
    Flow = 1:5
  )
  
  result <- yieldcleanr:::remove_overlap(data, cellsize = 1, max_pass = 2)
  
  expect_s3_class(result, "tbl_df")
  expect_lte(nrow(result), nrow(data))
  expect_false("cell_id" %in% names(result))
})

test_that("remove_overlap keeps points when no overlap", {
  data <- tibble::tibble(
    X = c(1, 10, 20, 30),  # Far apart, different cells
    Y = c(1, 10, 20, 30),
    Flow = 1:4
  )
  
  result <- yieldcleanr:::remove_overlap(data, cellsize = 1, max_pass = 10)
  
  expect_equal(nrow(result), 4)
})

test_that("remove_overlap with different cellsize", {
  data <- tibble::tibble(
    X = c(1, 1.1, 2),
    Y = c(1, 1.1, 2),
    Flow = 1:3
  )
  
  # With small cellsize, all points are in different cells
  result_small <- yieldcleanr:::remove_overlap(data, cellsize = 0.1, max_pass = 2)
  expect_equal(nrow(result_small), 3)
  
  # With large cellsize, all points are in same cell
  result_large <- yieldcleanr:::remove_overlap(data, cellsize = 10, max_pass = 2)
  expect_lte(nrow(result_large), 3)
})

test_that("filter_local_std handles missing Pass column", {
  data <- tibble::tibble(Flow = c(50, 55, 60, 200, 52))
  
  result <- yieldcleanr:::filter_local_std(data, swath_window = 5, std_limit = 2)
  
  expect_equal(nrow(result), 5)  # Should return unchanged
})

test_that("filter_local_std filters local outliers", {
  data <- tibble::tibble(
    Pass = c(1, 1, 1, 1, 1, 2, 2, 2, 2, 2),
    Flow = c(50, 51, 52, 53, 200, 45, 46, 47, 48, 49),  # 200 is outlier
    .row_id = 1:10
  )
  
  result <- yieldcleanr:::filter_local_std(data, swath_window = 1, std_limit = 2)
  
  expect_s3_class(result, "tbl_df")
  expect_false("is_outlier" %in% names(result))
  expect_false("upper_limit" %in% names(result))
})

test_that("detect_anomalies calls remove_overlap correctly", {
  data <- tibble::tibble(
    X = c(1, 1.1, 1.2, 10),
    Y = c(1, 1.1, 1.2, 10),
    Flow = 1:4
  )
  
  result <- detect_anomalies(data, type = "overlap", cellsize = 1, max_pass = 2, action = "filter")
  
  expect_s3_class(result, "tbl_df")
})

test_that("detect_anomalies calls filter_local_std correctly", {
  data <- tibble::tibble(
    Pass = c(1, 1, 1, 1, 1),
    Flow = c(50, 51, 52, 200, 53),  # 200 is outlier
    .row_id = 1:5,
    X = 1:5,
    Y = 1:5
  )
  
  result <- detect_anomalies(data, type = "local_sd", n_swaths = 2, lsd_limit = 2, action = "filter")
  
  expect_s3_class(result, "tbl_df")
})

test_that("detect_anomalies handles all action types with overlap", {
  data <- tibble::tibble(
    X = c(1, 1.1, 1.2),
    Y = c(1, 1.1, 1.2),
    Flow = 1:3
  )
  
  # Filter action
  result_filter <- detect_anomalies(data, type = "overlap", cellsize = 1, max_pass = 2, action = "filter")
  expect_s3_class(result_filter, "tbl_df")
  
  # Detect action
  result_detect <- detect_anomalies(data, type = "overlap", cellsize = 1, max_pass = 2, action = "detect")
  expect_s3_class(result_detect, "tbl_df")
  
  # Report action
  result_report <- detect_anomalies(data, type = "overlap", cellsize = 1, max_pass = 2, action = "report")
  expect_type(result_report, "list")
})

test_that("detect_anomalies handles empty data", {
  data <- tibble::tibble(
    X = numeric(),
    Y = numeric(),
    Flow = numeric()
  )
  
  result <- detect_anomalies(data, type = "overlap", action = "filter")
  expect_equal(nrow(result), 0)
})
