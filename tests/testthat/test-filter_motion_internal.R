# Test suite for filter_motion.R ----
library(dplyr)
library(yieldcleanr)

test_that("filter_velocity_jumps handles missing columns", {
  data <- tibble::tibble(Flow = 1:5)
  
  result <- yieldcleanr:::filter_velocity_jumps(data)
  
  expect_type(result, "list")
  expect_equal(nrow(result$data), 5)
  expect_equal(nrow(result$removed), 0)
})

test_that("filter_velocity_jumps works with valid data", {
  data <- tibble::tibble(
    X = c(1, 2, 3, 10, 11),
    Y = c(1, 1, 1, 1, 1),
    Interval = c(1, 1, 1, 1, 1),
    Flow = 1:5
  )
  
  result <- yieldcleanr:::filter_velocity_jumps(data)
  
  expect_type(result, "list")
  expect_true("data" %in% names(result))
  expect_true("removed" %in% names(result))
})

test_that("filter_heading_anomalies handles missing columns", {
  data <- tibble::tibble(Flow = 1:5)
  
  result <- yieldcleanr:::filter_heading_anomalies(data)
  
  expect_type(result, "list")
  expect_equal(nrow(result$data), 5)
})

test_that("filter_heading_anomalies works with valid data", {
  data <- tibble::tibble(
    X = c(1, 2, 3, 4, 5),
    Y = c(1, 1, 2, 1, 1),
    GPS_Time = 1:5,
    Flow = 1:5
  )
  
  result <- yieldcleanr:::filter_heading_anomalies(data, max_heading_change = 30)
  
  expect_type(result, "list")
  expect_true("data" %in% names(result))
  expect_true("removed" %in% names(result))
})

test_that("filter_heading_anomalies with straight line", {
  data <- tibble::tibble(
    X = 1:10,
    Y = rep(1, 10),
    GPS_Time = 1:10,
    Flow = 1:10
  )
  
  result <- yieldcleanr:::filter_heading_anomalies(data, max_heading_change = 60)
  
  expect_equal(nrow(result$data), 10)
  expect_equal(nrow(result$removed), 0)
})

test_that("detect_motion_anomalies with report action", {
  data <- tibble::tibble(
    X = c(1, 2, 3, 4, 5),
    Y = c(1, 1, 2, 1, 1),
    GPS_Time = 1:5,
    Interval = rep(1, 5),
    Flow = 1:5
  )
  
  result <- detect_anomalies(data, type = c("velocity_jump", "heading"), action = "report")
  
  expect_type(result, "list")
})

test_that("detect_anomalies velocity_jump filters correctly", {
  data <- tibble::tibble(
    X = c(1, 2, 3, 10, 11),
    Y = c(1, 1, 1, 1, 1),
    GPS_Time = 1:5,
    Interval = rep(1, 5),
    Flow = 1:5
  )
  
  result <- detect_anomalies(data, type = "velocity_jump", action = "filter")
  
  expect_s3_class(result, "tbl_df")
})
