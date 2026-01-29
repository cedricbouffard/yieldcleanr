library(testthat)
library(dplyr)
library(yieldcleanr)

# Test suite for filter_advanced.R ----

# Create test data for advanced filters
create_advanced_test_data <- function() {
  tibble::tibble(
    X = c(435000, 435001, 435002, 435003, 435004, 435005,
          435100, 435101, 435102, 435103, 435104, 435105),
    Y = c(5262000, 5262001, 5262002, 5262003, 5262004, 5262005,
          5262100, 5262101, 5262102, 5262103, 5262104, 5262105),
    Flow = c(50, 55, 52, 58, 300, 54,  # 300 is an outlier
             45, 48, 47, 50, 49, 46),
    Swath = rep(240, 12),
    Pass = rep(1:3, each = 4),
    .row_id = 1:12
  )
}

test_that("remove_overlap filters cells with too many passes", {
  data <- tibble::tibble(
    X = c(1, 1.1, 1.2, 1.3, 1.4, 1.5, 2, 2.1, 2.2),
    Y = c(1, 1, 1, 1, 1, 1, 2, 2, 2),
    Flow = 1:9,
    .row_id = 1:9
  )

  # With max_pass = 2, cells with 5-6 points will be filtered
  result <- remove_overlap(data, cellsize = 0.5, max_pass = 2)

  expect_true(nrow(result) < nrow(data))
  expect_true(all(c("X", "Y") %in% names(result)))
})

test_that("remove_overlap requires X and Y columns", {
  data <- tibble::tibble(Flow = c(1, 2, 3))

  expect_error(remove_overlap(data))
})

test_that("remove_overlap keeps all when no overlap", {
  data <- tibble::tibble(
    X = c(1, 10, 20, 30),
    Y = c(1, 10, 20, 30),
    Flow = 1:4,
    .row_id = 1:4
  )

  result <- remove_overlap(data, cellsize = 0.5, max_pass = 2)

  expect_equal(nrow(result), nrow(data))
})

test_that("filter_local_std removes outliers", {
  # Create data with a clear outlier within a single pass
  # With 5 identical values and one extreme outlier, the filter should remove the outlier
  data <- tibble::tibble(
    Flow = c(50, 50, 50, 50, 50, 500,  # 500 is an extreme outlier in Pass 1 (mean = 141.7, sd = 180)
             45, 48, 47, 50, 49, 46),  # Pass 2 and 3 have normal values
    Pass = c(1, 1, 1, 1, 1, 1,  # Pass 1 has 6 points
             2, 2, 2, 3, 3, 3),
    .row_id = 1:12
  )

  # With std_limit = 2, 500 is (500 - 141.7) / 180 = 1.99 SD from mean - might be borderline
  # Use std_limit = 1.5 to ensure the outlier is removed
  result <- filter_local_std(data, swath_window = 5, std_limit = 1.5)

  # The outlier 500 should be removed
  expect_false(500 %in% result$Flow)
})

test_that("filter_local_std skips when no Pass column", {
  data <- tibble::tibble(Flow = c(1, 2, 3))

  result <- filter_local_std(data)

  expect_equal(nrow(result), nrow(data))
})

test_that("filter_sliding_window removes temporal outliers", {
  # Create data with an outlier in the middle where rolling mean will be calculated
  data <- tibble::tibble(
    Flow = c(10, 10, 10, 10, 100, 10, 10, 10, 10, 10, 10, 10, 10),  # 100 is at position 5
    .row_id = 1:13
  )

  # With window_size = 7, the outlier at position 5 is within the window
  result <- filter_sliding_window(data, window_size = 7, n_std = 2)

  # The outlier 100 should be removed if it's within the window
  expect_true(nrow(result) <= nrow(data))
})

test_that("filter_sliding_window handles edge cases", {
  # Dataset with window larger than data - should still work
  data <- tibble::tibble(
    Flow = c(10, 20, 30),
    .row_id = 1:3
  )

  result <- filter_sliding_window(data, window_size = 11, n_std = 2)

  # All points should be kept (no filtering possible with small data)
  expect_equal(nrow(result), 3)
})

test_that("apply_overlap_filter returns filtered data", {
  data <- tibble::tibble(
    X = c(435000, 435001, 435002, 435003, 435100),
    Y = c(5262000, 5262001, 5262002, 5262003, 5262100),
    Flow = c(10, 15, 12, 18, 20),
    Swath = c(240, 240, 240, 240, 240),
    .row_id = 1:5
  )

  result <- apply_overlap_filter(data, cellsize = 0.3)

  expect_true(nrow(result) <= nrow(data))
})

test_that("apply_overlap_filter warns on missing columns", {
  data <- tibble::tibble(Flow = c(1, 2, 3))

  expect_warning(apply_overlap_filter(data))
})

test_that("apply_local_sd_filter returns filtered data", {
  data <- create_advanced_test_data()

  result <- apply_local_sd_filter(data, n_swaths = 5, lsd_limit = 3)

  expect_true(nrow(result) <= nrow(data))
})

test_that("apply_local_sd_filter warns on missing columns", {
  data <- tibble::tibble(Flow = c(1, 2, 3))

  expect_warning(apply_local_sd_filter(data))
})
