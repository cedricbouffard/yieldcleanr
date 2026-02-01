# Test suite for detect_anomalies - méta-fonction de détection d'anomalies ----
library(dplyr)
library(yieldcleanr)

# Create test data for anomaly detection
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

# Test detect_anomalies with overlap detection ----
test_that("detect_anomalies detects overlapping cells", {
  data <- tibble::tibble(
    X = c(1, 1.1, 1.2, 1.3, 1.4, 1.5, 2, 2.1, 2.2),
    Y = c(1, 1, 1, 1, 1, 1, 2, 2, 2),
    Flow = 1:9,
    .row_id = 1:9
  )

  # With max_pass = 2, cells with 5-6 points will be filtered
  result <- detect_anomalies(data, type = "overlap", cellsize = 0.5, max_pass = 2)

  expect_true(nrow(result) < nrow(data))
  expect_true(all(c("X", "Y") %in% names(result)))
})

test_that("detect_anomalies requires X and Y columns for overlap", {
  data <- tibble::tibble(Flow = c(1, 2, 3))

  # Should return data unchanged when columns missing
  result <- detect_anomalies(data, type = "overlap")
  
  expect_equal(nrow(result), nrow(data))
})

test_that("detect_anomalies keeps all when no overlap detected", {
  data <- tibble::tibble(
    X = c(1, 10, 20, 30),
    Y = c(1, 10, 20, 30),
    Flow = 1:4,
    .row_id = 1:4
  )

  result <- detect_anomalies(data, type = "overlap", cellsize = 0.5, max_pass = 2)

  expect_equal(nrow(result), nrow(data))
})

# Test detect_anomalies with local SD detection ----
test_that("detect_anomalies removes local SD outliers", {
  # Create data with a clear outlier within a single pass
  # With 5 identical values and one extreme outlier, the filter should remove the outlier
  data <- tibble::tibble(
    X = c(435000, 435001, 435002, 435003, 435004, 435005,
          435100, 435101, 435102, 435103, 435104, 435105),
    Y = c(5262000, 5262001, 5262002, 5262003, 5262004, 5262005,
          5262100, 5262101, 5262102, 5262103, 5262104, 5262105),
    Flow = c(50, 55, 52, 58, 300, 54,  # 300 is an extreme outlier
             45, 48, 47, 50, 49, 46),
    Swath = rep(240, 12)
  )

  # With lsd_limit = 2.4, the outlier 300 should be detected and removed
  result <- detect_anomalies(data, type = "local_sd", n_swaths = 5, lsd_limit = 2.4)

  # The outlier 300 should be removed
  expect_false(300 %in% result$Flow)
})

test_that("detect_anomalies skips local_sd when required columns missing", {
  data <- tibble::tibble(Flow = c(1, 2, 3))

  result <- detect_anomalies(data, type = "local_sd")

  expect_equal(nrow(result), nrow(data))
})

# Test detect_anomalies with velocity jump detection ----
test_that("detect_anomalies detects velocity jumps", {
  # Create data with sudden velocity changes
  data <- tibble::tibble(
    X = c(435000, 435010, 435020, 435100, 435110),
    Y = c(5262000, 5262010, 5262020, 5262100, 5262110),
    Interval = c(2, 2, 2, 2, 2),
    Flow = 1:5,
    GPS_Time = 1:5
  )

  result <- detect_anomalies(data, type = "velocity_jump", max_acceleration = 5)

  expect_true(nrow(result) <= nrow(data))
})

test_that("detect_anomalies handles edge cases with velocity jumps", {
  # Small dataset - should keep all points
  data <- tibble::tibble(
    X = c(435000, 435010, 435020),
    Y = c(5262000, 5262010, 5262020),
    Interval = c(2, 2, 2),
    Flow = 1:3,
    GPS_Time = 1:3
  )

  result <- detect_anomalies(data, type = "velocity_jump", max_acceleration = 5)

  # All points should be kept with small dataset
  expect_equal(nrow(result), 3)
})

# Test detect_anomalies with heading anomaly detection ----
test_that("detect_anomalies detects heading anomalies", {
  data <- tibble::tibble(
    X = c(435000, 435010, 435020, 435030, 435040),
    Y = c(5262000, 5262010, 5262020, 5262030, 5262040),
    Flow = 1:5,
    GPS_Time = 1:5
  )

  result <- detect_anomalies(data, type = "heading", max_heading_change = 60)

  expect_true(is.data.frame(result))
  expect_true(nrow(result) <= nrow(data))
})

# Test detect_anomalies with position outlier detection ----
test_that("detect_anomalies detects position outliers", {
  data <- create_advanced_test_data()

  result <- detect_anomalies(data, type = "position", gbuffer = 100)

  expect_true(is.data.frame(result))
  expect_true(nrow(result) <= nrow(data))
})

# Test detect_anomalies with action parameter ----
test_that("detect_anomalies with action = 'detect' marks without filtering", {
  data <- tibble::tibble(
    X = c(435000, 435001, 435002, 435003, 435004, 435005,
          435100, 435101, 435102, 435103, 435104, 435105),
    Y = c(5262000, 5262001, 5262002, 5262003, 5262004, 5262005,
          5262100, 5262101, 5262102, 5262103, 5262104, 5262105),
    Flow = c(50, 55, 52, 58, 300, 54,
             45, 48, 47, 50, 49, 46),
    Swath = rep(240, 12)
  )

  result <- detect_anomalies(data, type = "local_sd", action = "detect")

  # Should keep all rows but add flag column
  expect_equal(nrow(result), nrow(data))
  expect_true("local_sd_outlier" %in% names(result))
})

test_that("detect_anomalies with type = 'all' detects all anomaly types", {
  data <- create_advanced_test_data()

  result <- detect_anomalies(data, type = "all")

  expect_true(is.data.frame(result))
})

# Test detect_anomalies with multiple types ----
test_that("detect_anomalies applies multiple anomaly filters", {
  data <- create_advanced_test_data()
  data$GPS_Time <- 1:nrow(data)
  
  result <- detect_anomalies(data, type = c("overlap", "local_sd"))

  expect_true(nrow(result) <= nrow(data))
})

# Test edge cases ----
test_that("detect_anomalies handles empty data", {
  data <- tibble::tibble(
    X = numeric(),
    Y = numeric(),
    Flow = numeric()
  )

  result <- detect_anomalies(data, type = "overlap")

  expect_equal(nrow(result), 0)
})

test_that("detect_anomalies handles data without required columns gracefully", {
  data <- tibble::tibble(Flow = 1:5)

  result <- detect_anomalies(data, type = "all")

  # Should return data unchanged
  expect_equal(nrow(result), nrow(data))
})

test_that("detect_anomalies handles NA values gracefully", {
  data <- tibble::tibble(
    X = c(435000, 435010, NA, 435030, 435040),
    Y = c(5262000, 5262010, NA, 5262030, 5262040),
    Flow = 1:5,
    Swath = rep(240, 5)
  )

  result <- detect_anomalies(data, type = "local_sd")

  expect_true(is.data.frame(result))
})
