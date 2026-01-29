# Test helper-test_data.R ----

test_that("generation of test data works", {
  # Verify test data objects exist
  expect_true(exists("yield_test_data"))
  expect_true(exists("yield_test_data_header_mixed"))
  expect_true(exists("yield_test_data_outliers"))
  expect_true(exists("yield_test_data_overlap"))

  # Verify structure
  expect_equal(nrow(yield_test_data), 5)
  expect_equal(nrow(yield_test_data_header_mixed), 5)  # 5 rows with mixed HeaderStatus
  expect_equal(nrow(yield_test_data_outliers), 5)
  expect_equal(nrow(yield_test_data_overlap), 5)

  # Verify required columns exist
  expect_true(all(c("Longitude", "Latitude", "Flow") %in% names(yield_test_data)))
  expect_true("HeaderStatus" %in% names(yield_test_data_header_mixed))
})

test_that("test data has valid GPS coordinates", {
  # Check Latitude is in valid range
  expect_true(all(yield_test_data$Latitude > 0))
  expect_true(all(yield_test_data$Latitude < 90))

  # Check Longitude is in valid range
  expect_true(all(abs(yield_test_data$Longitude) <= 180))
})

test_that("test data has valid Flow values", {
  # Flow should be positive for yield_test_data
  expect_true(all(yield_test_data$Flow > 0))

  # yield_test_data_outliers has one extreme value
  expect_equal(yield_test_data_outliers$Flow[4], 600)  # The outlier
})
