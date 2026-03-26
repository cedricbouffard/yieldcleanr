# Test suite for visualization.R ----
library(dplyr)
library(yieldcleanr)

test_that("geom_yield_map_polygon exists", {
  expect_true(exists("geom_yield_map_polygon"))
  expect_true(is.function(geom_yield_map_polygon))
})

test_that("geom_yield_map_polygon requires yield column", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  
  # Create minimal sf data without yield column
  test_data <- tibble::tibble(
    other_col = c(1, 2, 3),
    geometry = sf::st_sfc(
      sf::st_point(c(-73.5, 45.5)),
      sf::st_point(c(-73.51, 45.51)),
      sf::st_point(c(-73.52, 45.52))
    )
  ) |> sf::st_as_sf()
  
  # Should error without yield column
  expect_error(
    geom_yield_map_polygon(test_data),
    "yield"
  )
})

test_that("geom_yield_map_polygon works with yield column", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  
  # Create minimal sf data with yield column
  test_data <- tibble::tibble(
    yield = c(8000, 8500, 9000),
    geometry = sf::st_sfc(
      sf::st_point(c(-73.5, 45.5)),
      sf::st_point(c(-73.51, 45.51)),
      sf::st_point(c(-73.52, 45.52))
    )
  ) |> sf::st_as_sf()
  
  # Should not error with yield column
  expect_no_error(
    geom_yield_map_polygon(test_data)
  )
})
