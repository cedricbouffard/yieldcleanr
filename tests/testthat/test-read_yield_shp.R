# Tests for read_yield_shp ----
library(dplyr)
library(yieldcleanr)

# Build a synthetic John Deere shapefile + Deere-Metadata.json in a temp dir.
# Note: the ESRI Shapefile driver abbreviates field names (10-char limit), so
# exact-name assertions (unit conversion, Velocity mapping) are done separately
# on in-memory objects below.
make_jd_shp_dir <- function() {
  skip_if_not_installed("sf")
  skip_if_not_installed("jsonlite")

  dir <- tempfile("jd_shp_")
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)

  lng <- c(-73.34903, -73.34905, -73.34907)
  lat <- c(45.19036, 45.19038, 45.19040)

  attrs <- data.frame(
    DISTANCE = c(3.0, 3.0, 3.0),
    SWATHWIDTH = c(6.66, 6.66, 6.66),
    SECTIONID = c(1L, 1L, 1L),
    Crop = c(126L, 126L, 126L),
    Time = c("9/9/2025 2:26:15 PM", "9/9/2025 2:27:13 PM", "9/9/2025 2:28:10 PM"),
    Heading = c(230, 231, 232),
    VARIETY = c("---", "---", "---"),
    Elevation = c(170, 171, 172),
    IsoTime = c("2025-09-09T14:26:15.096Z", "2025-09-09T14:27:13.092Z", "2025-09-09T14:28:12.000Z"),
    Machine = c(1L, 1L, 1L),
    FUEL = c(0.0, 0.25, 0.5),
    VEHICLSPEED = c(0.06, 0.37, 0.40),
    PRODUCTHASH = c("3e2d9c87", "3e2d9c87", "3e2d9c87"),
    VRYIELDMAS = c(20000, 22000, 21000),
    WetMass = c(1000, 1100, 1050),
    x = lng,
    y = lat,
    stringsAsFactors = FALSE
  )

  sf_obj <- sf::st_as_sf(attrs, coords = c("x", "y"), crs = 4326)
  suppressWarnings(sf::st_write(sf_obj, file.path(dir, "test_field.shp"), quiet = TRUE))

  meta <- list(
    Version = "1.0",
    ClientName = "Delfland",
    FarmName = "Delfland",
    FieldName = "130",
    Operation = "Harvest",
    CropSeason = 2025,
    CropId = 126,
    CropToken = "SHALLOTS",
    CropName = "Shallots",
    FieldOperationStartDate = "2025-09-09T14:26:15.096Z",
    DataAttributes = data.frame(
      Name = c("DISTANCE", "SWATHWIDTH", "SECTIONID", "Crop", "Elevation",
               "FUEL", "VEHICLSPEED", "VRYIELDMAS", "WetMass"),
      Unit = c("ft", "ft", NA, NA, "ft", "l", "mi1hr-1", "kg1ha-1", NA),
      stringsAsFactors = FALSE
    )
  )
  jsonlite::write_json(
    meta,
    file.path(dir, "test_field-Deere-Metadata.json"),
    auto_unbox = FALSE,
    pretty = TRUE,
    null = "null"
  )

  return(dir)
}

test_that("read_yield_shp handles nonexistent path", {
  expect_error(read_yield_shp("nonexistent_dir_or_shp"))
})

test_that("read_yield_shp reads from a directory and auto-discovers metadata", {
  dir <- make_jd_shp_dir()
  on.exit(unlink(dir, recursive = TRUE))

  data <- read_yield_shp(dir)

  expect_s3_class(data, "data.frame")
  expect_equal(nrow(data), 3)
  expect_true(all(c("Longitude", "Latitude", "Distance", "Swath", "Flow") %in% names(data)))

  # Colonne Crop (numerique 126) remplacee par le nom de culture du JSON
  expect_true(all(data$GrainType == "Shallots"))

  # Metadonnees conservees en attribut
  expect_true(!is.null(attr(data, "jd_metadata")))
})

test_that("read_yield_shp maps VRYIELDMAS to Flow", {
  dir <- make_jd_shp_dir()
  on.exit(unlink(dir, recursive = TRUE))

  data <- read_yield_shp(dir)

  expect_true(all(data$Flow == c(20000, 22000, 21000)))
})

test_that("read_yield_shp as_polygons returns sf", {
  skip_if_not_installed("sf")

  dir <- make_jd_shp_dir()
  on.exit(unlink(dir, recursive = TRUE))

  sf_data <- read_yield_shp(dir, as_polygons = TRUE)

  expect_s3_class(sf_data, "sf")
  expect_true(all(c("Swath_m", "Distance_m") %in% names(sf_data)))
  expect_equal(nrow(sf_data), 3)
})

test_that("read_yield_shp accepts a direct .shp path", {
  dir <- make_jd_shp_dir()
  on.exit(unlink(dir, recursive = TRUE))

  shp_path <- file.path(dir, "test_field.shp")
  data <- read_yield_shp(shp_path)

  expect_s3_class(data, "data.frame")
  expect_equal(nrow(data), 3)
})

# Unit conversion is tested on in-memory objects with exact John Deere field
# names, since the ESRI Shapefile driver abbreviates names on write.
test_that("convert_units_from_json converts imperial units to metric", {
  skip_if_not_installed("sf")

  sf_obj <- sf::st_as_sf(
    data.frame(
      DISTANCE = c(3, 3, 3),
      SWATHWIDTH = c(6.66, 6.66, 6.66),
      VEHICLSPEED = c(10, 10, 10),
      Longitude = c(-73.3, -73.2, -73.1),
      Latitude = c(45.1, 45.2, 45.3),
      stringsAsFactors = FALSE
    ),
    coords = c("Longitude", "Latitude"),
    crs = 4326
  )

  metadata <- list(units = list(
    DISTANCE = "ft",
    SWATHWIDTH = "ft",
    VEHICLSPEED = "mi1hr-1"
  ))

  result <- yieldcleanr:::convert_units_from_json(sf_obj, metadata)

  expect_true(all(abs(result$DISTANCE - 0.9144) < 1e-6))
  expect_true(all(abs(result$SWATHWIDTH - 2.029968) < 1e-6))
  expect_true(all(abs(result$VEHICLSPEED - 4.4704) < 1e-6))
  expect_true("DISTANCE_m" %in% names(result))
  expect_true("SWATHWIDTH_m" %in% names(result))
})
