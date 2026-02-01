# yieldcleanr

[![R-CI](https://github.com/cedricbouffard/yieldcleanr/actions/workflows/R-CI.yml/badge.svg)](https://github.com/cedricbouffard/yieldcleanr/actions)
[![Codecov](https://codecov.io/gh/cedricbouffard/yieldcleanr/branch/main/graph/badge.svg)](https://codecov.io/gh/cedricbouffard/yieldcleanr)

**yieldcleanr** is an R package for cleaning, filtering, and validating agricultural yield data collected by combine harvesters. It implements filtering methods documented in the USDA Yield Editor and provides an interactive Shiny application for visual data cleaning.

## Features

- **Interactive Shiny Application**: Visual interface for data cleaning with real-time preview
- **PCDI (Pulse Count Delay Integration)**: Automatic optimization of GPS-sensor delay compensation
- **Multiple Filter Types**:
  - Header status filtering (eliminates points when header is raised)
  - GPS quality filtering (GPS status, DOP values)
  - Velocity filtering (removes stationary or too-fast points)
  - Overlap filtering (filters overlapping swath areas)
  - Local standard deviation filtering (removes abnormal points in neighborhood)
  - Moisture and yield range filtering
- **Raster Export**: Generate interpolated raster maps with spline interpolation
- **Comprehensive Logging**: Detailed tracking of all cleaning operations
- **Multi-format Support**: Import from various yield monitor formats

## Installation

```r
# Install from GitHub
devtools::install_github("cedricbouffard/yieldcleanr")
```

## Quick Start

### Command Line Usage

```r
library(yieldcleanr)

# Clean yield data with default parameters
cleaned <- clean_yield_data(
  file_path = "data/yield_data.txt",
  output_file = "data/cleaned.csv",
  log_file = "data/cleaning_log.txt"
)

# Clean with custom parameters
cleaned <- clean_yield_data(
  file_path = "data/yield_data.txt",
  output_file = "data/cleaned.csv",
  log_file = "data/cleaning_log.txt",
  params = list(
    flow_delay = 2,              # GPS-sensor delay (seconds)
    moisture_delay = 15,         # Moisture sensor delay (seconds)
    min_velocity = 0.5,          # Minimum velocity (m/s)
    max_velocity = 10,           # Maximum velocity (m/s)
    yield_range = c(74, 235),    # Acceptable yield range
    moisture_range = c(10, 40),  # Acceptable moisture range
    overlap_cellsize = 0.3,      # Overlap filter cell size (m)
    overlap_limit = 50,          # Overlap threshold (%)
    std_swath = 5,               # Local SD swath count
    std_limit = 3                # Local SD threshold (std dev)
  )
)
```

### Interactive Shiny Application

```r
# Launch the interactive cleaning application
yieldcleanr::launch_shiny_app()
```

The Shiny application provides:
- Visual preview of raw and cleaned data
- Interactive parameter adjustment
- Real-time filter application
- Multiple visualization modes (yield map, deleted points, raster view)
- Export to multiple formats (GeoJSON, CSV, GeoTIFF)

## Data Format

The package accepts yield data with the following columns:
- **Required**: Longitude, Latitude, Yield (kg/ha or bu/acre)
- **Optional**: Moisture (%), Flow (kg/s), Velocity (m/s), GPS Status, Header Status, Swath Width

Example data format:
```
Longitude,Latitude,Yield_kg_ha,Moisture,Flow,Velocity
-73.5123,45.1234,8500.5,18.2,12.5,5.2
-73.5124,45.1235,8200.3,18.1,12.3,5.1
```

## New Simplified API

yieldcleanr now provides 8 meta-functions that replace the individual filter functions:

### 1. Filter Data

Apply one or multiple filters to your data:

```r
# Filter header status only
filtered <- filter_data(data, type = "header")

# Apply multiple filters
filtered <- filter_data(data, type = c("header", "gps", "velocity"))

# Apply all available filters
filtered <- filter_data(data, type = "all")
```

**Filter types**: `"header"`, `"gps"`, `"dop"`, `"velocity"`, `"yield"`, `"moisture"`, `"bounds"`, `"all"`

### 2. Detect Anomalies

Detect and filter anomalies in your data:

```r
# Detect and filter all anomalies
cleaned <- detect_anomalies(data, type = "all")

# Detect only overlaps with custom parameters
cleaned <- detect_anomalies(data, type = "overlap", 
                            cellsize = 0.5, max_pass = 30)

# Mark anomalies without filtering (adds flag columns)
marked <- detect_anomalies(data, type = c("overlap", "local_sd"), 
                           action = "detect")
```

**Anomaly types**: `"overlap"`, `"local_sd"`, `"velocity_jump"`, `"heading"`, `"position"`, `"all"`

**Actions**: `"filter"` (remove points), `"detect"` (mark only), `"report"` (summary only)

### 3. Optimize Delays

Optimize GPS-sensor delays using PCDI method:

```r
# Optimize both flow and moisture delays
result <- optimize_delays(data, type = "both")
data_corrected <- result$data

# Optimize flow delay only
result <- optimize_delays(data, type = "flow")

# Get optimal delays without applying corrections
delays <- optimize_delays(data, type = "both", apply_correction = FALSE)
print(delays$delays$flow)  # Optimal flow delay in seconds
```

### 4. Calculate Thresholds

Automatically calculate thresholds for filtering:

```r
# Calculate all thresholds
thresholds <- calculate_thresholds(data)

# Calculate only yield thresholds with custom quantiles
thresholds <- calculate_thresholds(data, type = "yield",
                                   yllim = 0.05, yulim = 0.95, yscale = 1.5)

# Access calculated thresholds
thresholds$yield$min_yield  # Minimum yield threshold
thresholds$velocity$max_velocity  # Maximum velocity threshold
```

**Threshold types**: `"yield"`, `"velocity"`, `"position"`, `"moisture"`, `"all"`

### 5. Convert Coordinates

Convert between coordinate systems:

```r
# Convert Lat/Lon to UTM
data_utm <- convert_coordinates(data, from = "latlon", to = "utm")

# Convert UTM to Lat/Lon
data_latlon <- convert_coordinates(data, from = "utm", to = "latlon")
```

### 6. Convert Yield Units

Convert yield between different units:

```r
# Convert flow (lbs/s) to yield (kg/ha)
data_yield <- convert_yield_units(data, from = "flow_lbs_s", to = "kg_ha")

# Convert kg/ha to bu/acre for corn
data_imperial <- convert_yield_units(data, from = "kg_ha", to = "bu_acre",
                                     crop_type = "maize")

# Convert kg/ha to tonnes/ha
data_tonnes <- convert_yield_units(data, from = "kg_ha", to = "t_ha")
```

### 7. Anonymize Data

Anonymize sensitive information:

```r
# Full anonymization (coordinates + attributes)
data_anon <- anonymize_data(data, type = "full")

# Anonymize only coordinates with rotation method
data_anon <- anonymize_data(data, type = "coordinates", method = "rotation")

# Anonymize only sensitive attributes
data_anon <- anonymize_data(data, type = "attributes")
```

**Types**: `"coordinates"`, `"attributes"`, `"full"`

**Methods for coordinates**: `"translation"`, `"rotation"`, `"noise"`

### 8. Export Data

Export data to various formats:

```r
# Export to CSV
export_data(data, "output.csv", format = "csv")

# Export to GeoJSON (format auto-detected from extension)
export_data(data, "output.geojson")

# Export to Shapefile
export_data(data, "output.shp", format = "shp", overwrite = TRUE)

# Export to GeoPackage
export_data(data, "output.gpkg", format = "gpkg")

# Export to raster (GeoTIFF)
export_data(data, "yield_map.tif", format = "raster", resolution = 5)
```

**Formats**: `"csv"`, `"geojson"`, `"shp"`, `"gpkg"`, `"raster"`

## Filtering Pipeline

The cleaning process follows this order:

1. **PCDI (Optional)**: Optimize GPS-sensor delay for flow and moisture
2. **Header Status**: Remove points when header is raised
3. **GPS Quality**: Filter by GPS status and DOP values
4. **Velocity**: Remove stationary and high-speed points
5. **Yield Range**: Filter yield values outside acceptable range
6. **Moisture Range**: Filter moisture values outside acceptable range
7. **Overlap**: Remove points in overlapping swath areas
8. **Local Standard Deviation**: Remove statistically abnormal points

## Raster Export

Generate interpolated raster maps:

```r
# Export cleaned data to raster using the new API
export_data(
  data = data_clean,
  file = "output/yield_map.tif",
  format = "raster",
  resolution = 1  # 1 meter resolution
)

# Or use the legacy functions (still available)
library(sf)

# Load cleaned data
data <- st_read("data/cleaned.geojson")

# Create raster with spline interpolation
raster <- export_raster(
  data = data,
  cell_size = 1,              # 1 meter resolution
  column_colonne = "Yield_kg_ha",
  crs_code = 32618           # UTM Zone 18N
)

# Save as GeoTIFF
save_raster(raster, "output/yield_map.tif")
```

The raster export uses Thin Plate Spline (TPS) interpolation for smooth, continuous surfaces.

## Documentation

- [Getting Started](vignettes/getting-started.html) - Introduction and basic usage
- [Filtering Methods](vignettes/filtering-methods.html) - Detailed explanation of filters
- [Shiny Application](vignettes/shiny-app.html) - Guide to the interactive app
- [API Reference](reference/index.html) - Function documentation

## Contributing

Contributions are welcome! Please see [CONTRIBUTING.md](.github/CONTRIBUTING.md) for guidelines.

1. Fork the repository
2. Create a feature branch (`git checkout -b feature/amazing-feature`)
3. Commit your changes (`git commit -m 'Add amazing feature'`)
4. Push to the branch (`git push origin feature/amazing-feature`)
5. Open a Pull Request

## Citation

If you use this package in your research, please cite:

```
Bouffard, C. (2025). yieldcleanr: Tools for cleaning and validating 
    agricultural yield data. R package version 0.1.0.
```

## License

This project is licensed under the MIT License - see [LICENSE](LICENSE) for details.

## Acknowledgments

- USDA Yield Editor documentation for filtering methodology
- The R-spatial community for spatial analysis tools
- Contributors and testers

## Contact

- **Issues**: [GitHub Issues](https://github.com/cedricbouffard/yieldcleanr/issues)
- **Email**: cedric.bouffard@irda.qc.ca

---

**Note**: This package is under active development. Please report any issues or feature requests on GitHub.
