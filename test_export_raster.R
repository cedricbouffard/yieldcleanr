# Clean environment
unloadNamespace('yieldcleanr')

# Reload package from source
devtools::load_all('.')

# Test with sample data
data <- data.frame(
  Longitude = c(-73.5, -73.51, -73.52, -73.53, -73.54),
  Latitude = c(45.5, 45.51, 45.52, 45.53, 45.54),
  Yield_kg_ha = c(1000, 1200, 1100, 1300, 900)
)
data_sf <- sf::st_as_sf(data, coords = c("Longitude", "Latitude"), crs = 4326)
r <- export_raster(data_sf, cell_size = 10)
print(r)