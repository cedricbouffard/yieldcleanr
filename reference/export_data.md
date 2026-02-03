# Méta-fonction d'export des données

Exporte les données nettoyées dans différents formats (CSV, GeoJSON,
Shapefile, raster, etc.).

## Usage

``` r
export_data(data, file, format = NULL, ...)
```

## Arguments

- data:

  Données à exporter (tibble ou objet sf)

- file:

  Chemin du fichier de sortie

- format:

  Format d'export: "csv", "geojson", "shp", "gpkg", "raster". Si NULL,
  détecté automatiquement à partir de l'extension du fichier.

- ...:

  Paramètres supplémentaires pour l'export

## Value

Chemin du fichier créé (invisible)

## Examples

``` r
if (FALSE) { # \dontrun{
# Export CSV
export_data(data, "output.csv", format = "csv")

# Export GeoJSON (format auto-détecté)
export_data(data, "output.geojson")

# Export Shapefile avec options
export_data(data, "output.shp", format = "shp", overwrite = TRUE)

# Export raster
export_data(data, "yield.tif", format = "raster", resolution = 5)
} # }
```
