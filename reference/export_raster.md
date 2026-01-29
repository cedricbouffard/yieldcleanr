# Exporter les donnees nettoyees en raster

Convertit les donnees de rendement nettoyees en raster avec une
resolution specifique et decoupe selon un polygone concave.

## Usage

``` r
export_raster(
  data,
  cell_size = 1,
  column_colonne = "Yield_kg_ha",
  fun = mean,
  crs_code = NULL
)
```

## Arguments

- data:

  Donnees nettoyees (objet sf avec geometrie points ou polygones)

- cell_size:

  Taille des cellules en metres (defaut: 1)

- column_colonne:

  Nom de la colonne contenant les valeurs a rasteriser

- fun:

  Fonction d'agregation (defaut: mean)

- crs_code:

  Code EPSG du systeme de coordonnees (defaut: NULL, auto-detecte)

## Value

Objet SpatRaster
