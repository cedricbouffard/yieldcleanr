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
  crs_code = NULL,
  method = c("auto", "tps", "idw", "nearest"),
  max_points_tps = 5000
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

- method:

  Methode d'interpolation: "auto" (defaut), "tps", "idw", ou "nearest"

  - "auto": Choisit automatiquement selon le nombre de points

  - "tps": Thin Plate Spline (lent mais precis, max 5000 points)

  - "idw": Inverse Distance Weighting (rapide, recommande)

  - "nearest": Plus proche voisin (tres rapide)

- max_points_tps:

  Nombre maximum de points pour TPS (defaut: 5000) Si plus de points,
  echantillonnage aleatoire ou switch vers IDW

## Value

Objet SpatRaster
