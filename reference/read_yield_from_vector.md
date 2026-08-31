# Lire des donnees depuis un fichier vectoriel (shapefile, GeoPackage, GeoJSON)

Equivalent de read_jd_to_polygons mais retourne un data frame de points
standardise (sans geometrie), pret pour le pipeline de nettoyage. Utile
pour les fichiers de semis, d'epandage, etc. qui ne sont pas du
rendement.

## Usage

``` r
read_yield_from_vector(file_path)
```

## Arguments

- file_path:

  Chemin vers un fichier .shp, .gpkg ou .geojson

## Value

Data frame avec colonnes standardisees et coordonnees Longitude/Latitude
