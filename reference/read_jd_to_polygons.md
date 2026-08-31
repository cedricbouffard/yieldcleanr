# Lire des donnees John Deere et convertir en polygones metriques

Lit un fichier ZIP John Deere (rendement, semis, vitesse, etc.) ou un
fichier vectoriel (shapefile, GeoPackage, GeoJSON) et cree des
polygones. Utilise les unites du JSON de metadonnees pour convertir en
metrique.

## Usage

``` r
read_jd_to_polygons(file_path, field_name = NULL)
```

## Arguments

- file_path:

  Chemin vers un fichier ZIP ou un fichier vectoriel (.shp, .gpkg,
  .geojson)

- field_name:

  Nom du champ dans le fichier ZIP. Ignore si file_path est un fichier
  vectoriel. Si NULL et que le ZIP ne contient qu'un seul shapefile,
  celui-ci est utilise automatiquement.

## Value

Objet SF avec polygones et toutes les colonnes preservees
