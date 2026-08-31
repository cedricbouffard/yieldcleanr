# Creer des polygones rectangulaires a partir de donnees ponctuelles

Convertit un data.frame avec coordonnees, heading, swath et distance en
polygones rectangulaires en UTM.

## Usage

``` r
create_polygons_from_data(data, heading_col = NULL)
```

## Arguments

- data:

  Data.frame avec colonnes Longitude, Latitude, et soit (Swath_m,
  Distance_m) soit (Swath, Distance) en metres

- heading_col:

  Nom de la colonne heading (defaut: "Heading" ou "heading")

## Value

Objet SF avec polygones en UTM
