# Filtrer selon les limites geographiques

Cette fonction filtre les points selon les limites geographiques du
champ (Easting/Northing ou Lat/Lon).

## Usage

``` r
filter_bounds(data, bounds = NULL, coord_type = "latlon")
```

## Arguments

- data:

  Tibble avec donnees de rendement

- bounds:

  Liste avec min/max des coordonnees x et y

- coord_type:

  Type de coordonnees : "utm" ou "latlon"

## Value

Tibble filtre dans les limites
