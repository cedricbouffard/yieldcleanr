# Filtrer selon le statut GPS

Cette fonction filtre les données selon la qualité du signal GPS.

## Usage

``` r
filter_gps_status(data, min_gps_status = 4)
```

## Arguments

- data:

  Tibble avec donnees de rendement

- min_gps_status:

  Statut GPS minimal (defaut 4 = bon)

## Value

Tibble filtre avec GPS valide
