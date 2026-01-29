# Filtrer selon la plage de vitesse

Cette fonction filtre les points selon la vitesse de déplacement. La
vélocité est calculée comme la distance euclidienne entre points
consécutifs divisée par l'intervalle de temps.

## Usage

``` r
filter_velocity(data, min_velocity = 0.5, max_velocity = 10)
```

## Arguments

- data:

  Tibble avec donnees de rendement

- min_velocity:

  Vitesse minimale en m/s (defaut 0.5)

- max_velocity:

  Vitesse maximale en m/s (defaut 10)

## Value

Tibble filtre avec vitesses valides
