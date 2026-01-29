# Filtrer selon la plage de vitesse

Cette fonction filtre les points selon la vitesse de deplacement. La
velocite est calculee comme la distance euclidienne entre points
consecutifs divisee par l'intervalle de temps.

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
