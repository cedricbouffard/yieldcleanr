# Filtre pour changements brusques de vitesse

Detecte et supprime les points ou il y a une variation tres grande de la
vitesse entre deux points consecutifs. La vitesse est calculee a partir
de la distance et du temps entre les points.

## Usage

``` r
filter_velocity_jumps(data, max_acceleration = 5, max_deceleration = -8)
```

## Arguments

- data:

  Tibble avec au minimum X, Y, Interval et orig_row_id

- max_acceleration:

  Acceleration maximale autorisee (m/s, defaut: 5)

- max_deceleration:

  Deceleration maximale autorisee (m/s, defaut: -8)

## Value

Liste avec data (donnees filtrees) et removed (points supprimes)
