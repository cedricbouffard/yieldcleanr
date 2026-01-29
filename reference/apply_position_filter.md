# Appliquer le filtre de position (POS)

Elimine les flyers GPS en verifiant que les points sont dans l'enveloppe
inter-quantile etendue du champ.

## Usage

``` r
apply_position_filter(data, thresholds)
```

## Arguments

- data:

  Tibble avec coordonnees X, Y

- thresholds:

  Liste des seuils de position

## Value

Tibble filtre
