# Filtrer selon le DOP (Dilution of Precision)

Cette fonction elimine les points avec un DOP trop eleve (mauvaise
precision GPS).

## Usage

``` r
filter_dop(data, max_dop = 10)
```

## Arguments

- data:

  Tibble avec donnees de rendement

- max_dop:

  Valeur maximale acceptable du DOP (defaut 10)

## Value

Tibble filtre avec DOP valide
