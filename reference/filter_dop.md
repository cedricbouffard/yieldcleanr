# Filtrer selon le DOP (Dilution of Precision)

Cette fonction élimine les points avec un DOP trop élevé (mauvaise
précision GPS).

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
