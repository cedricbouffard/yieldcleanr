# Appliquer le filtre a fenetre glissante

Cette fonction applique un filtre a fenetre glissante pour eliminer les
valeurs aberrantes basees sur les voisins temporels.

## Usage

``` r
filter_sliding_window(data, window_size = 11, n_std = 3, yield_col = "Flow")
```

## Arguments

- data:

  Tibble avec donnees de rendement

- window_size:

  Taille de la fenetre glissante

- n_std:

  Nombre d'ecarts-types pour le seuil

- yield_col:

  Nom de la colonne de rendement

## Value

Tibble filtre
