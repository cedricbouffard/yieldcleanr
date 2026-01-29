# Appliquer le filtre a fenetre glissante

Cette fonction applique un filtre à fenêtre glissante pour éliminer les
valeurs aberrantes basées sur les voisins temporels.

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
