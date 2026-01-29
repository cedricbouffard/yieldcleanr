# Calculer le nombre de points retires par chaque filtre

Cette fonction calcule combien de points seraient retires par chaque
filtre sans les appliquer reellement. Utile pour afficher le nombre de
points retires dans l'interface utilisateur avant le traitement.

## Usage

``` r
calculate_filter_counts(data, params = NULL)
```

## Arguments

- data:

  Donnees brutes (tibble)

- params:

  Liste des parametres de filtrage

## Value

Liste avec le nombre de points retires par filtre
