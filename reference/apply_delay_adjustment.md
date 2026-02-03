# Delay Adjustment : Delay Adjustment (Version Rapide)

Determine automatiquement le delai optimal entre le flux et la position
GPS en utilisant une recherche en deux etapes (grossiere + fine) avec
rgeoda.

## Usage

``` r
apply_delay_adjustment(
  data,
  delay_range = -25:25,
  n_iterations = 5,
  noise_level = NULL,
  value_col = "Flow",
  sample_fraction = NULL,
  method = NULL,
  coarse_step = 2,
  max_points = 10000,
  bandwidth = 30
)
```

## Arguments

- data:

  Tibble avec donnees de rendement (X, Y, Flow, GPS_Time, Interval)

- delay_range:

  Plage de delais a tester (defaut -25:25 secondes)

- value_col:

  Nom de la colonne de valeurs a analyser

- coarse_step:

  Pas pour la recherche grossiere (defaut 2)

- max_points:

  Nombre maximum de points pour l'analyse

- bandwidth:

  Distance de bande passante pour Moran (defaut 30m)

## Value

Liste avec optimal_delay, score_values et stability_metrics

## Examples

``` r
if (FALSE) { # \dontrun{
result <- apply_delay_adjustment(data, delay_range = -25:25)
} # }
```
