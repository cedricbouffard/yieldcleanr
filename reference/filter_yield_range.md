# Filtrer selon la plage de rendement

Cette fonction filtre les points selon la plage de rendement valide.
Peut utiliser des valeurs explicites ou l'auto-détection basée sur
l'écart-type.

## Usage

``` r
filter_yield_range(
  data,
  min_yield = NULL,
  max_yield = NULL,
  yield_column = "Yield_buacre",
  n_std = 3
)
```

## Arguments

- data:

  Tibble avec donnees de rendement

- min_yield:

  Rendement minimal acceptable. Si NULL, calcule automatiquement.

- max_yield:

  Rendement maximal acceptable. Si NULL, calcule automatiquement.

- yield_column:

  Nom de la colonne de rendement (defaut "Yield_buacre")

- n_std:

  Nombre d'ecarts-types pour auto-detection (defaut 3)

## Value

Tibble filtre dans la plage de rendement

## Examples

``` r
# Creer des donnees d'exemple avec rendements
data <- tibble::tibble(
  Yield_buacre = c(50, 100, 150, 300, 180),
  Flow = c(1.53, 3.7, 7.56, 10.36, 15.48)
)

# Valeurs explicites
data_filtered <- filter_yield_range(data, min_yield = 50, max_yield = 200)
#> Yield range filter: 1 points éliminés (rendement hors plage: 50 - 200 )

# Auto-detection basee sur l'ecart-type (moyenne ± 3*ET)
data_filtered <- filter_yield_range(data)
#> Yield auto-range: -127.5 - 439.5 (mean ± 3 SD = 156 ± 94.5 )

# Auto-detection avec plage plus large (moyenne ± 4*ET)
data_filtered <- filter_yield_range(data, n_std = 4)
#> Yield auto-range: -222 - 534 (mean ± 4 SD = 156 ± 94.5 )
```
