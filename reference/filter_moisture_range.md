# Filtrer selon la plage d'humidite

Cette fonction filtre les points selon la plage d'humidité valide. Peut
utiliser des valeurs explicites ou l'auto-détection basée sur
l'écart-type.

## Usage

``` r
filter_moisture_range(
  data,
  min_moisture = NULL,
  max_moisture = NULL,
  n_std = 3
)
```

## Arguments

- data:

  Tibble avec donnees de rendement

- min_moisture:

  Humidite minimale acceptable. Si NULL, calcule automatiquement.

- max_moisture:

  Humidite maximale acceptable. Si NULL, calcule automatiquement.

- n_std:

  Nombre d'ecarts-types pour auto-detection (defaut 3)

## Value

Tibble filtre dans la plage d'humidite

## Examples

``` r
# Valeurs explicites
data_clean <- filter_moisture_range(data, min_moisture = 8, max_moisture = 15)
#> Warning: Colonne Moisture non trouvée, saut du filtrage

# Auto-détection basée sur l'écart-type (mean ± 3*sd)
data_clean <- filter_moisture_range(data)
#> Warning: Colonne Moisture non trouvée, saut du filtrage

# Auto-détection avec plage plus large (mean ± 4*sd)
data_clean <- filter_moisture_range(data, n_std = 4)
#> Warning: Colonne Moisture non trouvée, saut du filtrage
```
