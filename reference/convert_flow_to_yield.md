# Convertir le flux de grain en rendement (boisseaux/acre)

Convertit le flux brut (LBS/SEC) en rendement (boisseaux/acre) avec la
formule : Rendement (bu/acre) = (Flow x Interval x 43560) /
(lbs_per_bushel x Swath_ft x Distance_ft)

## Usage

``` r
convert_flow_to_yield(
  data,
  lbs_per_bushel = NULL,
  sqft_per_acre = 43560,
  inches_per_foot = 12,
  force_recalculate = FALSE
)
```

## Arguments

- data:

  Tibble avec Flow, Interval, Swath, Distance

- lbs_per_bushel:

  LBS par boisseau. Si NULL, auto-detection via GrainType. Defaut 56
  pour le mais. Utiliser 60 pour soja et cereales.

- sqft_per_acre:

  Pieds carres par acre (defaut 43560)

- inches_per_foot:

  Pouces par pied (defaut 12)

## Value

Donnees avec colonne Yield_kg_ha

## Details

Ou :

- Flow = flux de grain en lbs/sec

- Interval = intervalle de temps en secondes

- Swath_ft = largeur de coupe en pieds (Swath_in / 12)

- Distance_ft = distance parcourue en pieds (Distance_in / 12)

- 43560 = pieds^2 par acre

- lbs_per_bushel = lbs par boisseau (selon la culture)

Facteurs de conversion standard :

- Mais : 56 lbs/bu a 15.5% humidite

- Soja : 60 lbs/bu a 13% humidite

- Ble/cereales : 60 lbs/bu

## Examples

``` r
if (FALSE) { # \dontrun{
# Auto-detection selon la culture
data <- convert_flow_to_yield(data)

# Explicite pour le mais
data <- convert_flow_to_yield(data, lbs_per_bushel = 56)

# Pour soja/cereales
data <- convert_flow_to_yield(data, lbs_per_bushel = 60)
} # }
```
