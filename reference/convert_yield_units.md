# Méta-fonction de conversion des unités de rendement

Convertit les unités de rendement entre différents systèmes (kg/ha,
bu/acre, etc.) et calcule le rendement à partir du flux.

## Usage

``` r
convert_yield_units(
  data,
  from = "flow_lbs_s",
  to = "kg_ha",
  crop_type = NULL,
  moisture_std = NULL
)
```

## Arguments

- data:

  Tibble avec données de rendement

- from:

  Unité source: "flow_lbs_s", "kg_ha", "bu_acre"

- to:

  Unité cible: "kg_ha", "bu_acre", "t_ha"

- crop_type:

  Type de culture pour la conversion (maize, soybean, wheat, etc.)

- moisture_std:

  Humidité standard pour la conversion (défaut selon culture)

## Value

Tibble avec rendement converti

## Examples

``` r
if (FALSE) { # \dontrun{
# Convertir flux (lbs/s) vers kg/ha
data_yield <- convert_yield_units(data, from = "flow_lbs_s", to = "kg_ha")

# Convertir kg/ha vers bu/acre pour du maïs
data_imperial <- convert_yield_units(data, from = "kg_ha", to = "bu_acre",
                                    crop_type = "maize")
} # }
```
