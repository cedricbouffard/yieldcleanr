# Méta-fonction de conversion des coordonnées

Convertit les coordonnées entre différents systèmes (Lat/Lon, UTM).

## Usage

``` r
convert_coordinates(data, from = "latlon", to = "utm", zone = NULL)
```

## Arguments

- data:

  Tibble avec données de rendement

- from:

  Système de coordonnées source: "latlon" ou "utm"

- to:

  Système de coordonnées cible: "latlon" ou "utm"

- zone:

  Zone UTM (optionnel, auto-détectée si non fournie)

## Value

Tibble avec coordonnées converties

## Examples

``` r
if (FALSE) { # \dontrun{
# Convertir Lat/Lon vers UTM
data_utm <- convert_coordinates(data, from = "latlon", to = "utm")

# Convertir UTM vers Lat/Lon
data_latlon <- convert_coordinates(data, from = "utm", to = "latlon")
} # }
```
