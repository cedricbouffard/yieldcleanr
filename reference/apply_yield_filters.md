# Application des filtres de rendement

Cette fonction applique les filtres qui peuvent être modifiés sans
recalculer le pré-traitement : header, GPS, vitesse, plage de rendement,
humidité, etc.

## Usage

``` r
apply_yield_filters(preprocessed_data, params = list(), polygon = TRUE)
```

## Arguments

- preprocessed_data:

  Données pré-traitées

- params:

  Liste des paramètres des filtres

- polygon:

  Booléen pour création de polygones

## Value

Données filtrées
