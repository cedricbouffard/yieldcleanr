# Pré-traitement des données de rendement

Cette fonction effectue les calculs coûteux qui ne dépendent pas des
filtres de rendement : UTM, position, PCDI, polygones, overlap

## Usage

``` r
preprocess_yield_data(data, params = list(), metrique = TRUE)
```

## Arguments

- data:

  Données brutes

- params:

  Liste des paramètres

- metrique:

  Booléen pour conversion métrique

## Value

Données pré-traitées avec tous les calculs coûteux effectués
