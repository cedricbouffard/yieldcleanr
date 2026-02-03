# Nettoyage rapide des données de rendement avec mise en cache

Cette fonction permet de nettoyer les données en plusieurs phases :

1.  Pré-traitement (polygones, overlap, Delay Adjustment) - calculé une
    fois

2.  Filtres de rendement - peuvent être réappliqués sans recalculer le
    pré-traitement

## Usage

``` r
clean_yield_fast(
  data,
  phase = "full",
  preprocessed_data = NULL,
  params = list(),
  metrique = TRUE,
  polygon = TRUE
)
```

## Arguments

- data:

  Données brutes

- phase:

  "preprocess" ou "filter" ou "full"

- preprocessed_data:

  Données pré-traitées (si phase = "filter")

- params:

  Liste des paramètres

- metrique:

  Booléen pour conversion métrique

- polygon:

  Booléen pour création de polygones

## Value

Liste avec données nettoyées et métadonnées
