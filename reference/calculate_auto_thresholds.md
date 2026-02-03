# Calculer les seuils automatiques (methode quantiles-IQR)

Automatise les filtres MINY, MAXY, MINV, MAXV, POS a partir d'analyses
de distributions basees sur les quantiles.

## Usage

``` r
calculate_auto_thresholds(
  data,
  yllim = 0.05,
  yulim = 0.95,
  yscale = 1.5,
  vllim = 0.02,
  vulim = 0.98,
  vscale = 1.5,
  minv_abs = 0.5,
  miny_abs = 0,
  gbuffer = 100
)
```

## Arguments

- data:

  Tibble avec donnees de rendement

- yllim:

  Limite quantile basse (defaut 0.05)

- yulim:

  Limite quantile haute (defaut 0.95)

- yscale:

  Facteur d'extension IQR (defaut 1.5)

- vllim:

  Limite quantile basse vitesse (defaut 0.02)

- vulim:

  Limite quantile haute vitesse (defaut 0.98)

- vscale:

  Facteur d'extension IQR vitesse (defaut 1.5)

- minv_abs:

  Seuil minimal absolu de vitesse (defaut 0.5 m/s)

- miny_abs:

  Seuil minimal absolu de rendement (defaut 0)

- gbuffer:

  Marge pour le filtre de position en metres (defaut 100)

## Value

Liste avec tous les seuils calcules
