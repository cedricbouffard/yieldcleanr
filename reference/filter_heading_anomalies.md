# Filtre pour variations brusques de direction du header

Detecte et supprime les points ou le header varie anormalement. Conserve
les virages normaux mais retire les points isoles anormaux (par exemple:
un point tourne et le point suivant revient dans le bon sens).

## Usage

``` r
filter_heading_anomalies(data, max_heading_change = 60, window_size = 3)
```

## Arguments

- data:

  Tibble avec au minimum X, Y, orig_row_id et GPS_Time

- max_heading_change:

  Variation maximale de direction entre 3 points consecutifs (degrés,
  défaut: 60)

- window_size:

  Taille de la fenetre pour detecter les anomalies (defaut: 3)

## Value

Liste avec data (donnees filtrees) et removed (points supprimes)
