# Appliquer le filtre ET local

Cette fonction identifie et élimine les points anormaux en utilisant un
voisinage de n swathes autour de chaque point. Les points dont le
rendement s'écarte de plus de STD_limit écarts-types de la moyenne
locale sont éliminés.

## Usage

``` r
filter_local_std(data, swath_window = 5, std_limit = 3, yield_col = "Flow")
```

## Arguments

- data:

  Tibble avec donnees (colonne Pass requise)

- swath_window:

  Nombre de passages dans le voisinage local

- std_limit:

  Nombre maximal d'ecarts-types depuis la moyenne locale

- yield_col:

  Nom de la colonne de rendement (defaut "Flow")

## Value

Tibble filtre avec outliers supprimes

## Examples

``` r
if (FALSE) { # \dontrun{
data_clean <- filter_local_std(data, swath_window = 5, std_limit = 3)
} # }
```
