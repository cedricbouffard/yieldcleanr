# Appliquer le filtre ET local

Cette fonction identifie et elimine les points anormaux en utilisant un
voisinage de n swathes autour de chaque point. Les points dont le
rendement s'ecarte de plus de STD_limit ecarts-types de la moyenne
locale sont elimines.

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
