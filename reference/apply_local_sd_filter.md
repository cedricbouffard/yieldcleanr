# Filtre d'ecart-type localise

Cree une grille spatiale et elimine les points aberrants locaux bases
sur l'ecart-type local.

## Usage

``` r
apply_local_sd_filter(data, n_swaths = 5, lsd_limit = 3, min_cells = 3)
```

## Arguments

- data:

  Tibble avec colonnes X, Y, Flow

- n_swaths:

  Nombre de largeurs de coupe par cellule (defaut 5)

- lsd_limit:

  Multiplicateur de l'ET local (defaut 3)

- min_cells:

  Observations minimales par cellule (defaut 3)

## Value

Tibble filtre

## Examples

``` r
# Creer des donnees d'exemple avec outliers locaux
data <- tibble::tibble(
  X = c(435000, 435001, 435002, 435003, 435004, 435005,
        435100, 435101, 435102, 435103, 435104, 435105),
  Y = c(5262000, 5262001, 5262002, 5262003, 5262004, 5262005,
        5262100, 5262101, 5262102, 5262103, 5262104, 5262105),
  Flow = c(50, 55, 52, 58, 300, 54,  # 300 = outlier local
           45, 48, 47, 50, 49, 46),
  Swath = c(240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240)
)

# Appliquer le filtre ET local
data_clean <- apply_local_sd_filter(data, n_swaths = 5, lsd_limit = 3)
#> === Localized SD Filter ===
#> Local SD filter: 0 points elimines (0%)
print(data_clean)
#> # A tibble: 12 × 4
#>         X       Y  Flow Swath
#>     <dbl>   <dbl> <dbl> <dbl>
#>  1 435000 5262000    50   240
#>  2 435001 5262001    55   240
#>  3 435002 5262002    52   240
#>  4 435003 5262003    58   240
#>  5 435004 5262004   300   240
#>  6 435005 5262005    54   240
#>  7 435100 5262100    45   240
#>  8 435101 5262101    48   240
#>  9 435102 5262102    47   240
#> 10 435103 5262103    50   240
#> 11 435104 5262104    49   240
#> 12 435105 5262105    46   240
```
