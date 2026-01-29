# Filtre de chevauchement base sur un bitmap

Implemente la methode rasterisee de Han et al. (1997) pour detecter et
eliminer les zones de chevauchement.

## Usage

``` r
apply_overlap_filter(data, cellsize = 0.3, overlap_threshold = 0.5)
```

## Arguments

- data:

  Tibble avec coordonnees X, Y et largeur de coupe

- cellsize:

  Taille de cellule de grille en metres (defaut 0.3)

- overlap_threshold:

  Ratio maximal de chevauchement (0-1, defaut 0.5)

## Value

Tibble filtre sans chevauchement

## Examples

``` r
# Creer des donnees d'exemple avec chevauchements possibles
data <- tibble::tibble(
  X = c(435000, 435001, 435002, 435003, 435100),
  Y = c(5262000, 5262001, 5262002, 5262003, 5262100),
  Flow = c(10, 15, 12, 18, 20),
  Swath = c(240, 240, 240, 240, 240)
)

# Appliquer le filtre de chevauchement (cellule 0.3m, max 50%)
data_clean <- apply_overlap_filter(data, cellsize = 0.3, overlap_threshold = 0.5)
#> === Bitmap Overlap Filter ===
#> Donnees etendues - utilisation du bitmap sparse
#> Overlap ratio: min 0 max 0
#> Overlap filter: 0 points éliminés (0%)
print(data_clean)
#> # A tibble: 5 × 5
#>        X       Y  Flow Swath overlap_ratio
#>    <dbl>   <dbl> <dbl> <dbl>         <dbl>
#> 1 435000 5262000    10   240             0
#> 2 435001 5262001    15   240             0
#> 3 435002 5262002    12   240             0
#> 4 435003 5262003    18   240             0
#> 5 435100 5262100    20   240             0
```
