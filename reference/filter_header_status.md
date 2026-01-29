# Filtrer selon le statut du header

Cette fonction filtre les données pour ne garder que les points où la
moissonneuse est en position de travail (header abaissé ou actif).
Header Status: 1 = harvesting (actif), 33 = header down (abaissé). Les
deux valeurs indiquent une récolte active.

## Usage

``` r
filter_header_status(data, header_values = c(1, 33))
```

## Arguments

- data:

  Tibble avec donnees de rendement

- header_values:

  Valeurs indiquant une recolte active (defaut c(1, 33))

## Value

Tibble filtre avec header actif

## Examples

``` r
# Creer des donnees d'exemple avec header mixte
data <- tibble::tibble(
  Flow = c(1.53, 3.7, 7.56, 10.36, 15.48),
  HeaderStatus = c(1, 33, 33, 0, 33)  # 1=actif, 33=header bas, 0=header haut
)

# Filtrer pour ne garder que la recolte active
data_filtered <- filter_header_status(data)
#> Header Status filter: 1 points éliminés (header non actif, valeurs acceptées: 1, 33 )
print(data_filtered)
#> # A tibble: 4 × 2
#>    Flow HeaderStatus
#>   <dbl>        <dbl>
#> 1  1.53            1
#> 2  3.7            33
#> 3  7.56           33
#> 4 15.5            33
```
