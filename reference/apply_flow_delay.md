# Appliquer la correction de delai de flux

Cette fonction compense le delai entre le moment ou le grain passe sous
le capteur de flux et le moment ou la position GPS est enregistree. Le
flux de grain est decale dans le temps pour correspondre a la position.

## Usage

``` r
apply_flow_delay(data, delay = 2, direction = "forward", value_col = "Flow")
```

## Arguments

- data:

  Tibble avec donnees de rendement

- delay:

  Nombre d'observations a decaler (positif = vers l'avant)

- direction:

  Direction du decalage : "forward" ou "backward"

## Value

Tibble avec valeurs de flux corrigees

## Examples

``` r
# Creer des donnees d'exemple
data <- tibble::tibble(
  Flow = c(1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5),
  Longitude = 1:7,
  Latitude = 1:7
)

# Appliquer la correction de delai de flux
data_corrected <- apply_flow_delay(data, delay = 1)
#> Flow delay correction: 1 seconds, 1 points elimines (valeurs NA)
print(data_corrected)
#> # A tibble: 6 × 4
#>    Flow Longitude Latitude Flow_raw
#>   <dbl>     <int>    <int>    <dbl>
#> 1   2.5         1        1      1.5
#> 2   3.5         2        2      2.5
#> 3   4.5         3        3      3.5
#> 4   5.5         4        4      4.5
#> 5   6.5         5        5      5.5
#> 6   7.5         6        6      6.5
```
