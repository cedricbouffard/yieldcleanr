# Convertir Latitude/Longitude en coordonnees UTM

Cette fonction convertit les coordonnées géographiques (WGS84) en
coordonnées UTM (Eastings/Northings).

## Usage

``` r
latlon_to_utm(data, zone = NULL, datum = "WGS84")
```

## Arguments

- data:

  Tibble avec colonnes Latitude et Longitude

- zone:

  Zone UTM (auto-detectee si NULL)

- datum:

  Datum a utiliser (defaut "WGS84")

## Value

Tibble avec colonnes X (Easting) et Y (Northing)

## Examples

``` r
# Creer des donnees d'exemple
data <- tibble::tibble(
  Latitude = c(47.506122, 47.506136, 47.506152),
  Longitude = c(-69.856661, -69.856681, -69.856701),
  Flow = c(1.53, 3.7, 7.56)
)

# Convertir en UTM
data_utm <- latlon_to_utm(data)
#> Zone UTM détectée: 19
print(data_utm)
#> # A tibble: 3 × 5
#>   Latitude Longitude  Flow       X        Y
#>      <dbl>     <dbl> <dbl>   <dbl>    <dbl>
#> 1     47.5     -69.9  1.53 435490. 5261766.
#> 2     47.5     -69.9  3.7  435488. 5261767.
#> 3     47.5     -69.9  7.56 435487. 5261769.
```
