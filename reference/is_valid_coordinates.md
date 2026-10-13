# Verifier la validite des coordonnees geographiques

Verifier la validite des coordonnees geographiques

## Usage

``` r
is_valid_coordinates(lon, lat, crs = 4326)
```

## Arguments

- lon:

  Vecteur de longitudes

- lat:

  Vecteur de latitudes

- crs:

  Code EPSG du CRS (defaut 4326 pour WGS84)

## Value

Vecteur logique indiquant si chaque paire de coordonnees est valide

## Examples

``` r
is_valid_coordinates(c(-73.5, 200), c(45.5, 95))
#> [1]  TRUE FALSE
```
