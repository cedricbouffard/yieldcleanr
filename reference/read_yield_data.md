# Lire des donnees de rendement brutes depuis un fichier texte

Cette fonction lit les donnees brutes de rendement depuis un fichier
texte formate selon le standard des fichiers de moissonneuse. Supporte
differents formats de fichiers (15-17 colonnes).

## Usage

``` r
read_yield_data(file_path = NULL, data = NULL, col_names = TRUE)
```

## Arguments

- file_path:

  Chemin du fichier texte d'entree

- col_names:

  Logique, si TRUE utilise les noms de colonnes standard

## Value

Un tibble avec les donnees brutes

## Examples

``` r
# Exemple avec donnees d'exemple (creation d'un fichier temporaire)
temp_file <- tempfile(pattern = "yield_data", fileext = ".txt")
writeLines(c(
  "-69.856661,47.506122,1.53,1762958157,2,77,240,30.8,33,1,2410019049,F0:1,L0:<1>,Maïs,7,0,61.3",
  "-69.856681,47.506136,3.7,1762958159,2,87,240,30.9,33,1,2410019049,F0:1,L0:<1>,Maïs,7,0,61.5"
), temp_file)

data <- read_yield_data(temp_file)
#> Warning: NAs introduced by coercion to integer range
#> Distance detectee en pouces (moyenne: 82 ) - conversion en metres
#> Swath detecte en pouces (moyenne: 240 ) - conversion en metres
print(data)
#> # A tibble: 2 × 18
#>   Longitude Latitude  Flow   GPS_Time Interval Distance Swath Moisture
#>       <dbl>    <dbl> <dbl>      <int>    <int>    <dbl> <dbl>    <dbl>
#> 1     -69.9     47.5  1.53 1762958157        2     1.96  6.10     30.8
#> 2     -69.9     47.5  3.7  1762958159        2     2.21  6.10     30.9
#> # ℹ 10 more variables: HeaderStatus <int>, Pass <int>, Serial <int>,
#> #   FieldID <chr>, LoadID <chr>, GrainType <chr>, GPSStatus <int>, DOP <dbl>,
#> #   Altitude <dbl>, .row_id <int>
```
