# Division securisee contre les divisions par zero ou NaN

Division securisee contre les divisions par zero ou NaN

## Usage

``` r
safe_divide(numerator, denominator, default = 0)
```

## Arguments

- numerator:

  Numerateur (scalaire ou vecteur)

- denominator:

  Denominateur (scalaire ou vecteur)

- default:

  Valeur par defaut a renvoyer en cas de division impossible (defaut 0)

## Value

Resultat de la division avec default aux positions indefinies

## Examples

``` r
safe_divide(10, 2)
#> [1] 5
safe_divide(10, 0, default = NA)
#> [1] NA
```
