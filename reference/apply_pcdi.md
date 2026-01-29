# PCDI : Phase Correlation Delay Identification

Determine automatiquement le delai optimal entre le flux et la position
GPS en utilisant la methode de correlation de phase.

## Usage

``` r
apply_pcdi(
  data,
  delay_range = 0:20,
  n_iterations = 10,
  noise_level = 0.05,
  value_col = "Flow"
)
```

## Arguments

- data:

  Tibble avec donnees de rendement (X, Y, Flow, GPS_Time, Interval)

- delay_range:

  Plage de delais a tester (defaut 0:20 secondes)

- n_iterations:

  Nombre d'iterations avec bruit aleatoire (defaut 10)

- noise_level:

  Niveau de bruit gaussien en proportion de la plage

## Value

Liste avec optimal_delay, rsc_values et stability_metrics

## Examples

``` r
if (FALSE) { # \dontrun{
result <- apply_pcdi(data, delay_range = 0:20)
} # }
```
