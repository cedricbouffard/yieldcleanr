# Appliquer la correction de delai d'humidite

Cette fonction compense le délai entre la mesure d'humidité et la
position GPS.

## Usage

``` r
apply_moisture_delay(data, delay = 15, direction = "forward")
```

## Arguments

- data:

  Tibble avec donnees de rendement

- delay:

  Nombre d'observations a decaler

- direction:

  Direction du decalage

## Value

Tibble avec valeurs d'humidite corrigees
