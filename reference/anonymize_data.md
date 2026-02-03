# Méta-fonction d'anonymisation des données

Anonymise les données de rendement en supprimant les informations
sensibles et/ou en transformant les coordonnées géographiques.

## Usage

``` r
anonymize_data(
  data,
  type = "full",
  method = "translation",
  preserve_structure = TRUE,
  ...
)
```

## Arguments

- data:

  Tibble avec données de rendement

- type:

  Type d'anonymisation: "coordinates" (décalage coordonnées),
  "attributes" (suppression attributs sensibles), "full" (les deux).
  Défaut: "full"

- method:

  Méthode d'anonymisation des coordonnées: "translation" (décalage
  aléatoire), "rotation" (rotation), "noise" (bruit). Défaut:
  "translation"

- preserve_structure:

  Si TRUE, conserve la structure spatiale relative. Défaut: TRUE

- ...:

  Paramètres supplémentaires pour l'anonymisation

## Value

Données anonymisées

## Examples

``` r
if (FALSE) { # \dontrun{
# Anonymisation complète
data_anon <- anonymize_data(data, type = "full")

# Anonymiser uniquement les coordonnées avec méthode de rotation
data_anon <- anonymize_data(data, type = "coordinates", method = "rotation")

# Anonymiser uniquement les attributs sensibles
data_anon <- anonymize_data(data, type = "attributes")
} # }
```
