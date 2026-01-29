# Pipeline AYCE complet avec sortie SF

Cette fonction execute le pipeline AYCE complet et retourne un objet SF
avec des polygones rectangles orientes et toutes les mesures en
metrique. Enveloppe de clean_yield() avec sortie metrique + polygones.

## Usage

``` r
ayce_sf(
  file_path = NULL,
  data = NULL,
  output_file = NULL,
  log_file = NULL,
  geometry_type = c("polygon", "point"),
  params = NULL
)
```

## Arguments

- file_path:

  Chemin du fichier d'entree

- output_file:

  Chemin optionnel pour GeoJSON

- log_file:

  Chemin optionnel pour le journal

- geometry_type:

  "polygon" ou "point" (compatibilite)

- params:

  Liste des parametres AYCE

## Value

Objet SF avec donnees nettoyees

## Examples

``` r
if (FALSE) { # \dontrun{
sf_result <- ayce_sf(
  file_path = "data.txt",
  geometry_type = "polygon"
)
plot(sf_result["Yield"])
} # }
```
