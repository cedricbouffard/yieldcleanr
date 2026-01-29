# AYCE : Auto Yield Cleaning Engine (sortie imperiale)

Systeme expert automatise pour le nettoyage des donnees de rendement
sans intervention humaine, base sur les methodes USDA Yield Editor.
Cette fonction retourne un tibble en unites imperiales (bu/acre).

## Usage

``` r
ayce_clean(file_path, output_file = NULL, log_file = NULL, params = NULL)
```

## Arguments

- file_path:

  Chemin du fichier d'entree (txt)

- output_file:

  Chemin du fichier CSV de sortie

- log_file:

  Chemin du journal de sortie

- params:

  Liste des parametres AYCE

## Value

Tibble nettoye en unites imperiales

## Examples

``` r
if (FALSE) { # \dontrun{
cleaned <- ayce_clean("data/original.txt")
} # }
```
