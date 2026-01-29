# Fonction unifiee de nettoyage des donnees de rendement

Cette fonction execute le pipeline complet de nettoyage des donnees de
rendement avec support pour les sorties en unites metriques ou
imperiales, et avec ou sans geometries SF (polygones ou points).

## Usage

``` r
clean_yield(
  file_path,
  metrique = TRUE,
  polygon = TRUE,
  params = NULL,
  output_file = NULL,
  log_file = NULL
)
```

## Arguments

- file_path:

  Chemin du fichier d'entree (txt/csv)

- metrique:

  TRUE pour les unites metriques (kg/ha), FALSE pour l'imperial
  (bu/acre)

- polygon:

  TRUE pour une sortie SF en polygones, FALSE pour une sortie tibble

- params:

  Liste des parametres AYCE (voir details)

- output_file:

  Chemin optionnel pour sauvegarder la sortie (CSV ou GeoJSON)

- log_file:

  Chemin optionnel pour sauvegarder le journal

## Value

Donnees nettoyees (tibble ou objet SF selon les parametres)

## Examples

``` r
if (FALSE) { # \dontrun{
# Sortie metrique avec polygones (objet SF)
sf_result <- clean_yield("data.txt", metrique = TRUE, polygon = TRUE)
plot(sf_result["Yield_kg_ha"])

# Sortie imperiale en tibble
data_result <- clean_yield("data.txt", metrique = FALSE, polygon = FALSE)

# Sortie metrique en tibble (sans geometrie)
data_metric <- clean_yield("data.txt", metrique = TRUE, polygon = FALSE)

# Avec parametres personnalises
result <- clean_yield("data.txt",
  metrique = TRUE,
  polygon = TRUE,
  params = list(
    delay_range = -10:25,
    n_swaths = 5,
    lsd_limit = 2.5
  )
)
} # }
```
