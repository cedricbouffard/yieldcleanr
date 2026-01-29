# Nettoyage des donnees de rendement avec suivi des suppressions

Variante de clean_yield() qui enregistre les points supprimes a chaque
etape ainsi que la raison. Utile pour la visualisation et le diagnostic.

## Usage

``` r
clean_yield_with_tracking(
  file_path = NULL,
  data = NULL,
  metrique = TRUE,
  polygon = TRUE,
  params = NULL
)
```

## Arguments

- file_path:

  Chemin du fichier d'entree (txt/csv)

- data:

  Donnees brutes a nettoyer (tibble). Alternative a file_path. Si
  fourni, file_path est ignore.

- metrique:

  TRUE pour les unites metriques (kg/ha), FALSE pour l'imperial
  (bu/acre)

- polygon:

  TRUE pour une sortie SF en polygones, FALSE pour une sortie tibble

- params:

  Liste des parametres AYCE (voir details)

## Value

A list containing:

- data_clean: Donnees nettoyees (tibble ou objet SF)

- deletions: Tableau des suppressions avec raisons

- stats: Statistiques de synthese

## Examples

``` r
if (FALSE) { # \dontrun{
# Avec un fichier
result <- clean_yield_with_tracking("data.txt", metrique = TRUE, polygon = TRUE)

# Avec des donnees en memoire
result <- clean_yield_with_tracking(data = my_data, metrique = TRUE, polygon = TRUE)

print(result$stats)
head(result$deletions)
} # }
```
