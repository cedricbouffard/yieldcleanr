# Fonction unifiee de nettoyage des donnees de rendement

Cette fonction execute le pipeline complet de nettoyage des donnees de
rendement avec support pour les sorties en unites metriques ou
imperiales, et avec ou sans geometries SF (polygones ou points).

## Usage

``` r
clean_yield(
  file_path = NULL,
  data = NULL,
  metrique = TRUE,
  polygon = TRUE,
  params = NULL,
  output_file = NULL,
  log_file = NULL
)
```

## Arguments

- file_path:

  Chemin du fichier d'entree (txt/csv). Ignore si `data` est fourni.

- data:

  Data frame ou tibble contenant les donnees de rendement. Alternative a
  `file_path`.

- metrique:

  TRUE pour les unites metriques (kg/ha), FALSE pour l'imperial
  (bu/acre)

- polygon:

  TRUE pour une sortie SF en polygones, FALSE pour une sortie tibble

- params:

  Liste des parametres AYCE (voir section "Parametres AYCE")

- output_file:

  Chemin optionnel pour sauvegarder la sortie (CSV ou GeoJSON)

- log_file:

  Chemin optionnel pour sauvegarder le journal de nettoyage

## Value

Un objet sf (si polygon = TRUE) ou un tibble (si polygon = FALSE)
contenant les donnees nettoyees avec les colonnes :

- Coordonnees X, Y (metres UTM ou lat/lon)

- Rendement (kg/ha ou bu/acre selon l'option metrique)

- Humidite (%)

- Vitesse (m/s)

- Colonnes de statut indiquant les filtres appliques

## Details

La fonction implemente un pipeline de nettoyage en plusieurs etapes :

1.  **Lecture des donnees** : Import des fichiers TXT/CSV avec detection
    automatique du format et des colonnes

2.  **Ajustement des delais** : Optimisation automatique des decalages
    temporels entre capteurs et GPS (delay adjustment)

3.  **Filtrage par position** : Elimination des points hors limites du
    champ

4.  **Filtrage de vitesse** : Suppression des points avec vitesse
    anormale

5.  **Filtrage de rendement** : Suppression des valeurs de rendement
    aberrantes

6.  **Filtrage de chevauchement** : Detection et elimination des zones
    de chevauchement entre passages

7.  **Filtrage par ecart-type local** : Suppression des points
    statistiquement aberrants dans leur voisinage

8.  **Export** : Generation de la sortie au format demande (tibble ou
    sf)

## Parametres AYCE

La liste `params` permet de personnaliser le comportement du moteur AYCE
:

- delay_range:

  Vecteur de valeurs de delai a tester (en secondes, defaut: -25:25)

- n_iterations:

  Nombre d'iterations pour l'optimisation (defaut: 10)

- noise_level:

  Niveau de bruit pour le lissage (defaut: 0.03)

- yllim, yulim:

  Quantiles pour les limites de rendement (defaut: 0.10, 0.90)

- yscale:

  Multiplicateur IQR pour le rendement (defaut: 1.1)

- v_lim, v_ulim:

  Quantiles pour les limites de vitesse (defaut: 0.05, 0.95)

- cellsize_overlap:

  Taille des cellules pour le filtre de chevauchement (defaut: 0.3m)

- overlap_threshold:

  Seuil de chevauchement maximum (defaut: 0.4)

- n_swaths:

  Nombre de largeurs de passage pour l'ecart-type local (defaut: 5)

- lsd_limit:

  Multiplicateur pour l'ecart-type local (defaut: 2.4)

## See also

[`clean_yield_fast`](https://cedricbouffard.github.io/yieldcleanr/reference/clean_yield_fast.md)
pour une version optimisee pour de grands jeux de donnees,
[`clean_yield_with_tracking`](https://cedricbouffard.github.io/yieldcleanr/reference/clean_yield_with_tracking.md)
pour conserver l'historique des points filtres,
[`read_yield_data`](https://cedricbouffard.github.io/yieldcleanr/reference/read_yield_data.md)
pour seulement importer les donnees,
[`launch_shiny_app`](https://cedricbouffard.github.io/yieldcleanr/reference/launch_shiny_app.md)
pour une interface graphique interactive.

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

# Utilisation avec un data frame deja charge
data <- read_yield_data("data.txt")
result <- clean_yield(data = data, metrique = TRUE, polygon = TRUE)

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

# Avec export automatique
clean_yield("data.txt", 
  metrique = TRUE, 
  polygon = TRUE,
  output_file = "output/cleaned.geojson",
  log_file = "output/cleaning_log.txt"
)
} # }
```
