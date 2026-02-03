# Méta-fonction de détection d'anomalies

Cette fonction unifiée détecte et filtre différents types d'anomalies
dans les données de rendement : chevauchements, écarts-types locaux,
changements brusques de vitesse, anomalies de direction et points hors
champ.

## Usage

``` r
detect_anomalies(data, type = "all", action = "filter", ...)
```

## Arguments

- data:

  Tibble avec données de rendement

- type:

  Type d'anomalie à détecter. Peut être un vecteur. Options: "overlap"
  (chevauchement), "local_sd" (écart-type local), "velocity_jump"
  (changement de vitesse), "heading" (direction), "position" (position),
  "all" (toutes)

- action:

  Action à effectuer: "filter" (filtrer les anomalies), "detect"
  (marquer sans filtrer), ou "report" (rapport uniquement). Défaut:
  "filter"

- ...:

  Paramètres spécifiques au type d'anomalie:

  - overlap: cellsize (défaut: 0.3), max_pass (défaut: 50)

  - local_sd: n_swaths (défaut: 5), lsd_limit (défaut: 2.4), min_cells
    (défaut: 3)

  - velocity_jump: max_acceleration (défaut: 5), max_deceleration
    (défaut: -8)

  - heading: max_heading_change (défaut: 60)

  - position: gbuffer (défaut: 100)

## Value

Selon l'action: data filtré, data avec colonnes de marquage, ou rapport

## Examples

``` r
if (FALSE) { # \dontrun{
# Détecter et filtrer toutes les anomalies
data_clean <- detect_anomalies(data, type = "all")

# Détecter uniquement les chevauchements avec paramètres personnalisés
data_clean <- detect_anomalies(data, type = "overlap",
                               cellsize = 0.5, max_pass = 30)

# Marquer sans filtrer
data_marked <- detect_anomalies(data, type = c("overlap", "local_sd"),
                                action = "detect")
} # }
```
