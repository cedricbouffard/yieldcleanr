# Pipeline complet d'anonymisation des donnees de rendement

Cette fonction applique l'ensemble des etapes d'anonymisation:

1.  Anonymisation spatiale (coordonnees)

2.  Suppression des attributs sensibles

3.  Chiffrement et stockage securise de la cle

## Usage

``` r
anonymize_yield_data(
  data,
  key_file = NULL,
  password = NULL,
  env_var = "YIELD_ANON_PASSWORD",
  columns_to_remove = c("Serial", "FieldID", "LoadID", "GPS_Time"),
  coordinate_system = "latlon",
  output_dir = NULL,
  verbose = TRUE
)
```

## Arguments

- data:

  Tibble avec les donnees de rendement

- key_file:

  Chemin du fichier pour stocker la cle de deplacement

- password:

  Mot de passe pour chiffrer la cle (ou NULL pour utiliser une variable
  d'environnement)

- env_var:

  Nom de la variable d'environnement contenant le mot de passe (defaut:
  "YIELD_ANON_PASSWORD")

- columns_to_remove:

  Colonnes sensibles a supprimer

- coordinate_system:

  Systeme de coordonnees: "latlon" ou "utm"

- output_dir:

  Repertoire de sortie pour les donnees anonymisees

- verbose:

  Si TRUE, affiche les informations de progression

## Value

Une liste contenant:

- data: Donnees anonymisees

- key_file: Chemin du fichier de cle

- report: Rapport de securite

- metadata: Metadonnees de l'anonymisation

## See also

[`anonymize_coordinates()`](https://cedricbouffard.github.io/yieldcleanr/reference/anonymize_coordinates.md),
[`remove_sensitive_attributes()`](https://cedricbouffard.github.io/yieldcleanr/reference/remove_sensitive_attributes.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Anonymisation complete avec mot de passe depuis variable d'environnement
Sys.setenv(YIELD_ANON_PASSWORD = "mon_mot_de_passe_securise")

result <- anonymize_yield_data(
  data,
  key_file = "chemin/vers/cle.enc",
  output_dir = "donnees/anonymisees"
)

# Sauvegarder les donnees anonymisees
write.csv(result$data, file.path(result$output_dir, "rendement_anon.csv"))
} # }
```
