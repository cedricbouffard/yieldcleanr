# Restaurer les coordonnées originales à partir de données anonymisées

Cette fonction restaure les coordonnées GPS originales en utilisant la
clé de décalage sauvegardée lors de l'anonymisation.

## Usage

``` r
restore_coordinates(
  data,
  key_file = NULL,
  password = NULL,
  key_info = NULL,
  coordinate_system = "latlon"
)
```

## Arguments

- data:

  Tibble ou data.frame contenant les coordonnées anonymisées

- key_file:

  Chemin du fichier contenant la clé chiffrée

- password:

  Mot de passe pour déchiffrer le fichier de clé

- key_info:

  Liste contenant les informations de clé (alternative à key_file)

- coordinate_system:

  Système de coordonnées utilisé lors de l'anonymisation

## Value

Tibble avec les coordonnées restaurées

## Details

Cette fonction est l'inverse de
[`anonymize_coordinates()`](https://cedricbouffard.github.io/yieldcleanr/reference/anonymize_coordinates.md).
Elle lit le fichier de clé chiffré, extrait les informations de décalage
et les applique en sens inverse pour retrouver les coordonnées
originales.

## See also

[`anonymize_coordinates()`](https://cedricbouffard.github.io/yieldcleanr/reference/anonymize_coordinates.md)
pour anonymiser les coordonnées

## Examples

``` r
if (FALSE) { # \dontrun{
# Restaurer à partir d'un fichier de clé
restored_data <- restore_coordinates(
  anonymized_data,
  key_file = "reference_key.enc",
  password = "mon_mot_de_passe_securise"
)
} # }
```
