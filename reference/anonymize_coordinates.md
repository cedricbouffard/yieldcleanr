# Anonymiser les coordonnées GPS des données de rendement

Cette fonction déplace les coordonnées GPS (Latitude/Longitude) en les
décalant selon un point de référence choisi aléatoirement. Le point de
référence est sauvegardé dans un fichier chiffré pour permettre la
réversibilité si nécessaire.

## Usage

``` r
anonymize_coordinates(
  data,
  output_key_file = NULL,
  password = NULL,
  algorithm = "aes-256-gcm",
  random_seed = NULL,
  coordinate_system = "latlon",
  utm_zone = NULL,
  return_full_key = FALSE
)
```

## Arguments

- data:

  Tibble ou data.frame contenant les colonnes Latitude et Longitude

- output_key_file:

  Chemin du fichier où sauvegarder la clé de décalage chiffrée. Si NULL,
  la clé est retournée dans l'objet résultat mais pas sauvegardée.

- password:

  Mot de passe pour chiffrer/déchiffrer le fichier de clé. Si NULL, une
  clé aléatoire est générée et retournée (non chiffrée).

- algorithm:

  Algorithme de chiffrement à utiliser. Options: "aes-256-gcm" (défaut),
  "aes-256-cbc", "chacha20". Voir
  [`openssl::encrypt()`](https://jeroen.r-universe.dev/openssl/reference/rsa_encrypt.html)
  pour les options.

- random_seed:

  Graine aléatoire pour la reproductibilité (optionnel). Si NULL,
  utilise une graine aléatoire.

- coordinate_system:

  Système de coordonnées à utiliser pour le décalage. Options: "latlon"
  (défaut) pour Latitude/Longitude, "utm" pour coordonnées UTM.

- utm_zone:

  Zone UTM si coordinate_system = "utm". Auto-détectée si NULL.

- return_full_key:

  Si TRUE, retourne la clé complète dans le résultat (défaut: FALSE pour
  ne pas exposer accidentellement la clé).

## Value

Une liste contenant:

- `data`: Les données avec coordonnées anonymisées

- `key_file`: Chemin du fichier de clé chiffrée (si sauvegardé)

- `offset`: Le décalage appliqué (lat_offset, lon_offset ou x_offset,
  y_offset)

- `reference_point`: Le point de référence original (si return_full_key
  = TRUE)

- `metadata`: Métadonnées sur l'anonymisation (timestamp, méthode, etc.)

## Details

La méthode d'anonymisation choisie consiste à:

1.  Sélectionner un point de référence aléatoire dans les données

2.  Calculer un décalage pour que ce point devienne l'origine (0,0) ou
    un point fixe

3.  Appliquer ce décalage à toutes les coordonnées

4.  Sauvegarder le point de référence et le décalage dans un fichier
    chiffré

Cette approche permet:

- L'anonymisation: les coordonnées absolues sont perdues

- La réversibilité: avec la clé, on peut retrouver les coordonnées
  originales

- La cohérence: toutes les données du même champ ont le même décalage

## Sécurité

Le fichier de clé est chiffré avec AES-256-GCM par défaut, qui offre:

- Confidentialité: les données sont illisibles sans le mot de passe

- Authenticité: détection de toute modification du fichier

Pour une sécurité maximale:

- Utilisez un mot de passe fort (12+ caractères, mixte)

- Stockez le fichier de clé séparément des données

- Ne partagez jamais le mot de passe et le fichier de clé ensemble

## See also

[`restore_coordinates()`](https://cedricbouffard.github.io/yieldcleanr/reference/restore_coordinates.md)
pour restaurer les coordonnées originales

## Examples

``` r
if (FALSE) { # \dontrun{
# Créer des données d'exemple
data <- tibble::tibble(
  Latitude = c(47.506122, 47.506136, 47.506152),
  Longitude = c(-69.856661, -69.856681, -69.856701),
  Flow = c(1.53, 3.7, 7.56)
)

# Anonymiser avec sauvegarde de clé chiffrée
result <- anonymize_coordinates(
  data,
  output_key_file = "reference_key.enc",
  password = "mon_mot_de_passe_securise"
)

# Données anonymisées
anonymized_data <- result$data

# Restaurer les coordonnées originales
restored <- restore_coordinates(
  anonymized_data,
  key_file = "reference_key.enc",
  password = "mon_mot_de_passe_securise"
)
} # }
```
