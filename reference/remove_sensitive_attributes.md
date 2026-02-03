# Supprimer les attributs sensibles des donnees de rendement

Cette fonction supprime les colonnes identifiantes et sensibles des
donnees de rendement pour renforcer l'anonymisation. Elle peut egalement
generaliser certaines colonnes (arrondir, categoriser) plutot que de les
supprimer completement.

## Usage

``` r
remove_sensitive_attributes(
  data,
  columns_to_remove = c("Serial", "FieldID", "LoadID", "GPS_Time"),
  generalize_cols = NULL,
  keep_temporal = FALSE,
  verbose = TRUE
)
```

## Arguments

- data:

  Tibble ou data.frame avec les donnees de rendement

- columns_to_remove:

  Vecteur des noms de colonnes a supprimer. Par defaut: c("Serial",
  "FieldID", "LoadID", "GPS_Time")

- generalize_cols:

  Liste nommee definissant les colonnes a generaliser au lieu de
  supprimer. Ex: list(GPS_Time = "hour", Pass = "none")

- keep_temporal:

  Si TRUE, conserve une version temporelle relative (minutes depuis le
  debut) au lieu de supprimer GPS_Time completement

- verbose:

  Si TRUE, affiche des informations sur les colonnes traitees

## Value

Tibble avec les colonnes sensibles supprimees ou generalisees

## Details

Les colonnes suivantes sont considerees comme sensibles par defaut:

- Serial: Numero de serie de la machine (identifiant unique)

- FieldID: Identifiant du champ (peut reveler la propriete)

- LoadID: Identifiant de la charge (lien avec d'autres donnees)

- GPS_Time: Horodatage precis (peut etre croise avec d'autres sources)

Options de generalisation pour GPS_Time:

- "none": Supprime completement

- "hour": Garde uniquement l'heure (sans minutes/secondes)

- "day": Garde uniquement la date (sans heure)

- "relative": Convertit en minutes ecoulees depuis le debut

## See also

[`anonymize_coordinates()`](https://cedricbouffard.github.io/yieldcleanr/reference/anonymize_coordinates.md)
pour l'anonymisation spatiale

## Examples

``` r
if (FALSE) { # \dontrun{
# Supprimer les colonnes sensibles par defaut
data_clean <- remove_sensitive_attributes(data)

# Supprimer des colonnes specifiques
data_clean <- remove_sensitive_attributes(
  data,
  columns_to_remove = c("Serial", "FieldID", "Variety")
)

# Generaliser plutot que supprimer
data_clean <- remove_sensitive_attributes(
  data,
  generalize_cols = list(GPS_Time = "hour", Pass = "none"),
  columns_to_remove = c("Serial", "FieldID", "LoadID")
)
} # }
```
