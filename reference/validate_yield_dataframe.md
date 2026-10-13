# Valider la structure d'un jeu de donnees de rendement

Verifie qu'un objet est un data frame non vide et possede les colonnes
requises. Fournit un message d'erreur clair avec suggestions en cas de
colonnes manquantes.

## Usage

``` r
validate_yield_dataframe(data, required_cols = NULL, arg_name = "data")
```

## Arguments

- data:

  Objet a verifier (data.frame, tibble ou sf)

- required_cols:

  Vecteur des noms de colonnes obligatoires (defaut NULL)

- arg_name:

  Nom du parametre dans la fonction appelante (defaut "data")

## Value

Invisible TRUE si valide, sinon leve une erreur informative

## Examples

``` r
df <- data.frame(Longitude = -73.5, Latitude = 45.5, Flow = 10)
validate_yield_dataframe(df, required_cols = c("Longitude", "Latitude"))
```
