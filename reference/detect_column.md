# Detecter une colonne parmi une liste de candidats possibles

Cherche sans sensibilite a la casse ou separateurs une colonne existante
parmi une liste de noms alternatifs.

## Usage

``` r
detect_column(data, candidates, default = NULL)
```

## Arguments

- data:

  Data frame

- candidates:

  Vecteur de chaines de caracteres des noms recherches

- default:

  Valeur par defaut si non trouvee (defaut NULL)

## Value

Le nom exact de la colonne trouvee dans data, ou default

## Examples

``` r
df <- data.frame(Yield_kg_ha = c(5000, 6000))
detect_column(df, c("yield", "flow", "yield_kg_ha"))
#> [1] "Yield_kg_ha"
```
