# Supprimer les points en chevauchement

Cette fonction identifie et élimine les points de chevauchement en
utilisant une grille cellsize x cellsize. Les points dans les cellules
avec PLUS de max_pass sont considérés comme du chevauchement et
éliminés.

## Usage

``` r
remove_overlap(data, cellsize = 0.3, max_pass = 50)
```

## Arguments

- data:

  Tibble avec donnees (colonnes X, Y en UTM)

- cellsize:

  Taille des cellules en metres (defaut 0.3m = 30cm)

- max_pass:

  Nombre max de passages avant chevauchement (defaut 50)

## Value

Tibble filtre sans chevauchement

## Examples

``` r
if (FALSE) { # \dontrun{
# Creer des donnees d'exemple en UTM
data <- tibble::tibble(
  X = c(435000, 435050, 435100, 435000, 435050),
  Y = c(5262000, 5262050, 5262100, 5262150, 5262200),
  Flow = c(2.5, 3.1, 2.8, 3.0, 2.9)
)

# Parametres par defaut (methode USDA)
data_clean <- remove_overlap(data, cellsize = 0.3, max_pass = 50)

# Pour des donnees avec beaucoup de chevauchement
data_clean <- remove_overlap(data, cellsize = 0.3, max_pass = 30)
} # }
```
