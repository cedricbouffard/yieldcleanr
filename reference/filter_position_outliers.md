# Filtre de position pour eliminer les points hors champ

Detecte et supprime les points qui sont en dehors du champ principal en
utilisant une methode de buffer autour du centre du champ. Seuls les
points dans les zones avec suffisamment de points voisins sont
conserves.

## Usage

``` r
filter_position_outliers(
  data,
  buffer_radius = 50,
  min_points_cell = 5,
  grid_size = 20
)
```

## Arguments

- data:

  Tibble avec au minimum X, Y

- buffer_radius:

  Rayon du buffer en metres (defaut: 50)

- min_points_cell:

  Nombre minimum de points par cellule pour qu'une zone soit valide
  (defaut: 5)

- grid_size:

  Taille de la grille pour l'analyse en metres (defaut: 20)

## Value

Liste avec data (donnees filtrees) et removed (points supprimes)
