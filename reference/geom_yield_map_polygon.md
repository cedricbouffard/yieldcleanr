# Creer une carte de rendement avec des polygones ggplot

Cette fonction genere des elements ggplot pour visualiser les donnees de
rendement sous forme de carte thematique avec des polygones colores
selon les classes de rendement.

## Usage

``` r
geom_yield_map_polygon(
  rdt_data,
  breaks = NULL,
  n_bins = 7,
  col_palette = NULL,
  line_color = "black",
  line_alpha = 0.1,
  line_size = 0.05
)
```

## Arguments

- rdt_data:

  Objet sf contenant les donnees de rendement avec une colonne 'yield'

- breaks:

  Vecteur numerique optionnel definissant les seuils de classification.
  Si NULL (par defaut), les seuils sont calcules automatiquement avec
  des bins de taille uniforme (0.25t, 0.5t, 1t, 1.5t ou 2t selon la
  distribution des donnees)

- n_bins:

  Nombre de bins a creer (par defaut: 7). Ignore si breaks est fourni.

- col_palette:

  Vecteur de couleurs pour les classes de rendement. Si NULL (par
  defaut), une palette est generee automatiquement avec le bin median en
  jaune, les bins inferieurs en rouge/orange et les superieurs en vert.

- line_color:

  Couleur des bordures des polygones. Par defaut : "black"

- line_alpha:

  Transparence des bordures (0-1). Par defaut : 0.1

- line_size:

  Epaisseur des bordures. Par defaut : 0.05

## Value

Une liste d'elements ggplot (geom_sf + scale_fill_manual) a ajouter a un
ggplot

## Examples

``` r
if (FALSE) { # \dontrun{
# Utilisation de base avec seuils automatiques
ggplot() +
  geom_yield_map_polygon(rdt_data) +
  theme_minimal()

# Avec seuils personnalises
breaks <- c(5000, 8000, 12000, 15000, 20000)
ggplot() +
  geom_yield_map_polygon(rdt_data, breaks = breaks) +
  theme_minimal()

# Avec palette de couleurs personnalisee
ggplot() +
  geom_yield_map_polygon(
    rdt_data,
    col_palette = c("red", "orange", "yellow", "green", "blue")
  ) +
  theme_minimal()
} # }
```
