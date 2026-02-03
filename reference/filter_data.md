# Méta-fonction de filtrage unifiée

Cette fonction unifiée permet d'appliquer un ou plusieurs filtres sur
les données de rendement. Elle remplace les fonctions individuelles
filter_header_status(), filter_gps_status(), filter_velocity(),
filter_yield_range(), filter_moisture_range(), filter_dop() et
filter_bounds().

## Usage

``` r
filter_data(data, type = "all", ...)
```

## Arguments

- data:

  Tibble avec données de rendement

- type:

  Type de filtre à appliquer. Peut être un vecteur pour appliquer
  plusieurs filtres en séquence. Options: "header", "gps", "dop",
  "velocity", "yield", "moisture", "bounds", "all"

- ...:

  Paramètres spécifiques au type de filtre:

  - header: header_values (défaut: c(1, 33))

  - gps: min_gps_status (défaut: 4)

  - dop: max_dop (défaut: 10)

  - velocity: min_velocity, max_velocity (défaut: auto-calculé)

  - yield: min_yield, max_yield, n_std (défaut: auto-calculé avec 3 SD)

  - moisture: min_moisture, max_moisture, n_std (défaut: auto-calculé
    avec 3 SD)

  - bounds: bounds (liste avec min_x, max_x, min_y, max_y), coord_type
    ("utm" ou "latlon")

## Value

Tibble filtré selon les critères spécifiés

## Examples

``` r
if (FALSE) { # \dontrun{
# Filtrer uniquement le header
data_filtered <- filter_data(data, type = "header")

# Filtrer header et GPS
data_filtered <- filter_data(data, type = c("header", "gps"))

# Appliquer tous les filtres disponibles
data_filtered <- filter_data(data, type = "all")

# Filtrer avec paramètres personnalisés
data_filtered <- filter_data(data, type = "velocity",
                             min_velocity = 1.0, max_velocity = 5.0)
} # }
```
