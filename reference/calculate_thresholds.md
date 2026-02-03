# Méta-fonction de calcul des seuils

Cette fonction calcule automatiquement les seuils pour le rendement, la
vitesse, la position et l'humidité en utilisant des méthodes
statistiques robustes.

## Usage

``` r
calculate_thresholds(data, type = "all", ...)
```

## Arguments

- data:

  Tibble avec données de rendement

- type:

  Type de seuil à calculer. Options: "yield", "velocity", "position",
  "moisture", "all". Défaut: "all"

- ...:

  Paramètres de calcul:

  - yield: yllim (quantile bas, défaut 0.10), yulim (quantile haut,
    défaut 0.90), yscale (multiplicateur IQR, défaut 1.1), min_yield_abs
    (défaut 0)

  - velocity: vllim (quantile bas, défaut 0.05), vulim (quantile haut,
    défaut 0.95), vscale (multiplicateur IQR, défaut 1.1),
    min_velocity_abs (défaut 0.5)

  - position: gbuffer (marge en mètres, défaut 100)

  - moisture: n_std (nombre d'écarts-types, défaut 3)

## Value

Liste avec les seuils calculés pour chaque type demandé

## Examples

``` r
if (FALSE) { # \dontrun{
# Calculer tous les seuils
thresholds <- calculate_thresholds(data)

# Calculer uniquement les seuils de rendement personnalisés
thresholds <- calculate_thresholds(data, type = "yield",
                                  yllim = 0.05, yulim = 0.95, yscale = 1.5)
} # }
```
