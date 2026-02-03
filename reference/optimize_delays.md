# Méta-fonction d'optimisation des délais

Cette fonction optimise les délais temporels entre les capteurs (flux et
humidité) en utilisant la méthode de delay adjustment (Delay
Adjustment).

## Usage

``` r
optimize_delays(
  data,
  type = "both",
  method = "delay_adjustment",
  delay_range = -25:25,
  n_iterations = 10,
  noise_level = 0.03,
  apply_correction = TRUE
)
```

## Arguments

- data:

  Tibble avec données de rendement

- type:

  Type de délai à optimiser: "flow" (flux), "moisture" (humidité), ou
  "both" (les deux). Défaut: "both"

- method:

  Méthode d'optimisation. Défaut: "delay_adjustment"

- delay_range:

  Plage de délais à tester en secondes. Défaut: -25:25

- n_iterations:

  Nombre d'itérations pour la stabilité. Défaut: 10

- noise_level:

  Niveau de bruit ajouté. Défaut: 0.03

- apply_correction:

  Si TRUE, applique la correction de délai aux données. Défaut: TRUE

## Value

Liste contenant:

- data: Données corrigées (si apply_correction = TRUE)

- delays: Délai(s) optimal(aux) trouvé(s)

- delay_adjustment_results: Résultats détaillés de l'optimisation

## Examples

``` r
if (FALSE) { # \dontrun{
# Optimiser les deux délais et appliquer les corrections
result <- optimize_delays(data, type = "both")
data_corrected <- result$data

# Optimiser uniquement le délai de flux sans appliquer
result <- optimize_delays(data, type = "flow", apply_correction = FALSE)
print(result$delays$flow)
} # }
```
