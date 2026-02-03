# Guide des Méta-fonctions

## Introduction

Cette vignette présente la nouvelle API simplifiée de `yieldcleanr`
basée sur des **méta-fonctions**. Au lieu d’avoir à retenir des dizaines
de fonctions individuelles, vous pouvez maintenant utiliser un petit
nombre de fonctions puissantes et flexibles.

### Pourquoi une nouvelle API ?

L’ancienne API avait **46 fonctions exportées**, ce qui rendait : - La
découverte des fonctions difficile - La documentation dispersée -
L’apprentissage long pour les nouveaux utilisateurs

La nouvelle API n’a que **24 fonctions exportées** organisées autour de
**8 méta-fonctions** principales.

## Les 8 Méta-fonctions

### 1. `filter_data()` - Tous les filtres en un seul appel

Cette fonction remplace toutes les fonctions de filtrage individuelles
: - `filter_header_status()` - `filter_gps_status()` - `filter_dop()` -
`filter_velocity()` - `filter_yield_range()` -
`filter_moisture_range()` - `filter_bounds()`

#### Exemples d’utilisation

``` r
library(yieldcleanr)
library(dplyr)

# Créer des données de test
data <- tibble::tibble(
  HeaderStatus = c(1L, 33L, 33L, 0L, 1L),
  GPSStatus = c(2L, 4L, 4L, 7L, 4L),
  DOP = c(5L, 15L, 8L, 5L, 10L),
  Yield_kg_ha = c(6270, 9405, 3135, 18810, 11286),
  Moisture = c(15, 16, 15, 16, 15),
  X = c(435000, 435010, 435020, 435030, 435040),
  Y = c(5262000, 5262010, 5262020, 5262030, 5262040),
  Interval = c(2L, 2L, 2L, 2L, 2L),
  Flow = 1:5
)

# Appliquer un seul filtre
data_filtered <- filter_data(data, type = "header")
cat("Après filtre header:", nrow(data_filtered), "points\n")
#> Après filtre header: 4 points

# Appliquer plusieurs filtres
data_multi <- filter_data(data, type = c("header", "gps"))
cat("Après filtres header + gps:", nrow(data_multi), "points\n")
#> Après filtres header + gps: 3 points

# Appliquer tous les filtres disponibles
data_all <- filter_data(data, type = "all")
cat("Après tous les filtres:", nrow(data_all), "points\n")
#> Après tous les filtres: 0 points
```

#### Paramètres personnalisés

``` r
# Filtre de rendement avec plage personnalisée
data_yield <- filter_data(data, 
                          type = "yield",
                          min_yield = 3000,
                          max_yield = 15000)

# Filtre de vitesse avec seuils personnalisés
data_velocity <- filter_data(data,
                             type = "velocity",
                             min_velocity = 0.5,
                             max_velocity = 8.0)

# Filtre GPS avec statut minimum personnalisé
data_gps <- filter_data(data,
                       type = "gps",
                       min_gps_status = 5)
```

### 2. `detect_anomalies()` - Détection d’anomalies unifiée

Cette fonction remplace toutes les fonctions de détection d’anomalies
: - `remove_overlap()` - `filter_local_std()` -
`filter_velocity_jumps()` - `filter_heading_anomalies()` -
`filter_position_outliers()` - `apply_overlap_filter()` -
`apply_local_sd_filter()`

#### Exemples d’utilisation

``` r
# Créer des données avec anomalies
data <- tibble::tibble(
  X = c(435000, 435001, 435002, 435003, 435100),
  Y = c(5262000, 5262001, 5262002, 5262003, 5262100),
  Flow = c(50, 55, 300, 52, 48),  # 300 est une anomalie
  Swath = rep(240, 5),
  Interval = rep(2, 5),
  GPS_Time = 1:5
)

# Détecter les chevauchements
data_clean <- detect_anomalies(data, type = "overlap", cellsize = 0.3, max_pass = 2)

# Détecter les outliers locaux
data_clean <- detect_anomalies(data, type = "local_sd", n_swaths = 5, lsd_limit = 2.4)

# Détecter les changements brusques de vitesse
data_clean <- detect_anomalies(data, type = "velocity_jump", max_acceleration = 5)

# Détecter les anomalies de direction
data_clean <- detect_anomalies(data, type = "heading", max_heading_change = 60)

# Détecter toutes les anomalies
data_clean <- detect_anomalies(data, type = "all")
```

#### Mode “detect” vs “filter”

``` r
# Mode "filter" (défaut) : filtre les anomalies
data_filtered <- detect_anomalies(data, type = "local_sd", action = "filter")

# Mode "detect" : marque sans filtrer
data_marked <- detect_anomalies(data, type = "local_sd", action = "detect")
# Ajoute une colonne 'local_sd_outlier' avec TRUE/FALSE
```

### 3. `calculate_thresholds()` - Calcul des seuils unifié

Cette fonction remplace
[`calculate_auto_thresholds()`](https://cedricbouffard.github.io/yieldcleanr/reference/calculate_auto_thresholds.md)
et offre plus de flexibilité.

#### Exemples d’utilisation

``` r
# Créer des données
data <- tibble::tibble(
  Yield_kg_ha = c(6270, 7524, 8778, 10032, 11286, 6897, 8142, 9405),
  X = 435000 + 1:8,
  Y = 5262000 + 1:8,
  Moisture = c(15, 16, 15, 16, 15, 16, 15, 16)
)

# Calculer tous les seuils
thresholds <- calculate_thresholds(data, type = "all")
print(thresholds)
#> $yield
#> $yield$min_yield
#> [1] 2639.67
#> 
#> $yield$max_yield
#>      90% 
#> 14477.43 
#> 
#> $yield$mean_yield
#> [1] 8541.75
#> 
#> $yield$sd_yield
#> [1] 1673.936
#> 
#> 
#> $velocity
#> $velocity$min_velocity
#> [1] 1.414214
#> 
#> $velocity$max_velocity
#>      95% 
#> 1.414214 
#> 
#> $velocity$mean_velocity
#> [1] 1.414214
#> 
#> $velocity$sd_velocity
#> [1] 0
#> 
#> 
#> $position
#> $position$min_x
#> [1] 435101
#> 
#> $position$max_x
#> [1] 434908
#> 
#> $position$min_y
#> [1] 5262101
#> 
#> $position$max_y
#> [1] 5261908
#> 
#> $position$buffer
#> [1] 100
#> 
#> 
#> $moisture
#> $moisture$min_moisture
#> [1] 13.89643
#> 
#> $moisture$max_moisture
#> [1] 17.10357
#> 
#> $moisture$mean_moisture
#> [1] 15.5
#> 
#> $moisture$sd_moisture
#> [1] 0.5345225

# Calculer seulement les seuils de rendement
thresholds_yield <- calculate_thresholds(data, type = "yield")
cat("Seuils de rendement:", thresholds_yield$yield$min_yield, "-", 
    thresholds_yield$yield$max_yield, "kg/ha\n")
#> Seuils de rendement: 2639.67 - 14477.43 kg/ha

# Calculer avec des paramètres personnalisés
thresholds_custom <- calculate_thresholds(data, type = "yield",
                                          yllim = 0.05, yulim = 0.95, yscale = 1.5)
```

### 4. `optimize_delays()` - Optimisation des délais unifiée

Cette fonction remplace `apply_flow_delay()` et
[`apply_moisture_delay()`](https://cedricbouffard.github.io/yieldcleanr/reference/apply_moisture_delay.md)
et offre une interface plus simple pour l’optimisation du delay
adjustment.

#### Exemples d’utilisation

``` r
# Créer des données avec décalage temporel
data <- tibble::tibble(
  Flow = c(10, 15, 12, 18, 14, 16, 13, 17, 11, 19),
  Moisture = c(15, 16, 15, 16, 15, 16, 15, 16, 15, 16),
  GPS_Time = 1:10,
  X = 435000 + 1:10,
  Y = 5262000 + 1:10,
  Interval = rep(2L, 10)
)

# Optimiser seulement le délai de flux
result <- optimize_delays(data, type = "flow", delay_range = -3:3, n_iterations = 2)
cat("Délai optimal flux:", result$delays$flow, "secondes\n")
#> Délai optimal flux:  secondes

# Optimiser seulement le délai d'humidité
result <- optimize_delays(data, type = "moisture", delay_range = -3:3, n_iterations = 2)
cat("Délai optimal humidité:", result$delays$moisture, "secondes\n")
#> Délai optimal humidité:  secondes

# Optimiser les deux délais
result <- optimize_delays(data, type = "both", delay_range = -3:3, n_iterations = 2)
cat("Délais optimaux - Flux:", result$delays$flow, 
    "Humidité:", result$delays$moisture, "secondes\n")
#> Délais optimaux - Flux:  Humidité:  secondes

# Optimiser et appliquer automatiquement les corrections
result <- optimize_delays(data, type = "flow", 
                          delay_range = -3:3, 
                          n_iterations = 2,
                          apply_correction = TRUE)
data_corrected <- result$data
```

### 5. `convert_coordinates()` - Conversion de coordonnées

Cette fonction offre une interface unifiée pour la conversion de
coordonnées.

#### Exemples d’utilisation

``` r
# Données en Lat/Lon
data_latlon <- tibble::tibble(
  Longitude = c(-69.856661, -69.856681),
  Latitude = c(47.506122, 47.506136)
)

# Convertir vers UTM
data_utm <- convert_coordinates(data_latlon, from = "latlon", to = "utm")
print(data_utm)
#> # A tibble: 2 × 4
#>   Longitude Latitude       X        Y
#>       <dbl>    <dbl>   <dbl>    <dbl>
#> 1     -69.9     47.5 435490. 5261766.
#> 2     -69.9     47.5 435488. 5261767.

# Convertir vers UTM avec zone spécifique
data_utm <- convert_coordinates(data_latlon, from = "latlon", to = "utm", zone = 18)
```

### 6. `convert_yield_units()` - Conversion de rendement

Cette fonction remplace
[`convert_flow_to_yield()`](https://cedricbouffard.github.io/yieldcleanr/reference/convert_flow_to_yield.md)
et offre plus d’options de conversion.

#### Exemples d’utilisation

``` r
# Données avec flux en lbs/s
data <- tibble::tibble(
  Flow = c(50, 55, 52),
  Interval = c(2, 2, 2),
  Distance = c(87, 87, 87),
  Swath = c(240, 240, 240)
)

# Convertir flux (lbs/s) vers kg/ha
data_yield <- convert_yield_units(data, from = "flow_lbs_s", to = "kg_ha")
print(data_yield)
#> # A tibble: 3 × 6
#>    Flow Interval Distance Swath Yield_kg_ha_wet Yield_kg_ha
#>   <dbl>    <dbl>    <dbl> <dbl>           <dbl>       <dbl>
#> 1    50        2       87   240            234.        234.
#> 2    55        2       87   240            257.        257.
#> 3    52        2       87   240            243.        243.

# Convertir kg/ha vers bu/acre (maïs)
data_imperial <- convert_yield_units(data_yield, from = "kg_ha", to = "bu_acre", crop_type = "maize")
print(data_imperial)
#> # A tibble: 3 × 7
#>    Flow Interval Distance Swath Yield_kg_ha_wet Yield_kg_ha Yield_bu_acre
#>   <dbl>    <dbl>    <dbl> <dbl>           <dbl>       <dbl>         <dbl>
#> 1    50        2       87   240            234.        234.          3.72
#> 2    55        2       87   240            257.        257.          4.09
#> 3    52        2       87   240            243.        243.          3.87

# Convertir vers tonnes/ha
data_tons <- convert_yield_units(data_yield, from = "kg_ha", to = "t_ha")
print(data_tons)
#> # A tibble: 3 × 7
#>    Flow Interval Distance Swath Yield_kg_ha_wet Yield_kg_ha Yield_t_ha
#>   <dbl>    <dbl>    <dbl> <dbl>           <dbl>       <dbl>      <dbl>
#> 1    50        2       87   240            234.        234.      0.234
#> 2    55        2       87   240            257.        257.      0.257
#> 3    52        2       87   240            243.        243.      0.243
```

### 7. `anonymize_data()` - Anonymisation unifiée

Cette fonction remplace
[`anonymize_coordinates()`](https://cedricbouffard.github.io/yieldcleanr/reference/anonymize_coordinates.md),
[`remove_sensitive_attributes()`](https://cedricbouffard.github.io/yieldcleanr/reference/remove_sensitive_attributes.md),
et
[`anonymize_yield_data()`](https://cedricbouffard.github.io/yieldcleanr/reference/anonymize_yield_data.md).

#### Exemples d’utilisation

``` r
# Créer des données avec informations sensibles
data <- tibble::tibble(
  X = c(435000, 435010, 435020),
  Y = c(5262000, 5262010, 5262020),
  Flow = c(50, 55, 52),
  Serial = c("ABC123", "ABC123", "ABC123"),
  FieldID = c("Field001", "Field001", "Field001")
)

# Anonymiser uniquement les coordonnées (décalage)
data_anon <- anonymize_data(data, type = "coordinates", method = "translation")
print(data_anon)
#> # A tibble: 3 × 5
#>         X        Y  Flow Serial FieldID 
#>     <dbl>    <dbl> <dbl> <chr>  <chr>   
#> 1 433380. 5271166.    50 ABC123 Field001
#> 2 433390. 5271176.    55 ABC123 Field001
#> 3 433400. 5271186.    52 ABC123 Field001

# Anonymiser uniquement les attributs sensibles
data_anon <- anonymize_data(data, type = "attributes")
print(data_anon)
#> # A tibble: 3 × 3
#>        X       Y  Flow
#>    <dbl>   <dbl> <dbl>
#> 1 435000 5262000    50
#> 2 435010 5262010    55
#> 3 435020 5262020    52

# Anonymiser complètement (coordonnées + attributs)
data_anon <- anonymize_data(data, type = "full", method = "translation")
print(data_anon)
#> # A tibble: 3 × 3
#>         X        Y  Flow
#>     <dbl>    <dbl> <dbl>
#> 1 425809. 5253735.    50
#> 2 425819. 5253745.    55
#> 3 425829. 5253755.    52

# Anonymiser avec rotation au lieu de décalage
data_anon <- anonymize_data(data, type = "coordinates", method = "rotation")
print(data_anon)
#> # A tibble: 3 × 5
#>         X        Y  Flow Serial FieldID 
#>     <dbl>    <dbl> <dbl> <chr>  <chr>   
#> 1 435024. 5262007.    50 ABC123 Field001
#> 2 435010  5262010     55 ABC123 Field001
#> 3 434996. 5262013.    52 ABC123 Field001
```

### 8. `export_data()` - Export unifié

Cette fonction remplace `export_raster()` et `save_raster()` et offre
une interface unifiée pour tous les formats d’export.

#### Exemples d’utilisation

``` r
# Créer des données
data <- tibble::tibble(
  Longitude = c(-69.856661, -69.856681),
  Latitude = c(47.506122, 47.506136),
  Yield_kg_ha = c(6270, 9405)
)

# Export CSV
temp_csv <- tempfile(fileext = ".csv")
export_data(data, temp_csv, format = "csv")
cat("Export CSV:", temp_csv, "\n")
#> Export CSV: /tmp/RtmpcNufLo/file2c5962571e68.csv

# Export GeoJSON (si sf est installé)
if (requireNamespace("sf", quietly = TRUE)) {
  data_sf <- sf::st_as_sf(data, coords = c("Longitude", "Latitude"), crs = 4326)
  temp_geojson <- tempfile(fileext = ".geojson")
  export_data(data_sf, temp_geojson)
  cat("Export GeoJSON:", temp_geojson, "\n")
}
#> Deleting source `/tmp/RtmpcNufLo/file2c59644fa784.geojson' failed
#> Writing layer `file2c59644fa784' to data source 
#>   `/tmp/RtmpcNufLo/file2c59644fa784.geojson' using driver `GeoJSON'
#> Writing 2 features with 1 fields and geometry type Point.
#> Export GeoJSON: /tmp/RtmpcNufLo/file2c59644fa784.geojson

# Export avec détection automatique du format
temp_file <- tempfile(fileext = ".csv")
export_data(data, temp_file)  # Détecte CSV depuis l'extension

# Nettoyage
unlink(temp_csv)
if (exists("temp_geojson")) unlink(temp_geojson)
unlink(temp_file)
```

## Pipeline complet avec les méta-fonctions

Voici un exemple de pipeline complet utilisant les méta-fonctions :

``` r
library(yieldcleanr)
library(dplyr)

# 1. Charger les données
file_path <- system.file("extdata", "sample1.txt", package = "yieldcleanr")
data_raw <- read_yield_data(file_path)

# 2. Convertir les coordonnées
data <- convert_coordinates(data_raw, from = "latlon", to = "utm")

# 3. Convertir le flux en rendement
data <- convert_yield_units(data, from = "flow_lbs_s", to = "kg_ha")

# 4. Calculer les seuils
thresholds <- calculate_thresholds(data, type = "all")

# 5. Appliquer les filtres de base
data <- filter_data(data, type = c("header", "gps", "velocity", "yield", "moisture"))

# 6. Détecter et filtrer les anomalies
data <- detect_anomalies(data, type = c("overlap", "local_sd"))

# 7. Optimiser les délais (si nécessaire)
# result <- optimize_delays(data, type = "both")
# data <- result$data

# 8. Anonymiser (si nécessaire)
# data <- anonymize_data(data, type = "full")

# 9. Exporter
# export_data(data, "output.csv", format = "csv")

cat("Pipeline terminé! Points finaux:", nrow(data), "\n")
#> Pipeline terminé! Points finaux: 20451
```

## Tableau récapitulatif

| Ancienne fonction                                                                                                        | Nouvelle méta-fonction                                                                                     | Paramètre `type`           |
|--------------------------------------------------------------------------------------------------------------------------|------------------------------------------------------------------------------------------------------------|----------------------------|
| `filter_header_status()`                                                                                                 | [`filter_data()`](https://cedricbouffard.github.io/yieldcleanr/reference/filter_data.md)                   | `"header"`                 |
| `filter_gps_status()`                                                                                                    | [`filter_data()`](https://cedricbouffard.github.io/yieldcleanr/reference/filter_data.md)                   | `"gps"`                    |
| `filter_dop()`                                                                                                           | [`filter_data()`](https://cedricbouffard.github.io/yieldcleanr/reference/filter_data.md)                   | `"dop"`                    |
| `filter_velocity()`                                                                                                      | [`filter_data()`](https://cedricbouffard.github.io/yieldcleanr/reference/filter_data.md)                   | `"velocity"`               |
| `filter_yield_range()`                                                                                                   | [`filter_data()`](https://cedricbouffard.github.io/yieldcleanr/reference/filter_data.md)                   | `"yield"`                  |
| `filter_moisture_range()`                                                                                                | [`filter_data()`](https://cedricbouffard.github.io/yieldcleanr/reference/filter_data.md)                   | `"moisture"`               |
| `filter_bounds()`                                                                                                        | [`filter_data()`](https://cedricbouffard.github.io/yieldcleanr/reference/filter_data.md)                   | `"bounds"`                 |
| `remove_overlap()`                                                                                                       | [`detect_anomalies()`](https://cedricbouffard.github.io/yieldcleanr/reference/detect_anomalies.md)         | `"overlap"`                |
| `filter_local_std()`                                                                                                     | [`detect_anomalies()`](https://cedricbouffard.github.io/yieldcleanr/reference/detect_anomalies.md)         | `"local_sd"`               |
| `filter_velocity_jumps()`                                                                                                | [`detect_anomalies()`](https://cedricbouffard.github.io/yieldcleanr/reference/detect_anomalies.md)         | `"velocity_jump"`          |
| `filter_heading_anomalies()`                                                                                             | [`detect_anomalies()`](https://cedricbouffard.github.io/yieldcleanr/reference/detect_anomalies.md)         | `"heading"`                |
| `filter_position_outliers()`                                                                                             | [`detect_anomalies()`](https://cedricbouffard.github.io/yieldcleanr/reference/detect_anomalies.md)         | `"position"`               |
| `apply_overlap_filter()`                                                                                                 | [`detect_anomalies()`](https://cedricbouffard.github.io/yieldcleanr/reference/detect_anomalies.md)         | `"overlap"`                |
| `apply_local_sd_filter()`                                                                                                | [`detect_anomalies()`](https://cedricbouffard.github.io/yieldcleanr/reference/detect_anomalies.md)         | `"local_sd"`               |
| [`calculate_auto_thresholds()`](https://cedricbouffard.github.io/yieldcleanr/reference/calculate_auto_thresholds.md)     | [`calculate_thresholds()`](https://cedricbouffard.github.io/yieldcleanr/reference/calculate_thresholds.md) | `"all"` ou `"yield"`, etc. |
| `apply_flow_delay()`                                                                                                     | [`optimize_delays()`](https://cedricbouffard.github.io/yieldcleanr/reference/optimize_delays.md)           | `"flow"`                   |
| [`apply_moisture_delay()`](https://cedricbouffard.github.io/yieldcleanr/reference/apply_moisture_delay.md)               | [`optimize_delays()`](https://cedricbouffard.github.io/yieldcleanr/reference/optimize_delays.md)           | `"moisture"`               |
| [`convert_flow_to_yield()`](https://cedricbouffard.github.io/yieldcleanr/reference/convert_flow_to_yield.md)             | [`convert_yield_units()`](https://cedricbouffard.github.io/yieldcleanr/reference/convert_yield_units.md)   | `"flow_lbs_s"` → `"kg_ha"` |
| [`anonymize_coordinates()`](https://cedricbouffard.github.io/yieldcleanr/reference/anonymize_coordinates.md)             | [`anonymize_data()`](https://cedricbouffard.github.io/yieldcleanr/reference/anonymize_data.md)             | `"coordinates"`            |
| [`remove_sensitive_attributes()`](https://cedricbouffard.github.io/yieldcleanr/reference/remove_sensitive_attributes.md) | [`anonymize_data()`](https://cedricbouffard.github.io/yieldcleanr/reference/anonymize_data.md)             | `"attributes"`             |
| [`anonymize_yield_data()`](https://cedricbouffard.github.io/yieldcleanr/reference/anonymize_yield_data.md)               | [`anonymize_data()`](https://cedricbouffard.github.io/yieldcleanr/reference/anonymize_data.md)             | `"full"`                   |
| `export_raster()`                                                                                                        | [`export_data()`](https://cedricbouffard.github.io/yieldcleanr/reference/export_data.md)                   | `"raster"`                 |
| `save_raster()`                                                                                                          | [`export_data()`](https://cedricbouffard.github.io/yieldcleanr/reference/export_data.md)                   | `"raster"`                 |

## Conclusion

Les méta-fonctions offrent une interface plus simple et plus cohérente
pour travailler avec yieldcleanr. Au lieu de retenir des dizaines de
fonctions, vous n’avez besoin que de **8 méta-fonctions** pour accomplir
la plupart des tâches de nettoyage de données de rendement.

Pour les cas d’utilisation avancés, les fonctions individuelles sont
toujours disponibles en interne, mais la nouvelle API recommandée
utilise les méta-fonctions présentées dans cette vignette.
