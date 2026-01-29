# sample2.txt - Soybean Data (File 2)

## Introduction

This file contains soybean yield data (file 2) collected by a combine
harvester.

## Loading Data

``` r
library(yieldcleanr)
library(ggplot2)
library(sf)
```

    ## Linking to GEOS 3.12.1, GDAL 3.8.4, PROJ 9.4.0; sf_use_s2() is TRUE

## Raw Data

``` r
file_path <- system.file("extdata", "sample2.txt", package = "yieldcleanr")
data_raw <- read_yield_data(file_path)
```

    ## Distance detectee en pouces (moyenne: 76.1 ) - conversion en metres

``` r
cat("Raw data:\n")
```

    ## Raw data:

``` r
cat("  Rows:", nrow(data_raw), "\n")
```

    ##   Rows: 36869

``` r
cat("  Mean flow:", round(mean(data_raw$Flow, na.rm = TRUE), 2), "lbs/s\n")
```

    ##   Mean flow: 6.55 lbs/s

``` r
cat("  Grain type:", unique(data_raw$GrainType), "\n")
```

    ##   Grain type: Corn

## Raw Data Map

``` r
data_raw_sf <- sf::st_as_sf(data_raw, coords = c("Longitude", "Latitude"), crs = 4326)

ggplot() +
  geom_sf(data = data_raw_sf, aes(color = Flow), size = 0.5, alpha = 0.7) +
  scale_color_viridis_c(name = "Flow (lbs/s)") +
  theme_minimal() +
  labs(title = "Raw Data - sample2.txt", subtitle = "Flow before cleaning")
```

![](sample2_en_files/figure-html/raw-map-1.png)

## AYCE Cleaning

``` r
cleaned <- clean_yield(
  file_path = file_path,
  metrique = TRUE,
  polygon = TRUE
)
```

    ## ================================================
    ##    Yield Data Cleaning Pipeline               
    ##    Output: Metric (kg/ha)                      
    ##    Geometry: Polygons                          
    ## ================================================
    ## 
    ## Etape 1 : chargement des donnees...
    ## Distance detectee en pouces (moyenne: 76.1 ) - conversion en metres
    ##   - 36869 raw observations loaded
    ## Etape 2 : conversion en coordonnees UTM...
    ## Zone UTM detectee: 15
    ## Etape 3 : PCDI - optimisation du delai de flux...
    ## === PCDI: Phase Correlation Delay Identification ( Flow ) ===
    ## Delai optimal ( Flow ): 8 secondes
    ## RSC a l'optimal : 0.5489
    ## Stabilite (CV) : 4e-04
    ##   Delai optimal flux: 8 secondes
    ## Etape 3b : PCDI - optimisation du delai d'humidite...
    ## === PCDI: Phase Correlation Delay Identification ( Moisture ) ===
    ## Delai optimal ( Moisture ): -10 secondes
    ## RSC a l'optimal : 0.6664
    ## Stabilite (CV) : 2e-04
    ##   Delai optimal humidite: -10 secondes
    ## Etape 3c : calcul du rendement initial pour les seuils...
    ## GrainType non reconnu, utilisation 56 lbs/boisseau (mais par defaut)
    ## Yield calcule: 3320.3 bu/acre (lbs/bu = 56 )
    ## Etape 4 : calcul des seuils automatiques...
    ## === Automatic Threshold Calculation (AYCE) ===
    ## Yield: Yield_buacre MIN = 0 MAX = 9257.35
    ## Velocity: MIN = 0.86 MAX = 2.86
    ## Position: X[ 573012 - 573744 ]
    ## Position: Y[ 4342581 - 4343148 ]
    ## Etape 5 : filtre header...
    ##   Rows: 29476
    ## Etape 6 : filtre GPS...
    ##   Rows: 29476
    ## Etape 7 : calcul de la vitesse...
    ## Etape 8 : filtre vitesse...
    ##   Rows: 29015
    ## Etape 9 : correction du delai de flux ( 8 s)...
    ## Flow delay correction: -8 seconds, 8 points elimines (valeurs NA)
    ##   Rows: 29007
    ## Etape 9a : correction du delai d'humidite ( -10 s)...
    ## Moisture delay correction: 10 seconds, 10 points elimines (valeurs NA)
    ##   Rows: 28997
    ## Etape 9b : calcul du rendement apres delai...
    ## GrainType non reconnu, utilisation 56 lbs/boisseau (mais par defaut)
    ## Yield calcule: 3345.4 bu/acre (lbs/bu = 56 )
    ## Etape 9c : recalcul des seuils apres delai...
    ## === Automatic Threshold Calculation (AYCE) ===
    ## Yield: Yield_buacre MIN = 0 MAX = 8539.28
    ## Velocity: MIN = 1.49 MAX = 2.52
    ## Position: X[ 573029 - 573736 ]
    ## Position: Y[ 4342582 - 4343147 ]
    ## Etape 9d : validation de Pass via analyse de direction...
    ##   Pass column has 192 unique values
    ##   Pass column appears reasonable, using as-is
    ## Etape 9e : suppression des points de bordure lies au delai...
    ##    1503 boundary points removed ( beginning , delay: 8 s = 8 points)
    ## Etape 10 : suppression des rendements nuls...
    ##   Rows: 27494
    ## Etape 11 : filtre plage de rendement...
    ## Yield range filter: 1 points elimines (rendement hors plage: 0 - 8539.3 )
    ##   Rows: 27493
    ## Etape 12 : filtre humidite (auto-detection)...
    ## Moisture auto-range: 16.1 - 23.4 (mean +/- 3 SD = 19.7 +/- 1.2 )
    ## Moisture range filter: 122 points elimines (humidite hors plage: 16.1 - 23.4 )
    ##   Rows: 27371
    ## Etape 13 : filtre de chevauchement bitmap...
    ## === Bitmap Overlap Filter ===
    ## Donnees etendues - utilisation du bitmap sparse
    ## Overlap ratio: min 0 max 1
    ## Overlap filter: 41 points elimines (0.1%)
    ##   Rows: 27330
    ## Etape 14 : filtre ecart-type localise...
    ## === Localized SD Filter ===
    ## Local SD filter: 159 points elimines (0.6%)
    ##   Rows: 27171
    ## Etape 15 : validation et controle qualite...
    ## === AYCE Validation & Quality Control ===
    ## Retention rate: 73.7 %
    ## Raw CV: 55.5 %
    ## Clean CV: 37.9 %
    ##   Retention rate: 73.7 %
    ## Etape 16 : formatage de la sortie...
    ## Etape 16b : calcul du cap...
    ## Etape 16b.1 : lissage du cap par segments...
    ## Etape 16c : creation de l'objet SF polygones...
    ## Creation d'un objet SF avec polygones...
    ## Creation des geometries des polygones...
    ## Objet SF cree : 27171 polygones
    ## 
    ## ================================================
    ## Termine : 27171 observations nettoyees
    ## Rendement moyen : 222301 kg/ha
    ## ================================================

## Cleaned Data Map

``` r
ggplot() +
  geom_sf(data = cleaned, aes(color = Yield), size = 0.5, alpha = 0.7) +
  scale_color_viridis_c(name = "Yield (kg/ha)") +
  theme_minimal() +
  labs(title = "Cleaned Data - sample2.txt", subtitle = "Yield after AYCE")
```

![](sample2_en_files/figure-html/cleaned-map-1.png)

## Statistics

``` r
cat("Raw data:\n")
```

    ## Raw data:

``` r
cat("  Rows:", nrow(data_raw), "\n")
```

    ##   Rows: 36869

``` r
cat("\nCleaned data:\n")
```

    ## 
    ## Cleaned data:

``` r
cat("  Rows:", nrow(cleaned), "\n")
```

    ##   Rows: 27171

``` r
cat("  Mean yield:", round(mean(cleaned$Yield, na.rm = TRUE), 1), "kg/ha\n")
```

    ##   Mean yield: 222301.4 kg/ha

``` r
cat("  Retention rate:", round(nrow(cleaned) / nrow(data_raw) * 100, 1), "%\n")
```

    ##   Retention rate: 73.7 %
