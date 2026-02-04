## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 10,
  fig.height = 6,
  warning = FALSE,
  message = FALSE
)

## ----setup--------------------------------------------------------------------
library(yieldcleanr)
library(dplyr)
library(ggplot2)
library(sf)
library(gridExtra)

## ----sample1-raw--------------------------------------------------------------
file_path <- system.file("extdata", "sample1.txt", package = "yieldcleanr")
data_raw_s1 <- read_yield_data(file_path)

cat("=== SAMPLE 1 - SOJA ===\n")
cat("Points bruts:", nrow(data_raw_s1), "\n")
cat("Flow (lbs/sec):\n")
cat("  Moyenne:", round(mean(data_raw_s1$Flow, na.rm = TRUE), 2), "\n")
cat("  Écart-type:", round(sd(data_raw_s1$Flow, na.rm = TRUE), 2), "\n")
cat("  Min-Max:", round(min(data_raw_s1$Flow, na.rm = TRUE), 2), "-", 
    round(max(data_raw_s1$Flow, na.rm = TRUE), 2), "\n")

## ----sample1-pipeline---------------------------------------------------------
# Étape 1: Conversion UTM (sans conversion en rendement)
data_s1 <- latlon_to_utm(data_raw_s1)

# Étape 2: Delay Adjustment (sur Flow, pas sur Yield)
delay_result_s1 <- optimize_delays(data_s1, type = "flow", delay_range = -25:25, n_iterations = 10, noise_level = 0.03)
cat("\n🔧 Delay Adjustment - Délai optimal:", delay_result_s1$delays$flow, "secondes\n")

if (!is.null(delay_result_s1$data)) {
  data_s1 <- delay_result_s1$data
}

# Étape 3: Conversion en rendement APRÈS delay adjustment
data_s1 <- convert_flow_to_yield(data_s1)

# Étape 4: Seuils automatiques
thresholds_s1 <- calculate_thresholds(data_s1)
cat("\n📈 Seuils calculés:\n")
cat("  Vitesse:", round(thresholds_s1$velocity$min_velocity, 2), "-", round(thresholds_s1$velocity$max_velocity, 2), "m/s\n")
cat("  Rendement:", round(thresholds_s1$yield$min_yield, 1), "-", round(thresholds_s1$yield$max_yield, 1), "kg/ha\n")

# Étape 5-8: Filtres successifs
cat("\n🔧 FILTRES APPLIQUÉS:\n")

# Filtre vitesse
n_before <- nrow(data_s1)
data_s1 <- filter_data(data_s1, type = "velocity", 
                       min_velocity = thresholds_s1$velocity$min_velocity, 
                       max_velocity = thresholds_s1$velocity$max_velocity)
removed_vel <- n_before - nrow(data_s1)
cat("  Vitesse:", removed_vel, "points retirés (", round(removed_vel/n_before*100, 1), "%)\n")

# Filtre humidité
n_before <- nrow(data_s1)
data_s1 <- filter_data(data_s1, type = "moisture", n_std = 3)
removed_moist <- n_before - nrow(data_s1)
cat("  Humidité:", removed_moist, "points retirés (", round(removed_moist/n_before*100, 1), "%)\n")

# Filtre chevauchement
n_before <- nrow(data_s1)
data_s1 <- detect_anomalies(data_s1, type = "overlap", cellsize = 0.3, overlap_threshold = 0.5)
removed_overlap <- n_before - nrow(data_s1)
cat("  Chevauchement:", removed_overlap, "points retirés (", round(removed_overlap/n_before*100, 1), "%)\n")

# Filtre écart-type local
n_before <- nrow(data_s1)
data_s1 <- detect_anomalies(data_s1, type = "local_sd", n_swaths = 5, lsd_limit = 3)
removed_lsd <- n_before - nrow(data_s1)
cat("  Écart-type local:", removed_lsd, "points retirés (", round(removed_lsd/n_before*100, 1), "%)\n")

# Résultat final
cat("\n📊 RÉSULTAT FINAL\n")
cat("Points nettoyés:", nrow(data_s1), "\n")
cat("Taux de rétention:", round(nrow(data_s1)/nrow(data_raw_s1)*100, 1), "%\n")
cat("Rendement moyen:", round(mean(data_s1$Yield_kg_ha, na.rm = TRUE), 1), "kg/ha\n")
cat("CV final:", round(sd(data_s1$Yield_kg_ha, na.rm = TRUE) / mean(data_s1$Yield_kg_ha, na.rm = TRUE) * 100, 1), "%\n")

