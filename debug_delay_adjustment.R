#!/usr/bin/env Rscript

# Debug Delay Adjustment

library(devtools)
load_all()
library(yieldcleanr)
library(dplyr)

cat("=== Debug Delay Adjustment ===\n")

file_path <- system.file("extdata", "sample2.txt", package = "yieldcleanr")
data_raw <- read_yield_data(file_path)
data_utm <- latlon_to_utm(data_raw)

cat("Donnees:", nrow(data_utm), "points\n")

# Preparer les donnees comme dans apply_delay_adjustment
data <- data_utm
max_points <- 10000

# Echantillonnage par grille
n_points <- nrow(data)
if (n_points > max_points) {
  data <- grid_sample_data(data, max_points = max_points, cellsize = 100)
  cat("Echantillonne:", nrow(data), "points sur", n_points, "\n")
}

# Tester differents delais manuellement
cat("\n=== Test des delais ===\n")
test_delays <- c(13, 14, 15, 16, 17)
results <- data.frame(delay = integer(), score = numeric())

for (delay in test_delays) {
  if (delay >= 0) {
    shifted_values <- lead(data$Flow, delay)
  } else {
    shifted_values <- lag(data$Flow, abs(delay))
  }
  
  valid <- !is.na(shifted_values) & !is.na(data$X) & !is.na(data$Y)
  cat("Delai", delay, ":", sum(valid), "points valides - ")
  
  if (sum(valid) > 50) {
    score <- calculate_moran_fast(
      data$X[valid],
      data$Y[valid],
      shifted_values[valid],
      bandwidth = 30
    )
    cat("Moran =", round(score, 4), "\n")
    results <- rbind(results, data.frame(delay = delay, score = score))
  } else {
    cat("Trop peu de points\n")
  }
}

cat("\n=== Resultats ===\n")
print(results %>% arrange(desc(score)))
