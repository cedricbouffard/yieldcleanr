#!/usr/bin/env Rscript

# Test du nouveau Delay Adjustment rapide sur sample2
# Devrait trouver un delai de 15 secondes

library(yieldcleanr)
library(dplyr)

# Charger les donnees
file_path <- system.file("extdata", "sample2.txt", package = "yieldcleanr")
data_raw <- read_yield_data(file_path)

cat("=== Test Delay Adjustment Rapide sur sample2 ===\n")
cat("Donnees brutes:", nrow(data_raw), "points\n")

# Convertir en UTM
data_utm <- latlon_to_utm(data_raw)

cat("Apres conversion UTM:", nrow(data_utm), "points\n")
cat("Range X:", round(min(data_utm$X, na.rm = TRUE), 1), "a", round(max(data_utm$X, na.rm = TRUE), 1), "\n")
cat("Range Y:", round(min(data_utm$Y, na.rm = TRUE), 1), "a", round(max(data_utm$Y, na.rm = TRUE), 1), "\n")

# Tester le Delay Adjustment
cat("\n=== Execution Delay Adjustment ===\n")
start_time <- Sys.time()

result <- apply_delay_adjustment(
  data = data_utm,
  delay_range = -25:25,
  coarse_step = 2,
  value_col = "Flow",
  max_points = 10000,
  bandwidth = 30
)

end_time <- Sys.time()
execution_time <- as.numeric(end_time - start_time, units = "secs")

cat("\n=== Resultats ===\n")
cat("Delai optimal trouve:", result$optimal_delay, "secondes\n")
cat("Temps d'execution:", round(execution_time, 2), "secondes\n")
cat("Score Moran:", round(result$score_values$score[which.max(result$score_values$score)], 4), "\n")
cat("Stabilite:", round(result$stability, 4), "\n")

if (result$optimal_delay == 15) {
  cat("\n✓ SUCCES: Le delai de 15 secondes a ete trouve!\n")
} else {
  cat("\n✗ ATTENTION: Delai attendu = 15s, Delai trouve =", result$optimal_delay, "s\n")
  cat("\nScores par delai:\n")
  print(result$score_values %>% arrange(desc(score)) %>% head(10))
}

# Afficher tous les scores pour debug
cat("\n=== Tous les scores ===\n")
print(result$score_values %>% arrange(delay))
