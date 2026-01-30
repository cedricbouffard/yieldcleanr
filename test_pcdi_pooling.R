#!/usr/bin/env Rscript

# Test du PCDI avec pooling sur 5 iterations (version legacy)

library(devtools)
load_all()
library(yieldcleanr)
library(dplyr)

cat("=== Test PCDI avec pooling (5 iterations, legacy method) ===\n\n")

file_path <- system.file("extdata", "sample2.txt", package = "yieldcleanr")
data_raw <- read_yield_data(file_path)
data_utm <- latlon_to_utm(data_raw)

cat("Donnees:", nrow(data_utm), "points\n\n")

start_time <- Sys.time()

result <- apply_pcdi(
  data = data_utm,
  delay_range = -25:25,
  n_iterations = 5,
  value_col = "Flow",
  max_points = 10000,
  bandwidth = 30
)

end_time <- Sys.time()
execution_time <- as.numeric(end_time - start_time, units = "secs")

cat("\n=== Resultats ===\n")
cat("Delai optimal:", result$optimal_delay, "secondes\n")
cat("Temps execution:", round(execution_time, 2), "secondes\n")
cat("Stabilite (CV):", round(result$stability, 4), "\n")

if (result$optimal_delay == 15) {
  cat("\n✓ SUCCES: Delai de 15 secondes trouve!\n")
} else {
  cat("\n Delai trouve:", result$optimal_delay, "s (attendu: 15s)\n")
  cat("\nTop 5 delais:\n")
  top5 <- result$score_values %>% 
    arrange(desc(mean_score)) %>% 
    head(5)
  print(top5)
}
