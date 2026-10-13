## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 10,
  fig.height = 6,
  warning = FALSE,
  message = FALSE
)

## ----moisture-setup-----------------------------------------------------------
library(yieldcleanr)
library(ggplot2)
library(dplyr)

# Charger les données
file_path <- system.file("extdata", "sample1.txt", package = "yieldcleanr")
data_raw <- read_yield_data(file_path)

# Réduire la taille pour accélérer le build (échantillon représentatif)
set.seed(42)
if (nrow(data_raw) > 3000) {
  data_raw <- data_raw[sample(nrow(data_raw), 3000), ]
}

# Préparation
data <- latlon_to_utm(data_raw) %>%
  convert_flow_to_yield()

cat("=== Filtre d'humidité ===\n")
cat("Points avant filtrage:", nrow(data), "\n")
cat("Humidité (%):\n")
cat("  Moyenne:", round(mean(data$Moisture, na.rm = TRUE), 1), "\n")
cat("  Min:", round(min(data$Moisture, na.rm = TRUE), 1), "\n")
cat("  Max:", round(max(data$Moisture, na.rm = TRUE), 1), "\n")
cat("  Écart-type:", round(sd(data$Moisture, na.rm = TRUE), 1), "\n")

## ----auto-thresholds-moisture-------------------------------------------------
# Calculer les seuils automatiques
n_std <- 3
mean_moisture <- mean(data$Moisture, na.rm = TRUE)
sd_moisture <- sd(data$Moisture, na.rm = TRUE)

min_moisture_auto <- mean_moisture - n_std * sd_moisture
max_moisture_auto <- mean_moisture + n_std * sd_moisture

cat("\n=== Seuils calculés automatiquement ===\n")
cat("Moyenne:", round(mean_moisture, 1), "%\n")
cat("Écart-type:", round(sd_moisture, 1), "%\n")
cat("Plage (moyenne ± 3 SD):\n")
cat("  Minimum:", round(min_moisture_auto, 1), "%\n")
cat("  Maximum:", round(max_moisture_auto, 1), "%\n")

## ----moisture-dist, fig.width=12, fig.height=6--------------------------------
# Distribution de l'humidité
moistures <- data$Moisture[is.finite(data$Moisture)]

p1 <- ggplot(data.frame(moisture = moistures), aes(x = moisture)) +
  geom_histogram(bins = 30, fill = "#3498db", alpha = 0.7, color = "white") +
  geom_vline(xintercept = min_moisture_auto, color = "#e74c3c", 
             linetype = "dashed", size = 1) +
  geom_vline(xintercept = max_moisture_auto, color = "#e74c3c", 
             linetype = "dashed", size = 1) +
  geom_vline(xintercept = mean_moisture, color = "#27ae60", 
             linetype = "solid", size = 1) +
  annotate("text", x = min_moisture_auto, y = Inf, 
           label = "Min", vjust = 2, color = "#e74c3c") +
  annotate("text", x = max_moisture_auto, y = Inf, 
           label = "Max", vjust = 2, color = "#e74c3c") +
  annotate("text", x = mean_moisture, y = Inf, 
           label = "Moyenne", vjust = 2, color = "#27ae60") +
  labs(title = "Distribution de l'humidité",
       subtitle = "Seuils automatiques (moyenne ± 3 SD)",
       x = "Humidité (%)",
       y = "Fréquence") +
  theme_minimal()

p1

## ----auto-filter-moisture-----------------------------------------------------
cat("\n=== Application du filtre automatique ===\n")

# Appliquer le filtre
data_filtered <- filter_data(data, type = "moisture", n_std = 3)

cat("Points après filtrage:", nrow(data_filtered), "\n")
cat("Points retirés:", nrow(data) - nrow(data_filtered), "\n")
cat("Taux de rétention:", round(nrow(data_filtered)/nrow(data)*100, 1), "%\n")

## ----manual-filter-moisture---------------------------------------------------
cat("\n=== Filtrage avec seuils manuels ===\n")

# Seuils pour soja
min_manual <- 8
max_manual <- 20

cat("Seuils manuels (soja):", min_manual, "-", max_manual, "%\n")

data_manual <- filter_data(data, 
                           type = "moisture",
                           min_moisture = min_manual,
                           max_moisture = max_manual)

cat("Points après filtrage manuel:", nrow(data_manual), "\n")
cat("Points retirés:", nrow(data) - nrow(data_manual), "\n")

