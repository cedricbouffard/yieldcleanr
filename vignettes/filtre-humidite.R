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

## ----removed-moisture, fig.width=14, fig.height=6-----------------------------
# Identifier les points éliminés
removed <- data %>%
  filter(Moisture < min_moisture_auto | 
         Moisture > max_moisture_auto |
         !is.finite(Moisture))

cat("\nPoints éliminés par humidité:", nrow(removed), "\n")

if (nrow(removed) > 0) {
  sf_removed <- sf::st_as_sf(removed, coords = c("Longitude", "Latitude"), crs = 4326)
  sf_all <- sf::st_as_sf(data, coords = c("Longitude", "Latitude"), crs = 4326)
  
  par(mfrow = c(1, 2))
  
  plot(sf_all["Moisture"], main = "Tous les points (humidité)", 
       pch = 19, cex = 0.3, breaks = "jenks", key.pos = NULL)
  
  plot(sf_removed["Moisture"], main = "Points éliminés (humidité)", 
       pch = 19, cex = 0.5, breaks = "jenks", key.pos = NULL)
}

## ----moisture-temporal, fig.width=12, fig.height=6----------------------------
# Trier par temps et échantillonner pour la ligne de tendance (LOESS est lent avec beaucoup de points)
data_time <- data %>%
  arrange(GPS_Time) %>%
  mutate(time_normalized = (GPS_Time - min(GPS_Time)) / 3600)  # En heures

# Échantillon pour la ligne de tendance (max 5000 points)
n_points <- nrow(data_time)
if (n_points > 5000) {
  set.seed(42)
  data_sample <- data_time[sample(n_points, 5000), ]
} else {
  data_sample <- data_time
}

# Graphique temporel
p2 <- ggplot(data_time, aes(x = time_normalized, y = Moisture)) +
  geom_point(alpha = 0.3, size = 0.5, color = "#3498db") +
  geom_smooth(data = data_sample, method = "loess", color = "#e74c3c", se = TRUE, span = 0.5) +
  geom_hline(yintercept = min_moisture_auto, color = "#27ae60", 
             linetype = "dashed", alpha = 0.7) +
  geom_hline(yintercept = max_moisture_auto, color = "#27ae60", 
             linetype = "dashed", alpha = 0.7) +
  labs(title = "Évolution de l'humidité au cours du temps",
       subtitle = "Points hors plage seront éliminés",
       x = "Temps (heures depuis le début)",
       y = "Humidité (%)") +
  theme_minimal()

p2

## ----yield-conversion---------------------------------------------------------
cat("\n=== Impact de l'humidité sur le rendement ===\n")

# Calculer le rendement sec
humidite_standard <- 13  # Pour soja

# Avant filtrage
rendement_humide <- mean(data$Yield_kg_ha, na.rm = TRUE)
humidite_moyenne <- mean(data$Moisture, na.rm = TRUE)
rendement_sec_brut <- rendement_humide * (100 - humidite_moyenne) / (100 - humidite_standard)

cat("Avant filtrage:\n")
cat("  Rendement humide moyen:", round(rendement_humide, 1), "kg/ha\n")
cat("  Humidité moyenne:", round(humidite_moyenne, 1), "%\n")
cat("  Rendement sec équivalent:", round(rendement_sec_brut, 1), "kg/ha\n")

# Après filtrage
rendement_humide_filt <- mean(data_filtered$Yield_kg_ha, na.rm = TRUE)
humidite_moyenne_filt <- mean(data_filtered$Moisture, na.rm = TRUE)
rendement_sec_filt <- rendement_humide_filt * (100 - humidite_moyenne_filt) / (100 - humidite_standard)

cat("\nAprès filtrage:\n")
cat("  Rendement humide moyen:", round(rendement_humide_filt, 1), "kg/ha\n")
cat("  Humidité moyenne:", round(humidite_moyenne_filt, 1), "%\n")
cat("  Rendement sec équivalent:", round(rendement_sec_filt, 1), "kg/ha\n")

cat("\nDifférence:", round(rendement_sec_filt - rendement_sec_brut, 1), "kg/ha\n")

## ----params-table-moisture, echo=FALSE----------------------------------------
params_df <- data.frame(
  Paramètre = c("type", "min_value", "max_value", "n_std", "moisture_column"),
  Description = c(
    "Type de filtre ('moisture')",
    "Humidité minimum (%)",
    "Humidité maximum (%)",
    "Nombre d'écarts-types pour calcul auto",
    "Nom de la colonne d'humidité"
  ),
  Défaut = c("'moisture'", "auto", "auto", "3", "'Moisture'")
)

knitr::kable(params_df, caption = "Paramètres du filtre d'humidité")

## ----zero-moisture------------------------------------------------------------
# Points avec humidité nulle ou manquante
zero_moist <- data %>%
  filter(Moisture <= 0 | is.na(Moisture))

cat("Points avec humidité ≤ 0 ou manquante:", nrow(zero_moist), "\n")
cat("Ces points sont généralement éliminés car l'humidité est requise\n")
cat("pour la conversion rendement humide → sec.\n")

## ----moisture-drift, fig.width=10, fig.height=5-------------------------------
# Simuler une dérive d'humidité
set.seed(42)
temps <- 1:100
humidite_normale <- rnorm(100, mean = 12, sd = 1)
humidite_derfive <- c(humidite_normale[1:80], rnorm(20, mean = 18, sd = 2))

df_drift <- data.frame(
  temps = temps,
  humidite = humidite_derfive,
  type = c(rep("Normal", 80), rep("Dérive", 20))
)

p3 <- ggplot(df_drift, aes(x = temps, y = humidite, color = type)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_hline(yintercept = 15, color = "#e74c3c", linetype = "dashed") +
  annotate("text", x = 90, y = 16, label = "Seuil max", color = "#e74c3c") +
  labs(title = "Dérive d'humidité en fin de journée",
       subtitle = "Augmentation due à la rosée ou changement de conditions",
       x = "Temps", y = "Humidité (%)", color = "Condition") +
  theme_minimal()

p3

