## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 10,
  fig.height = 6,
  warning = FALSE,
  message = FALSE
)

## ----velocity-calc------------------------------------------------------------
library(yieldcleanr)
library(ggplot2)
library(dplyr)

# Charger les données
file_path <- system.file("extdata", "sample1.txt", package = "yieldcleanr")
data_raw <- read_yield_data(file_path)

# Conversion UTM
data_utm <- latlon_to_utm(data_raw)

# Calcul manuel de la vitesse pour illustration
data_calc <- data_utm %>%
  mutate(
    dx = X - lag(X),
    dy = Y - lag(Y),
    distance = sqrt(dx^2 + dy^2),
    velocity_manual = distance / Interval
  )

cat("=== Calcul de la vitesse ===\n")
cat("Formule: v = √((x₂-x₁)² + (y₂-y₁)²) / Δt\n\n")
cat("Exemple sur les 5 premiers points:\n")
head(data_calc %>% select(X, Y, Interval, distance, velocity_manual), 5)

## ----thresholds-calc----------------------------------------------------------
# Calculer les seuils automatiques
thresholds <- calculate_thresholds(data_utm)

cat("=== Seuils de vitesse calculés ===\n")
cat("Quantile 2%:", round(quantile(data_calc$velocity_manual, 0.02, na.rm = TRUE), 2), "m/s\n")
cat("Quantile 98%:", round(quantile(data_calc$velocity_manual, 0.98, na.rm = TRUE), 2), "m/s\n")
cat("\nSeuil minimum:", round(thresholds$velocity$min_velocity, 2), "m/s\n")
cat("Seuil maximum:", round(thresholds$velocity$max_velocity, 2), "m/s\n")

## ----velocity-dist, fig.width=12, fig.height=6--------------------------------
# Distribution des vitesses
velocities <- data_calc$velocity_manual[!is.na(data_calc$velocity_manual)]

# Créer le graphique
df_vel <- data.frame(velocity = velocities)

p1 <- ggplot(df_vel, aes(x = velocity)) +
  geom_histogram(bins = 50, fill = "#3498db", alpha = 0.7, color = "white") +
  geom_vline(xintercept = thresholds$velocity$min_velocity, color = "#e74c3c", 
             linetype = "dashed", size = 1) +
  geom_vline(xintercept = thresholds$velocity$max_velocity, color = "#e74c3c", 
             linetype = "dashed", size = 1) +
  annotate("text", x = thresholds$velocity$min_velocity, y = Inf, 
           label = "Min", vjust = 2, color = "#e74c3c") +
  annotate("text", x = thresholds$velocity$max_velocity, y = Inf, 
           label = "Max", vjust = 2, color = "#e74c3c") +
  labs(title = "Distribution des vitesses",
       subtitle = "Seuils de filtrage indiqués en rouge",
       x = "Vitesse (m/s)",
       y = "Fréquence") +
  theme_minimal() +
  xlim(0, quantile(velocities, 0.99))

p1

## ----filter-application-------------------------------------------------------
cat("=== Application du filtre de vitesse ===\n")

# Avant filtrage
n_before <- nrow(data_utm)
cat("Points avant filtrage:", n_before, "\n")
cat("Vitesse moyenne:", round(mean(data_calc$velocity_manual, na.rm = TRUE), 2), "m/s\n")
cat("Vitesse min-max:", round(min(data_calc$velocity_manual, na.rm = TRUE), 2), "-",
    round(max(data_calc$velocity_manual, na.rm = TRUE), 2), "m/s\n\n")

# Appliquer le filtre
data_filtered <- filter_data(data_utm, 
                             type = "velocity",
                             min_velocity = thresholds$velocity$min_velocity,
                             max_velocity = thresholds$velocity$max_velocity)

# Après filtrage
n_after <- nrow(data_filtered)
cat("Points après filtrage:", n_after, "\n")
cat("Points retirés:", n_before - n_after, "(", 
    round((n_before - n_after)/n_before*100, 1), "%)\n")

## ----velocity-viz, fig.width=14, fig.height=6---------------------------------
# Calculer les vitesses pour visualisation
data_viz_before <- data_utm %>%
  mutate(velocity = sqrt((X - lag(X))^2 + (Y - lag(Y))^2) / Interval)

data_viz_after <- data_filtered %>%
  mutate(velocity = sqrt((X - lag(X))^2 + (Y - lag(Y))^2) / Interval)

# Créer les objets sf
sf_before <- sf::st_as_sf(data_viz_before, coords = c("Longitude", "Latitude"), crs = 4326)
sf_after <- sf::st_as_sf(data_viz_after, coords = c("Longitude", "Latitude"), crs = 4326)

# Graphique comparatif
par(mfrow = c(1, 2))

plot(sf_before["velocity"], main = "AVANT - Toutes les vitesses", 
     pch = 19, cex = 0.3, breaks = "jenks", key.pos = NULL)
plot(sf_after["velocity"], main = "APRÈS - Vitesses filtrées", 
     pch = 19, cex = 0.3, breaks = "jenks", key.pos = NULL)

## ----low-velocity, fig.width=10, fig.height=5---------------------------------
# Identifier les points à vitesse nulle ou très faible
low_vel <- data_calc %>% 
  filter(velocity_manual < 0.5 | is.na(velocity_manual))

cat("Points avec vitesse < 0.5 m/s:", nrow(low_vel), "\n")
cat("Causes possibles:\n")
cat("  - Arrêts de la moissonneuse\n")
cat("  - Manœuvres de demi-tour\n")
cat("  - Problèmes de transmission GPS\n")

# Visualiser
if (nrow(low_vel) > 0) {
  ggplot(low_vel, aes(x = X, y = Y)) +
    geom_point(color = "#e74c3c", alpha = 0.5, size = 1) +
    labs(title = "Points à vitesse anormalement faible",
         subtitle = "Ces points seront éliminés par le filtre",
         x = "X (m)", y = "Y (m)") +
    theme_minimal()
}

## ----high-velocity, fig.width=10, fig.height=5--------------------------------
# Identifier les points à vitesse excessive
high_vel <- data_calc %>% 
  filter(velocity_manual > 10)

cat("\nPoints avec vitesse > 10 m/s:", nrow(high_vel), "\n")
cat("Causes possibles:\n")
cat("  - Sauts GPS (perte de signal)\n")
cat("  - Points mal synchronisés\n")
cat("  - Erreurs de positionnement\n")

# Visualiser
if (nrow(high_vel) > 0) {
  ggplot(high_vel, aes(x = X, y = Y)) +
    geom_point(color = "#e74c3c", alpha = 0.5, size = 1) +
    labs(title = "Points à vitesse anormalement élevée",
         subtitle = "Ces points seront éliminés par le filtre",
         x = "X (m)", y = "Y (m)") +
    theme_minimal()
}

## ----params-table, echo=FALSE-------------------------------------------------
params_df <- data.frame(
  Paramètre = c("type", "min_value", "max_value", "vllim", "vulim", 
                "vscale", "minv_abs"),
  Description = c(
    "Type de filtre ('velocity')",
    "Vitesse minimale (m/s)",
    "Vitesse maximale (m/s)",
    "Quantile bas pour calcul auto",
    "Quantile haut pour calcul auto",
    "Facteur d'échelle haut",
    "Minimum absolu (m/s)"
  ),
  Défaut = c("'velocity'", "auto", "auto", "0.02", "0.98", "1.5", "0.5")
)

knitr::kable(params_df, caption = "Paramètres du filtre de vitesse")

## ----case1, fig.width=10, fig.height=5----------------------------------------
# Simuler un arrêt
set.seed(123)
time <- 1:100
x <- c(seq(0, 40, length.out = 40), 
       rep(40, 20),  # Arrêt
       seq(40, 80, length.out = 40))
y <- seq(0, 100, length.out = 100)
velocity <- c(rep(2, 39), rep(0, 21), rep(2, 40))

df_case1 <- data.frame(time = time, x = x, y = y, velocity = velocity)

p_case1 <- ggplot(df_case1, aes(x = time)) +
  geom_line(aes(y = velocity), color = "#2c3e50", size = 1) +
  geom_hline(yintercept = 0.5, color = "#e74c3c", linetype = "dashed") +
  annotate("rect", xmin = 40, xmax = 60, ymin = -0.2, ymax = 2.2,
           alpha = 0.2, fill = "#e74c3c") +
  annotate("text", x = 50, y = 1.5, label = "Arrêt\nvidange", color = "#e74c3c") +
  labs(title = "Cas 1: Arrêt pour vidange",
       subtitle = "Vitesse = 0 pendant 20 secondes - sera éliminé",
       x = "Temps (s)", y = "Vitesse (m/s)") +
  theme_minimal()

p_case1

## ----case2, fig.width=10, fig.height=5----------------------------------------
# Simuler un saut GPS
time <- 1:100
x <- c(seq(0, 50, length.out = 50), 
       100,  # Saut
       seq(102, 150, length.out = 49))
y <- seq(0, 100, length.out = 100)
velocity <- c(rep(2, 50), 50, rep(2, 49))  # Pic de vitesse

df_case2 <- data.frame(time = time, x = x, y = y, velocity = velocity)

p_case2 <- ggplot(df_case2, aes(x = time)) +
  geom_line(aes(y = velocity), color = "#2c3e50", size = 1) +
  geom_hline(yintercept = 5, color = "#e74c3c", linetype = "dashed") +
  annotate("rect", xmin = 50, xmax = 51, ymin = 0, ymax = 50,
           alpha = 0.2, fill = "#e74c3c") +
  annotate("text", x = 55, y = 40, label = "Saut\nGPS", color = "#e74c3c") +
  labs(title = "Cas 2: Saut GPS",
       subtitle = "Vitesse instantanée de 50 m/s - sera éliminé",
       x = "Temps (s)", y = "Vitesse (m/s)") +
  theme_minimal()

p_case2

