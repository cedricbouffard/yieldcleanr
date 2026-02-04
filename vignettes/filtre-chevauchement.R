## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 10,
  fig.height = 6,
  warning = FALSE,
  message = FALSE
)

## ----overlap-setup------------------------------------------------------------
library(yieldcleanr)
library(ggplot2)
library(dplyr)

# Charger les données
file_path <- system.file("extdata", "sample1.txt", package = "yieldcleanr")
data_raw <- read_yield_data(file_path)

# Préparation
data <- latlon_to_utm(data_raw) %>%
  convert_flow_to_yield() %>%
  filter_data(type = "velocity", min_velocity = 0.5, max_velocity = 10) %>%
  filter_data(type = "moisture", n_std = 3)

cat("=== Filtre de chevauchement ===\n")
cat("Points avant filtrage:", nrow(data), "\n")
cat("Nombre de passages uniques:", length(unique(data$Pass)), "\n")

## ----grid-params--------------------------------------------------------------
# Paramètres de la grille
cell_size <- 0.3  # 30 cm
overlap_threshold <- 0.5

cat("\n=== Paramètres de la grille ===\n")
cat("Taille des cellules:", cell_size, "m\n")
cat("Seuil de chevauchement:", overlap_threshold, "\n")
cat("Surface par cellule:", cell_size^2, "m²\n")

# Calculer les dimensions de la grille
x_range <- range(data$X, na.rm = TRUE)
y_range <- range(data$Y, na.rm = TRUE)

nx <- ceiling((x_range[2] - x_range[1]) / cell_size)
ny <- ceiling((y_range[2] - y_range[1]) / cell_size)

cat("\nDimensions de la grille:", nx, "x", ny, "=", nx * ny, "cellules\n")

## ----grid-viz, fig.width=12, fig.height=8-------------------------------------
# Créer une visualisation simplifiée de la grille
set.seed(42)

# Échantillon de points pour visualisation
sample_data <- data %>%
  sample_n(min(1000, nrow(data)))

# Créer la grille pour visualisation
x_seq <- seq(x_range[1], x_range[2], by = cell_size)
y_seq <- seq(y_range[1], y_range[2], by = cell_size)

# Graphique
p1 <- ggplot(sample_data, aes(x = X, y = Y)) +
  geom_point(aes(color = factor(Pass)), size = 1, alpha = 0.6) +
  geom_hline(yintercept = y_seq, color = "gray80", alpha = 0.3, size = 0.2) +
  geom_vline(xintercept = x_seq[seq(1, length(x_seq), by = 10)], 
             color = "gray80", alpha = 0.3, size = 0.2) +
  coord_equal() +
  labs(title = "Points de rendement superposés à la grille spatiale",
       subtitle = paste("Cellules de", cell_size, "m - Passages colorés différemment"),
       x = "X (m)", y = "Y (m)", color = "Passage") +
  theme_minimal() +
  theme(legend.position = "bottom")

p1

## ----overlap-application------------------------------------------------------
cat("\n=== Application du filtre de chevauchement ===\n")

# Appliquer le filtre
data_filtered <- detect_anomalies(data, 
                                  type = "overlap",
                                  cellsize = cell_size,
                                  overlap_threshold = overlap_threshold)

cat("Points après filtrage:", nrow(data_filtered), "\n")
cat("Points retirés:", nrow(data) - nrow(data_filtered), "\n")
cat("Taux de rétention:", round(nrow(data_filtered)/nrow(data)*100, 1), "%\n")

## ----overlap-zones, fig.width=14, fig.height=6--------------------------------
# Identifier les points éliminés
removed <- anti_join(data, data_filtered, by = c("X", "Y", "GPS_Time"))

cat("\n=== Analyse des zones de chevauchement ===\n")
cat("Points en zone de chevauchement:", nrow(removed), "\n")

if (nrow(removed) > 0) {
  # Distribution par passage
  removed_by_pass <- removed %>%
    group_by(Pass) %>%
    summarise(n = n()) %>%
    arrange(desc(n))
  
  cat("\nDistribution par passage (top 5):\n")
  print(head(removed_by_pass, 5))
  
  # Visualisation
  sf_removed <- sf::st_as_sf(removed, coords = c("Longitude", "Latitude"), crs = 4326)
  sf_all <- sf::st_as_sf(data, coords = c("Longitude", "Latitude"), crs = 4326)
  
  par(mfrow = c(1, 2))
  
  plot(sf_all["Yield_kg_ha"], main = "Tous les points", 
       pch = 19, cex = 0.3, breaks = "jenks", key.pos = NULL)
  
  plot(sf_removed["Yield_kg_ha"], main = "Points en chevauchement", 
       pch = 19, cex = 0.5, breaks = "jenks", key.pos = NULL)
}

## ----overlap-simulation, fig.width=14, fig.height=10--------------------------
# Créer une simulation de passages avec chevauchement
set.seed(42)

# Passage 1 : ligne droite
pass1 <- data.frame(
  x = seq(0, 100, by = 1),
  y = rep(10, 101),
  pass = 1,
  yield = rnorm(101, mean = 5000, sd = 200)
)

# Passage 2 : ligne droite parallèle
pass2 <- data.frame(
  x = seq(0, 100, by = 1),
  y = rep(20, 101),
  pass = 2,
  yield = rnorm(101, mean = 5200, sd = 200)
)

# Passage 3 : avec chevauchement sur la fin
pass3_x <- c(seq(0, 80, by = 1), seq(80, 100, by = 0.5))  # Densité doublée à la fin
pass3 <- data.frame(
  x = pass3_x,
  y = rep(30, length(pass3_x)),
  pass = 3,
  yield = rnorm(length(pass3_x), mean = 4800, sd = 200)
)

# Combiner
all_passes <- bind_rows(pass1, pass2, pass3)

# Visualisation des passages
p2 <- ggplot(all_passes, aes(x = x, y = y, color = factor(pass))) +
  geom_point(size = 2, alpha = 0.7) +
  geom_hline(yintercept = c(10, 20, 30), linetype = "dashed", alpha = 0.3) +
  annotate("rect", xmin = 80, xmax = 100, ymin = 28, ymax = 32,
           alpha = 0.2, fill = "red") +
  annotate("text", x = 90, y = 35, label = "Zone de\nchevauchement", 
           color = "red", size = 4) +
  labs(title = "Simulation de passages avec zone de chevauchement",
       subtitle = "Passage 3 a une densité doublée entre x=80 et x=100",
       x = "X (m)", y = "Y (m)", color = "Passage") +
  theme_minimal() +
  coord_equal()

p2

## ----overlap-ratio------------------------------------------------------------
# Discrétiser en cellules de 5m
cell_sim <- 5

all_passes$cell_x <- floor(all_passes$x / cell_sim)
all_passes$cell_y <- floor(all_passes$y / cell_sim)

# Compter les points par cellule
cell_counts <- all_passes %>%
  group_by(cell_x, cell_y) %>%
  summarise(
    n_points = n(),
    n_passes = n_distinct(pass),
    .groups = 'drop'
  )

cat("\n=== Comptage par cellule (simulation) ===\n")
cat("Cellules occupées:", nrow(cell_counts), "\n")
cat("Moyenne de points par cellule:", round(mean(cell_counts$n_points), 1), "\n")
cat("Max points dans une cellule:", max(cell_counts$n_points), "\n")

# Identifier les cellules en chevauchement
mean_points <- mean(cell_counts$n_points)
cell_counts$ratio <- cell_counts$n_points / mean_points
cell_counts$is_overlap <- cell_counts$ratio > 1.5

cat("\nCellules en chevauchement (ratio > 1.5):", 
    sum(cell_counts$is_overlap), "\n")
print(cell_counts %>% filter(is_overlap))

## ----params-table-overlap, echo=FALSE-----------------------------------------
params_df <- data.frame(
  Paramètre = c("type", "cellsize", "overlap_threshold", "max_pass"),
  Description = c(
    "Type de détection ('overlap')",
    "Taille des cellules (m)",
    "Seuil de ratio pour élimination",
    "Nombre max de passages avant élimination"
  ),
  Défaut = c("'overlap'", "0.3", "0.5", "50")
)

knitr::kable(params_df, caption = "Paramètres du filtre de chevauchement")

## ----overlap-stats------------------------------------------------------------
cat("\n=== Impact sur les statistiques ===\n")

# Avant filtrage
stats_before <- data.frame(
  metric = c("Nombre de points", "Rendement moyen", "Écart-type", "CV (%)"),
  value = c(
    nrow(data),
    mean(data$Yield_kg_ha, na.rm = TRUE),
    sd(data$Yield_kg_ha, na.rm = TRUE),
    sd(data$Yield_kg_ha, na.rm = TRUE) / mean(data$Yield_kg_ha, na.rm = TRUE) * 100
  )
)

# Après filtrage
stats_after <- data.frame(
  metric = c("Nombre de points", "Rendement moyen", "Écart-type", "CV (%)"),
  value = c(
    nrow(data_filtered),
    mean(data_filtered$Yield_kg_ha, na.rm = TRUE),
    sd(data_filtered$Yield_kg_ha, na.rm = TRUE),
    sd(data_filtered$Yield_kg_ha, na.rm = TRUE) / mean(data_filtered$Yield_kg_ha, na.rm = TRUE) * 100
  )
)

comparison <- data.frame(
  Métrique = stats_before$metric,
  Avant = round(stats_before$value, 1),
  Après = round(stats_after$value, 1),
  Variation = round((stats_after$value - stats_before$value) / stats_before$value * 100, 2)
)

print(comparison, row.names = FALSE)

