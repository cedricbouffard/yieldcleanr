## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 10,
  fig.height = 6,
  warning = FALSE,
  message = FALSE
)

## ----delay_adjustment-demo----------------------------------------------------
library(yieldcleanr)
library(ggplot2)
library(dplyr)

# Charger les données
file_path <- system.file("extdata", "sample2.txt", package = "yieldcleanr")
data_raw <- read_yield_data(file_path)

# Préparation des données (UTM seulement, pas de conversion en rendement)
data <- latlon_to_utm(data_raw)

# Appliquer Delay Adjustment avec plage étendue sur Flow (pas sur Yield)
cat("=== Application du Delay Adjustment ===\n")
delay_result <- optimize_delays(data, type = "flow", delay_range = -25:25, n_iterations = 10, noise_level = 0.03)

cat("Délai optimal détecté:", delay_result$delays$flow, "secondes\n")
optimal_rsc <- delay_result$delay_adjustment_results$flow$rsc_values$mean_score[delay_result$delay_adjustment_results$flow$rsc_values$delay == delay_result$delays$flow]
if (length(optimal_rsc) > 0 && !is.na(optimal_rsc[1])) {
  cat("Score Moran moyen au délai optimal:", round(optimal_rsc[1], 4), "\n")
} else {
  cat("Score Moran moyen au délai optimal: NA\n")
}

## ----delay_adjustment-comparison, fig.width=14, fig.height=6------------------
# Données avant correction (converties en rendement pour visualisation)
data_before <- convert_flow_to_yield(data)

# Données après correction (avec conversion en rendement)
data_after <- convert_flow_to_yield(delay_result$data)

# Créer les objets sf
sf_before <- sf::st_as_sf(data_before, coords = c("Longitude", "Latitude"), crs = 4326)
sf_after <- sf::st_as_sf(data_after, coords = c("Longitude", "Latitude"), crs = 4326)

# Graphique comparatif
par(mfrow = c(1, 2))

plot(sf_before["Yield_kg_ha"], main = "AVANT correction Delay Adjustment", 
     pch = 19, cex = 0.3, breaks = "jenks", key.pos = NULL)
plot(sf_after["Yield_kg_ha"], main = "APRÈS correction Delay Adjustment", 
     pch = 19, cex = 0.3, breaks = "jenks", key.pos = NULL)

## ----delay_adjustment-simulation, fig.width=12, fig.height=8------------------
# Créer des données simulées avec un délai connu
set.seed(42)
n_points <- 200

# Trajectoire de la moissonneuse
x <- seq(0, 100, length.out = n_points)
y <- sin(x/10) * 10 + seq(0, 50, length.out = n_points)

# Rendement "vrai" avec pattern spatial
yield_true <- 5000 + 1000 * sin(x/5) + rnorm(n_points, 0, 200)

# Flux mesuré avec délai de 5 secondes
delay <- 5
yield_measured <- c(rep(NA, delay), yield_true[1:(n_points-delay)])

# Créer dataframe
df <- data.frame(
  x = x,
  y = y,
  yield_true = yield_true,
  yield_measured = yield_measured,
  time = 1:n_points
)

# Visualisation
p2 <- ggplot(df, aes(x = x, y = y)) +
  geom_point(aes(color = yield_true), size = 2, alpha = 0.7) +
  scale_color_viridis_c(name = "Rendement\nvrai (kg/ha)") +
  labs(title = "Rendement spatial 'vrai'",
       subtitle = "Pattern régulier sans délai") +
  theme_minimal()

p3 <- ggplot(df, aes(x = x, y = y)) +
  geom_point(aes(color = yield_measured), size = 2, alpha = 0.7) +
  scale_color_viridis_c(name = "Rendement\nmesuré (kg/ha)") +
  labs(title = "Rendement mesuré avec délai de 5 secondes",
       subtitle = "Pattern décalé - bandes obliques artificielles") +
  theme_minimal()

gridExtra::grid.arrange(p2, p3, ncol = 2)

## ----delay_adjustment-moran-effect, fig.width=10, fig.height=6----------------
# Calculer Moran I pour différents délais simulés
calculate_moran_simple <- function(yield, x, y, threshold = 10) {
  n <- length(yield)
  y_bar <- mean(yield, na.rm = TRUE)
  
  numerator <- 0
  denominator <- sum((yield - y_bar)^2, na.rm = TRUE)
  W <- 0
  
  for (i in 1:n) {
    for (j in 1:n) {
      if (i != j) {
        dist <- sqrt((x[i] - x[j])^2 + (y[i] - y[j])^2)
        if (dist <= threshold) {
          w <- 1
          numerator <- numerator + w * (yield[i] - y_bar) * (yield[j] - y_bar)
          W <- W + w
        }
      }
    }
  }
  
  I <- (n / W) * (numerator / denominator)
  return(I)
}

# Tester différents délais
delays_test <- -10:10
moran_scores <- sapply(delays_test, function(d) {
  if (d >= 0) {
    y_shifted <- c(rep(NA, d), yield_true[1:(n_points-d)])
  } else {
    y_shifted <- c(yield_true[(-d+1):n_points], rep(NA, -d))
  }
  calculate_moran_simple(y_shifted, x, y)
})

# Graphique
df_moran <- data.frame(delay = delays_test, moran = moran_scores)

ggplot(df_moran, aes(x = delay, y = moran)) +
  geom_line(size = 1, color = "#2c3e50") +
  geom_point(size = 3, color = "#3498db") +
  geom_vline(xintercept = 5, linetype = "dashed", color = "#e74c3c") +
  annotate("text", x = 5.5, y = max(moran_scores) - 0.05,
           label = "Délai réel: 5s", color = "#e74c3c", hjust = 0) +
  labs(title = "Détection du délai par maximisation de Moran I",
       subtitle = "Le pic correspond au délai réel de 5 secondes",
       x = "Délai testé (secondes)",
       y = "Indice de Moran") +
  theme_minimal()

