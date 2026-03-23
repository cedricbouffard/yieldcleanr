#' Creer une carte de rendement avec des polygones ggplot
#'
#' Cette fonction genere des elements ggplot pour visualiser les donnees de rendement
#' sous forme de carte thematique avec des polygones colores selon les classes de rendement.
#'
#' @param rdt_data Objet sf contenant les donnees de rendement avec une colonne 'yield'
#' @param breaks Vecteur numerique optionnel definissant les seuils de classification.
#'   Si NULL (par defaut), les seuils sont calcules automatiquement avec des bins 
#'   de taille uniforme (0.25t, 0.5t, 1t, 1.5t ou 2t selon la distribution des donnees)
#' @param n_bins Nombre de bins a creer (par defaut: 7). Ignore si breaks est fourni.
#' @param col_palette Vecteur de couleurs pour les classes de rendement.
#'   Si NULL (par defaut), une palette est generee automatiquement avec le bin median
#'   en jaune, les bins inferieurs en rouge/orange et les superieurs en vert.
#' @param line_color Couleur des bordures des polygones.
#'   Par defaut : "black"
#' @param line_alpha Transparence des bordures (0-1).
#'   Par defaut : 0.1
#' @param line_size Epaisseur des bordures.
#'   Par defaut : 0.05
#'
#' @return Une liste d'elements ggplot (geom_sf + scale_fill_manual) a ajouter a un ggplot
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Utilisation de base avec seuils automatiques
#' ggplot() +
#'   geom_yield_map_polygon(rdt_data) +
#'   theme_minimal()
#'
#' # Avec seuils personnalises
#' breaks <- c(5000, 8000, 12000, 15000, 20000)
#' ggplot() +
#'   geom_yield_map_polygon(rdt_data, breaks = breaks) +
#'   theme_minimal()
#'
#' # Avec palette de couleurs personnalisee
#' ggplot() +
#'   geom_yield_map_polygon(
#'     rdt_data,
#'     col_palette = c("red", "orange", "yellow", "green", "blue")
#'   ) +
#'   theme_minimal()
#' }
geom_yield_map_polygon <- function(
    rdt_data,
    breaks = NULL,
    n_bins = 7,
    col_palette = NULL,
    line_color = "black",
    line_alpha = 0.1,
    line_size = 0.05
) {

  # Verification des donnees
  if (!"yield" %in% names(rdt_data)) {
    rlang::abort("La colonne 'yield' est requise dans rdt_data")
  }

  if (!inherits(rdt_data, "sf")) {
    rlang::warn("rdt_data devrait etre un objet sf pour une visualisation cartographique")
  }

  # Calcul des seuils de classification
  if (!is.null(breaks)) {
    br <- breaks
    n_bins <- length(br) - 1
  } else {
    # Utiliser la meme logique que le rapport : bins de taille uniforme
    valid_yield <- rdt_data$yield[!is.na(rdt_data$yield)]
    med <- median(valid_yield, na.rm = TRUE)
    yield_min <- min(valid_yield, na.rm = TRUE)
    yield_max <- max(valid_yield, na.rm = TRUE)
    
    # Determiner la taille des bins
    yield_range <- yield_max - yield_min
    
    # Progression: 0.25t, 0.5t, 1t, 1.5t, 2t, 2.5t, 3t, 3.5t, 4t, 4.5t, 5t
    calc_edge_pct <- function(bs) {
      med_rounded <- floor(med / bs) * bs
      breaks_temp <- c()
      for (i in -3:3) breaks_temp <- c(breaks_temp, med_rounded + (i * bs))
      breaks_temp <- c(breaks_temp, med_rounded + (4 * bs))
      if (yield_min < breaks_temp[1]) breaks_temp[1] <- floor(yield_min / bs) * bs - bs
      if (yield_max > breaks_temp[length(breaks_temp)]) breaks_temp[length(breaks_temp)] <- ceiling(yield_max / bs) * bs + bs
      
      cuts <- cut(valid_yield, breaks = breaks_temp, include.lowest = TRUE)
      counts <- table(cuts)
      pct <- as.numeric(counts) / sum(counts) * 100
      (pct[1] + pct[length(pct)]) / 2
    }
    
    # Tester les tailles de bins
    bin_sizes <- c(250, 500, 1000, 1500, 2000, 2500, 3000, 3500, 4000, 4500, 5000)
    bin_size <- 500  # defaut
    
    for (bs in bin_sizes) {
      avg_edge <- calc_edge_pct(bs)
      if (avg_edge < 2) {
        bin_size <- 250
        break
      } else if (avg_edge <= 10) {
        bin_size <- bs
        break
      } else if (bs == 2500) {
        bin_size <- 2500
      }
    }
    
    # Creer les breaks avec la taille choisie
    med_rounded <- floor(med / bin_size) * bin_size
    br <- c()
    for (i in -3:3) br <- c(br, med_rounded + (i * bin_size))
    br <- c(br, med_rounded + (4 * bin_size))
    
    # Ajuster les extremites
    if (yield_min < br[1]) br[1] <- floor(yield_min / bin_size) * bin_size - bin_size
    if (yield_max > br[length(br)]) br[length(br)] <- ceiling(yield_max / bin_size) * bin_size + bin_size
    
    n_bins <- length(br) - 1
    
    rlang::inform(paste(
      "Seuils calcules automatiquement :",
      paste(round(br, 0), collapse = ", ")
    ))
  }

  # Identifier le bin median
  med <- median(rdt_data$yield, na.rm = TRUE)
  median_bin_index <- which(br[-length(br)] <= med & med < br[-1])
  if (length(median_bin_index) == 0) median_bin_index <- ceiling(n_bins / 2)

  # Palette de couleurs par defaut si non fournie
  if (is.null(col_palette)) {
    # Creer une palette avec le bin median en jaune
    bin_colors <- character(n_bins)
    if (median_bin_index > 1) {
      reds <- grDevices::colorRampPalette(c("#C0392B", "#E74C3C", "#E67E22"))(median_bin_index - 1)
      bin_colors[1:(median_bin_index-1)] <- reds
    }
    bin_colors[median_bin_index] <- "#F1C40F"  # Jaune pour le median
    if (median_bin_index < n_bins) {
      greens <- grDevices::colorRampPalette(c("#AED136", "#27AE60", "#1E8449"))(n_bins - median_bin_index)
      bin_colors[(median_bin_index+1):n_bins] <- greens
    }
    col_palette <- bin_colors
  }

  # Classification des rendements
  rdt_data$yield_classe <- cut(rdt_data$yield, breaks = br, labels = 1:n_bins, include.lowest = TRUE)

  # Nombre de classes effectives presentes dans les donnees
  n_classes <- length(unique(stats::na.omit(rdt_data$yield_classe)))

  if (n_classes == 0) {
    rlang::warn("Aucune donnee valide pour la classification")
  } else {
    rlang::inform(paste("Carte creee avec", n_classes, "classes de rendement"))
  }

  # Creation des labels
  labels <- c()
  for (i in 1:n_bins) {
    val1 <- br[i] / 1000
    val2 <- br[i+1] / 1000
    decimals <- ifelse(bin_size <= 250, 2, ifelse(bin_size <= 500, 1, 0))
    v1 <- round(val1, decimals)
    v2 <- round(val2, decimals)
    
    if (i == 1) {
      labels <- c(labels, paste0("< ", v2, " t/ha"))
    } else if (i == n_bins) {
      labels <- c(labels, paste0("> ", v1, " t/ha"))
    } else {
      labels <- c(labels, paste0(v1, " - ", v2, " t/ha"))
    }
  }

  # Creation des elements ggplot
  plot_elements <- list(
    ggplot2::geom_sf(
      data = rdt_data,
      mapping = ggplot2::aes(fill = factor(yield_classe, levels = 1:n_bins)),
      color = scales::alpha(line_color, line_alpha),
      linewidth = line_size
    ),
    ggplot2::scale_fill_manual(
      values = col_palette,
      labels = labels,
      name = "Classe de rendement",
      na.value = "grey50"
    )
  )

  return(plot_elements)
}
