#' Unified Yield Data Cleaning Function
#'
#' Cette fonction exécute le pipeline complet de nettoyage des données de rendement
#' avec support pour les sorties en unités métriques ou impériales, et avec ou sans
#' géométries SF (polygones ou points).
#'
#' @param file_path Path to input file (txt/csv format)
#' @param metrique TRUE for metric units (kg/ha), FALSE for imperial (bu/acre)
#' @param polygon TRUE for SF polygon output, FALSE for tibble output
#' @param params List of AYCE parameters (see details)
#' @param output_file Optional path to save output (CSV or GeoJSON)
#' @param log_file Optional path to save log file
#' @return Cleaned data as tibble or SF object depending on parameters
#' @export
#' @examples
#' \dontrun{
#' # Metric output with polygons (SF object)
#' sf_result <- clean_yield("data.txt", metrique = TRUE, polygon = TRUE)
#' plot(sf_result["Yield_kg_ha"])
#'
#' # Imperial output as tibble
#' data_result <- clean_yield("data.txt", metrique = FALSE, polygon = FALSE)
#'
#' # Metric output as tibble (no geometry)
#' data_metric <- clean_yield("data.txt", metrique = TRUE, polygon = FALSE)
#'
#' # With custom parameters
#' result <- clean_yield("data.txt",
#'   metrique = TRUE,
#'   polygon = TRUE,
#'   params = list(
#'     delay_range = -10:25,
#'     n_swaths = 5,
#'     lsd_limit = 2.5
#'   )
#' )
#' }
clean_yield <- function(file_path, metrique = TRUE, polygon = TRUE,
                        params = NULL, output_file = NULL, log_file = NULL) {

  # ----- Default AYCE parameters -----
  default_params <- list(
    # PCDI parameters
    delay_range = -25:10,
    n_iterations = 5,
    noise_level = 0.05,

    # Threshold parameters (balanced for good cleaning)
    yllim = 0.10,      # Yield lower quantile limit
    yulim = 0.90,      # Yield upper quantile limit
    yscale = 1.1,      # Yield IQR multiplier
    v_lim = 0.05,      # Velocity lower quantile limit
    v_ulim = 0.95,     # Velocity upper quantile limit
    v_scale = 1.1,     # Velocity IQR multiplier
    minv_abs = 0.5,    # Absolute minimum velocity (m/s)
    miny_abs = 0,      # Absolute minimum yield
    gbuffer = 100,     # Position buffer (meters)

    # Overlap filter parameters
    cellsize_overlap = 0.3,    # Grid cell size (meters) - USDA standard
    overlap_threshold = 0.5,   # Maximum overlap ratio

    # Local SD filter parameters
    n_swaths = 5,              # Grid cell size in swath widths
    lsd_limit = 2.4,           # Local SD multiplier (balanced)
    min_cells = 3,             # Minimum observations per cell

    # Auto-detection parameters
    n_std = 3                  # Number of std devs for auto-range detection
  )

  # Merge parameters
  params <- modifyList(default_params, params %||% list())

  # ----- Step 1: Read raw data -----
  rlang::inform("================================================")
  rlang::inform("   Yield Data Cleaning Pipeline               ")
  if (metrique) {
    rlang::inform("   Output: Metric (kg/ha)                      ")
  } else {
    rlang::inform("   Output: Imperial (bu/acre)                  ")
  }
  if (polygon) {
    rlang::inform("   Geometry: Polygons                          ")
  } else {
    rlang::inform("   Geometry: None (tibble output)              ")
  }
  rlang::inform("================================================")
  rlang::inform("")

  rlang::inform("Step 1: Loading data...")
  data <- read_yield_data(file_path)
  data_raw <- data
  rlang::inform(paste("  -", nrow(data), "raw observations loaded"))

  # ----- Step 2: UTM conversion -----
  rlang::inform("Step 2: Converting to UTM coordinates...")
  data <- latlon_to_utm(data)

  # ----- Step 3: PCDI -----
  rlang::inform("Step 3: PCDI - Flow Delay Optimization...")
  pcdi_result <- apply_pcdi(data,
    delay_range = params$delay_range,
    n_iterations = params$n_iterations,
    noise_level = params$noise_level
  )
  flow_delay <- pcdi_result$optimal_delay
  rlang::inform(paste("  Optimal delay:", flow_delay, "seconds"))

  # ----- Step 3b: Calculate initial yield (before flow delay) for threshold estimation -----
  rlang::inform("Step 3b: Calculating initial yield for threshold estimation...")
  data <- convert_flow_to_yield(data)

  # ----- Step 4: Auto thresholds -----
  rlang::inform("Step 4: Calculating auto thresholds...")
  thresholds <- calculate_auto_thresholds(data,
    yllim = params$yllim, yulim = params$yulim, yscale = params$yscale,
    vllim = params$v_lim, vulim = params$v_ulim, vscale = params$v_scale,
    minv_abs = params$minv_abs, miny_abs = params$miny_abs,
    gbuffer = params$gbuffer
  )

  # ----- Step 5: Header filter -----
  rlang::inform("Step 5: Header filter...")
  data <- data |> dplyr::filter(HeaderStatus %in% c(1, 33) | is.na(HeaderStatus))
  rlang::inform(paste("  Rows:", nrow(data)))

  # ----- Step 6: GPS filter -----
  rlang::inform("Step 6: GPS filter...")
  data <- data |> dplyr::filter(is.na(GPSStatus) | GPSStatus >= 4)
  rlang::inform(paste("  Rows:", nrow(data)))

  # ----- Step 7: Calculate velocity -----
  rlang::inform("Step 7: Calculating velocity...")
  data <- data |>
    dplyr::mutate(
      velocity = sqrt((X - dplyr::lag(X))^2 + (Y - dplyr::lag(Y))^2) /
                    dplyr::coalesce(Interval, 1)
    )
  data$velocity[is.na(data$velocity) | !is.finite(data$velocity)] <- 0

  # ----- Step 8: Velocity filter -----
  rlang::inform("Step 8: Velocity filter...")
  data <- data |>
    dplyr::filter(velocity >= thresholds$min_velocity & velocity <= thresholds$max_velocity)
  rlang::inform(paste("  Rows:", nrow(data)))

  # ----- Step 9: Flow delay correction -----
  rlang::inform(paste("Step 9: Flow delay correction (", flow_delay, "s)..."))
  data <- apply_flow_delay(data, delay = -flow_delay)
  rlang::inform(paste("  Rows:", nrow(data)))

  # ----- Step 9b: Calculate yield from delayed Flow -----
  rlang::inform("Step 9b: Calculating yield from delayed flow...")
  data <- convert_flow_to_yield(data)

  # ----- Step 9c: Recalculate yield thresholds after delay correction -----
  rlang::inform("Step 9c: Recalculating yield thresholds after delay correction...")
  thresholds <- calculate_auto_thresholds(data,
    yllim = params$yllim, yulim = params$yulim, yscale = params$yscale,
    vllim = params$v_lim, vulim = params$v_ulim, vscale = params$v_scale,
    minv_abs = params$minv_abs, miny_abs = params$miny_abs,
    gbuffer = params$gbuffer
  )

  # ----- Step 9d: Validate Pass column using direction analysis -----
  rlang::inform("Step 9d: Validating Pass column using direction analysis...")
  if (all(c("X", "Y") %in% names(data))) {
    n <- nrow(data)

    # If Pass column doesn't exist or has unreasonable values, skip validation
    if (!("Pass" %in% names(data)) || all(is.na(data$Pass))) {
      rlang::inform("  No valid Pass column, skipping validation")
    } else {
      n_original_passes <- length(unique(data$Pass))
      rlang::inform(paste("  Pass column has", n_original_passes, "unique values"))

      # Check if Pass count is reasonable (10-500 for typical fields)
      if (n_original_passes >= 10 && n_original_passes <= 500) {
        rlang::inform("  Pass column appears reasonable, using as-is")
      } else {
        rlang::inform(paste("  Warning: Pass column has", n_original_passes,
                           "passes, which may be unusual"))
      }
    }
  }

  # ----- Step 9e: Remove boundary points (flow delay effect) -----
  rlang::inform("Step 9e: Removing boundary points affected by flow delay...")
  if ("Pass" %in% names(data) && "Yield_buacre" %in% names(data) && !all(is.na(data$Pass)) && "Interval" %in% names(data)) {
    n_before <- nrow(data)
    abs_delay <- abs(flow_delay)

    data <- data |>
      dplyr::arrange(GPS_Time) |>
      dplyr::group_by(Pass) |>
      dplyr::mutate(
        row_in_pass = dplyr::row_number(),
        n_in_pass = dplyr::n(),
        interval_cumsum = cumsum(Interval),
        total_interval = sum(Interval, na.rm = TRUE),
        interval_from_end = total_interval - interval_cumsum
      ) |>
      dplyr::ungroup()

    if (flow_delay < 0) {
      boundary <- "end"
      n_delay_points <- round(mean(data |>
        dplyr::group_by(Pass) |>
        dplyr::filter(interval_from_end <= abs_delay | is.na(interval_from_end)) |>
        dplyr::summarise(n = dplyr::n()) |>
        dplyr::ungroup() |>
        dplyr::pull(n), na.rm = TRUE))
      data <- data |>
        dplyr::group_by(Pass) |>
        dplyr::filter(interval_from_end > abs_delay | is.na(interval_from_end)) |>
        dplyr::ungroup()
    } else {
      boundary <- "beginning"
      n_delay_points <- round(mean(data |>
        dplyr::group_by(Pass) |>
        dplyr::filter(interval_cumsum <= abs_delay | is.na(interval_cumsum)) |>
        dplyr::summarise(n = dplyr::n()) |>
        dplyr::ungroup() |>
        dplyr::pull(n), na.rm = TRUE))
      data <- data |>
        dplyr::group_by(Pass) |>
        dplyr::filter(interval_cumsum > abs_delay | is.na(interval_cumsum)) |>
        dplyr::ungroup()
    }

    data$interval_cumsum <- NULL
    data$total_interval <- NULL
    data$interval_from_end <- NULL

    n_removed <- n_before - nrow(data)
    rlang::inform(paste("  ", n_removed, "boundary points removed (",
                       boundary, ", delay:", flow_delay, "s =",
                       n_delay_points, "points)"))
  } else {
    rlang::inform("  Missing Pass, Yield, or Interval column, skipping boundary removal")
    data <- data
  }

  # ----- Step 10: Remove zero yield -----
  rlang::inform("Step 10: Removing zero yield points...")
  n_before <- nrow(data)
  data <- data |> dplyr::filter(Yield_buacre > 0)
  n_removed <- n_before - nrow(data)
  if (n_removed > 0) {
    rlang::inform(paste("  ", n_removed, "zero yield points removed"))
  }
  rlang::inform(paste("  Rows:", nrow(data)))

  # ----- Step 11: Yield range filter -----
  rlang::inform("Step 11: Yield range filter...")
  data <- filter_yield_range(data,
    min_yield = thresholds$min_yield,
    max_yield = thresholds$max_yield
  )
  rlang::inform(paste("  Rows:", nrow(data)))

  # ----- Step 12: Moisture filter -----
  rlang::inform("Step 12: Moisture filter (auto-detection)...")
  data <- filter_moisture_range(data, n_std = params$n_std)
  rlang::inform(paste("  Rows:", nrow(data)))

  # ----- Step 13: Overlap filter -----
  rlang::inform("Step 13: Bitmap Overlap Filter...")
  data <- apply_overlap_filter(data,
    cellsize = params$cellsize_overlap,
    overlap_threshold = params$overlap_threshold
  )
  rlang::inform(paste("  Rows:", nrow(data)))

  # ----- Step 14: Local SD filter -----
  rlang::inform("Step 14: Localized SD Filter...")
  data <- apply_local_sd_filter(data,
    n_swaths = params$n_swaths,
    lsd_limit = params$lsd_limit,
    min_cells = params$min_cells
  )
  rlang::inform(paste("  Rows:", nrow(data)))

  # ----- Step 15: Validation -----
  rlang::inform("Step 15: Validation & Quality Control...")
  validation <- ayce_validate(data, data_raw, pcdi_result, thresholds)
  rlang::inform(paste("  Retention rate:", round(validation$retention_rate * 100, 1), "%"))

  # ----- Step 16: Format output -----
  rlang::inform("Step 16: Formatting output...")

   # Calculate unit conversions
   if (metrique) {
     data$Yield_kg_ha <- data$Yield_buacre * 67.25  # kg/ha
     data$Flow_kg_s <- data$Flow * 0.453592         # kg/s
     data$Yield_final <- data$Yield_kg_ha
     data$Flow_final <- data$Flow_kg_s
     yield_label <- "Yield_kg_ha"
     flow_label <- "Flow_kg_s"
     unit_label <- "kg/ha"
   } else {
     data$Yield_final <- data$Yield_buacre
     data$Flow_final <- data$Flow
     yield_label <- "Yield_bu_ac"
     flow_label <- "Flow_lbs_s"
     unit_label <- "bu/acre"
   }

   # For tibble output, remove intermediate columns
   if (!polygon) {
     data$Yield_buacre <- NULL
     data$Flow <- NULL
     if (metrique) {
       data$Yield_kg_ha <- NULL
       data$Flow_kg_s <- NULL
     }
   }

    if (polygon) {
      # ----- Calculate heading for polygons -----
      rlang::inform("Step 16b: Calculating heading...")
      data <- data |>
        dplyr::mutate(
          heading = atan2(
            dplyr::lag(Longitude, default = Longitude[1]) - Longitude,
            dplyr::lag(Latitude, default = Latitude[1]) - Latitude
          ) * 180 / pi
        )
      data$heading[is.na(data$heading)] <- 0

      # ----- Smooth heading to reduce noise (within segments, with local rolling for curves) -----
      rlang::inform("Step 16b.1: Smoothing heading within segments...")
      # Calculate heading differences to detect turns
      n <- nrow(data)
      heading_diff <- c(data$heading[2:n] - data$heading[1:(n-1)], 0)
      # Handle circular wrap-around for differences (e.g., -179 to +179 is only 2 degrees)
      heading_diff <- ((heading_diff + 180) %% 360) - 180
      # Detect turns (threshold: 30 degrees)
      turn_threshold <- 30
      is_turn <- abs(heading_diff) > turn_threshold

      # Identify segments (runs of consecutive non-turns)
      segment_id <- cumsum(c(TRUE, is_turn[1:(n-1)]))
      data$segment_id <- segment_id

      # Smooth heading within each segment
      data <- data |>
        dplyr::group_by(segment_id) |>
        dplyr::mutate(
          row_in_segment = dplyr::row_number(),
          n_in_segment = dplyr::n(),
          # Calculate heading variation within segment (circular SD)
          heading_var = {
            angles_rad <- heading * pi / 180
            sin_mean <- mean(sin(angles_rad), na.rm = TRUE)
            cos_mean <- mean(cos(angles_rad), na.rm = TRUE)
            R <- sqrt(sin_mean^2 + cos_mean^2)
            circular_sd <- ifelse(R < 0.001, 0, sqrt(-2 * log(R)) * 180 / pi)
            circular_sd
          },
          heading_smooth = dplyr::case_when(
            # Keep first and last point of each segment (boundaries) unchanged
            row_in_segment == 1 | row_in_segment == n_in_segment ~ heading,
            # Curved segments (>15° variation): use small rolling window (3 obs)
            heading_var > 15 ~ {
              # Local rolling mean with small window for curved paths
              zoo::rollapply(
                heading,
                width = 3,
                align = "center",
                partial = TRUE,
                FUN = function(x) {
                  angles_rad <- x * pi / 180
                  real_part <- mean(cos(angles_rad), na.rm = TRUE)
                  imag_part <- mean(sin(angles_rad), na.rm = TRUE)
                  smoothed_angle <- atan2(imag_part, real_part) * 180 / pi
                  smoothed_angle
                }
              )
            },
            # Straight segments (<=15° variation): use full segment mean
            n_in_segment >= 3 ~ {
              angles_rad <- heading * pi / 180
              real_part <- mean(cos(angles_rad), na.rm = TRUE)
              imag_part <- mean(sin(angles_rad), na.rm = TRUE)
              smoothed_angle <- atan2(imag_part, real_part) * 180 / pi
              smoothed_angle
            },
            # Keep original for short segments
            TRUE ~ heading
          )
        ) |>
        dplyr::ungroup()

      data$heading <- data$heading_smooth
      data$heading[is.na(data$heading)] <- 0
      data$segment_id <- NULL
      data$row_in_segment <- NULL
      data$n_in_segment <- NULL
      data$heading_var <- NULL

     # ----- Create SF object with polygons -----
     rlang::inform("Step 16c: Creating SF polygon object...")

     # Ensure metric columns exist for polygon creation
     if (!"Swath_m" %in% names(data)) {
       data$Swath_m <- data$Swath * 0.0254
     }
     if (!"Distance_m" %in% names(data)) {
       data$Distance_m <- data$Distance * 0.0254
     }
     if (!"Altitude_m" %in% names(data)) {
      data$Altitude_m <- data$Altitude * 0.3048
      }

      # Create SF object
      sf_result <- data_to_sf(data, crs = 4326)

      # Select output columns (use existing column names)
      output_cols <- c("Yield_kg_ha", "Flow_kg_s", "Moisture", "Swath_m", "Distance_m",
                       "Heading_deg", "Altitude_m", "HeaderStatus", "Pass",
                       "GPS_Time", "Longitude", "Latitude", "X_utm", "Y_utm",
                       "Variety", "GrainType")

      sf_output <- sf_result |>
        dplyr::select(dplyr::any_of(output_cols)) |>
        dplyr::rename(
          Yield = Yield_kg_ha,
          Flow = Flow_kg_s,
          Heading = Heading_deg
        )

      # ----- Step 17: Export -----
      if (!is.null(output_file)) {
        rlang::inform("Step 17: Exporting...")
        ext <- tolower(tools::file_ext(output_file))
        if (ext == "geojson" || ext == "json") {
          sf::st_write(sf_output, output_file, driver = "GeoJSON", delete_dsn = TRUE)
        } else if (ext %in% c("shp", "gpkg")) {
          sf::st_write(sf_output, output_file, driver = toupper(ext), delete_dsn = TRUE)
        } else {
          # Default to CSV
          utils::write.csv(sf_output, output_file, row.names = FALSE)
        }
        rlang::inform(paste("  Saved to:", output_file))
      }

      # ----- Step 18: Generate log -----
      if (!is.null(log_file)) {
        rlang::inform("Step 18: Generating log...")
        generate_clean_yield_log(sf_output, data_raw, params, pcdi_result,
                                thresholds, validation, log_file, file_path)
      }

      rlang::inform("")
      rlang::inform("================================================")
      rlang::inform(paste("Complete:", nrow(sf_output), "observations cleaned"))
      rlang::inform(paste("Mean yield:", round(mean(sf_output$Yield), 0), unit_label))
      rlang::inform("================================================")

      return(sf_output)

  } else {
    # ----- Tibble output -----
    # Rename final columns
    data$Yield <- data$Yield_final
    data$Flow <- data$Flow_final

    # Select output columns
    output_cols <- c("X", "Y", "Latitude", "Longitude", "Flow", "Moisture",
                     "Swath", "Distance", "Pass", "HeaderStatus", "GPS_Time",
                     "Interval", "Yield", "Variety", "GrainType", "Altitude")

    data_output <- data |>
      dplyr::select(dplyr::any_of(output_cols)) |>
      dplyr::mutate(
        HeaderStatus = as.integer(HeaderStatus)
      ) |>
      dplyr::arrange(GPS_Time)

    # ----- Step 17: Export -----
    if (!is.null(output_file)) {
      rlang::inform("Step 17: Exporting...")
      ext <- tolower(tools::file_ext(output_file))
      if (ext == "csv") {
        utils::write.csv(data_output, output_file, row.names = FALSE)
      } else if (ext == "geojson" || ext == "json") {
        sf_data <- sf::st_as_sf(data_output,
          coords = c("Longitude", "Latitude"),
          crs = 4326
        )
        sf::st_write(sf_data, output_file, driver = "GeoJSON", delete_dsn = TRUE)
      }
      rlang::inform(paste("  Saved to:", output_file))
    }

    # ----- Step 18: Generate log -----
    if (!is.null(log_file)) {
      rlang::inform("Step 18: Generating log...")
      generate_clean_yield_log(data_output, data_raw, params, pcdi_result,
                              thresholds, validation, log_file, file_path)
    }

    rlang::inform("")
    rlang::inform("================================================")
    rlang::inform(paste("Complete:", nrow(data_output), "observations cleaned"))
    rlang::inform(paste("Mean yield:", round(mean(data_output$Yield), 1), unit_label))
    rlang::inform("================================================")

    # Add attributes for compatibility
    attr(data_output, "ayce_params") <- params
    attr(data_output, "pcdi_result") <- pcdi_result
    attr(data_output, "thresholds") <- thresholds
    attr(data_output, "validation") <- validation

    return(data_output)
  }
}


#' Generate log for clean_yield function
#' @noRd
generate_clean_yield_log <- function(data_clean, data_raw, params, pcdi_result,
                                     thresholds, validation, log_file,
                                     source_file = "unknown") {

  n_raw <- nrow(data_raw)
  n_clean <- nrow(data_clean)
  n_removed <- n_raw - n_clean

  # Calculate stats
  stats_raw <- data_raw |>
    dplyr::summarise(
      mean_yield = mean(Yield_buacre, na.rm = TRUE),
      sd_yield = stats::sd(Yield_buacre, na.rm = TRUE),
      cv = sd_yield / mean_yield * 100,
      n = dplyr::n()
    )

  stats_clean <- data_clean |>
    dplyr::summarise(
      mean_yield = mean(Yield, na.rm = TRUE),
      sd_yield = stats::sd(Yield, na.rm = TRUE),
      cv = sd_yield / mean_yield * 100,
      n = dplyr::n()
    )

  # Get RSC value
  rsc_value <- tryCatch({
    pcdi_result$rsc_values$mean_rsc[pcdi_result$rsc_values$delay == pcdi_result$optimal_delay][1]
  }, error = function(e) NA)

  log_lines <- c(
    "=======================================================",
    "         Yield Data Cleaning Pipeline - Log           ",
    "=======================================================",
    paste0("Date: ", Sys.time()),
    paste0("Source file: ", source_file),
    "",
    "--- SUMMARY ---",
    paste0("Original points: ", n_raw),
    paste0("Cleaned points: ", n_clean),
    paste0("Points removed: ", n_removed, " (", round(n_removed/n_raw*100, 1), "%)"),
    paste0("Retention rate: ", round(validation$retention_rate * 100, 1), "%"),
    "",
    "--- PCDI RESULTS (Flow Delay) ---",
    paste0("Optimal Delay: ", pcdi_result$optimal_delay, " seconds"),
    paste0("RSC at optimal: ", round(rsc_value, 4) %||% "NA"),
    paste0("Stability (CV): ", round(pcdi_result$stability, 4) %||% "NA"),
    "",
    "--- AUTOMATIC THRESHOLDS ---",
    paste0("Yield Range: [", round(thresholds$min_yield, 2), " - ", round(thresholds$max_yield, 2), "]"),
    paste0("Velocity Range: [", round(thresholds$min_velocity, 2), " - ", round(thresholds$max_velocity, 2), " m/s]"),
    "",
    "--- FILTER PARAMETERS ---",
    paste0("Overlap - Cellsize: ", params$cellsize_overlap, " m"),
    paste0("Overlap - Threshold: ", params$overlap_threshold * 100, "%"),
    paste0("Local SD - Swaths: ", params$n_swaths),
    paste0("Local SD - Limit: ", params$lsd_limit, " SD"),
    "",
    "--- STATISTICS ---",
    paste0("Raw:    Mean=", round(stats_raw$mean_yield, 1),
           " SD=", round(stats_raw$sd_yield, 1),
           " CV=", round(stats_raw$cv, 1), "%",
           " N=", stats_raw$n),
    paste0("Clean:  Mean=", round(stats_clean$mean_yield, 1),
           " SD=", round(stats_clean$sd_yield, 1),
           " CV=", round(stats_clean$cv, 1), "%",
           " N=", stats_clean$n),
    paste0("CV Improvement: ", round(validation$cv_improvement * 100, 1), "%"),
    "",
    "======================================================="
  )

  writeLines(log_lines, log_file)
  rlang::inform(paste("Log file saved:", log_file))
}
