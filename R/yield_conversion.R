#' Convert grain flow to yield in bushels per acre
#'
#' Converts raw grain flow (LBS/SEC) to yield (Bushels/acre)
#' using the formula:
#' Yield (bu/acre) = (Flow × Interval × 43560) / (lbs_per_bushel × Swath_ft × Distance_ft)
#'
#' Where:
#'   - Flow = grain flow in lbs/sec
#'   - Interval = time interval in seconds
#'   - Swath_ft = swath width in feet (Swath_in / 12)
#'   - Distance_ft = distance traveled during interval in feet (Distance_in / 12)
#'   - 43560 = sq ft per acre
#'   - lbs_per_bushel = lbs per bushel (varies by grain type)
#'
#' Standard conversion factors:
#'   - Corn (maïs): 56 lbs/bu @ 15.5% moisture
#'   - Soybeans (soja): 60 lbs/bu @ 13% moisture
#'   - Wheat/cereals: 60 lbs/bu
#'
#' @param data Tibble with Flow, Interval, Swath, Distance columns
#' @param lbs_per_bushel LBS per bushel. If NULL, auto-detect from GrainType column.
#'   Default 56 for corn. Use 60 for soybeans and cereals.
#' @param sqft_per_acre Square feet per acre (default 43560)
#' @param inches_per_foot Inches per foot (default 12)
#' @return Data with Yield_buacre column added
#' @noRd
#' @examples
#' \dontrun{
#' # Auto-detect from grain type
#' data <- convert_flow_to_yield(data)
#'
#' # Explicit for corn
#' data <- convert_flow_to_yield(data, lbs_per_bushel = 56)
#'
#' # For soybeans/cereals
#' data <- convert_flow_to_yield(data, lbs_per_bushel = 60)
#' }
convert_flow_to_yield <- function(data, lbs_per_bushel = NULL,
                                   sqft_per_acre = 43560,
                                   inches_per_foot = 12) {

  if (!all(c("Flow", "Interval", "Swath", "Distance") %in% names(data))) {
    rlang::warn("Colonnes Flow, Interval, Swath, Distance requises")
    return(data)
  }

  # Auto-detect lbs_per_bushel from GrainType if not specified
  if (is.null(lbs_per_bushel)) {
    lbs_per_bushel <- get_lbs_per_bushel(data)
  }

  # Calculate Width_ft and Distance_ft
  # Swath_ft = Swath / 12 (convert inches to feet)
  # Distance_ft = Distance / 12 (convert inches to feet)
  # Distance is the distance traveled during the Interval

  # Filter out rows with zero values (would cause division by zero or Inf)
  data <- data |>
    dplyr::filter(.data$Distance > 0, .data$Swath > 0, .data$Flow > 0)

  data <- data |>
    dplyr::mutate(
      Width_ft = .data$Swath / inches_per_foot,
      Distance_ft = .data$Distance / inches_per_foot,
      # Yield = (Flow × Interval × 43560) / (lbs_per_bushel × Width_ft × Distance_ft)
      Yield_buacre = (.data$Flow * .data$Interval * sqft_per_acre) /
                     (lbs_per_bushel * .data$Width_ft * .data$Distance_ft)
    )

  # Remove temporary columns
  data <- data |>
    dplyr::select(-Width_ft, -Distance_ft)

  # Calculate mean excluding Inf values
  finite_yield <- data$Yield_buacre[is.finite(data$Yield_buacre)]
  mean_yield <- if (length(finite_yield) > 0) mean(finite_yield, na.rm = TRUE) else NA

  rlang::inform(paste("Yield calculé:", round(mean_yield, 1),
                      "bu/acre (lbs/bu =", lbs_per_bushel, ")"))

  return(data)
}


#' Get lbs per bushel from grain type
#'
#' Returns the standard conversion factor based on grain type.
#'
#' @param data Tibble with GrainType or Grain_Type column
#' @return LBS per bushel (56 for corn, 60 for soybeans/cereals)
#' @noRd
get_lbs_per_bushel <- function(data) {
  # Check for GrainType or Grain_Type column
  grain_col <- if ("GrainType" %in% names(data)) {
    "GrainType"
  } else if ("Grain_Type" %in% names(data)) {
    "Grain_Type"
  } else {
    NULL
  }

  if (!is.null(grain_col)) {
    grain <- tolower(unique(data[[grain_col]]))

    # Check for soybeans/soja
    if (any(grepl("soja|soy", grain))) {
      return(60)
    }

    # Check for wheat/cereals
    if (any(grepl("blé|wheat|wheat|cereal|avoine|barley|orge", grain))) {
      return(60)
    }

    # Default to corn (56 lbs/bu)
    rlang::inform(paste("GrainType non reconnu, utilisation 56 lbs/boisseau (maïs par défaut)"))
    return(56)
  }

  # Default if no GrainType column
  rlang::inform("Pas de colonne GrainType, utilisation 56 lbs/boisseau (maïs par défaut)")
  return(56)
}


#' Alternative: Convert with simplified formula
#'
#' Yield = Flow × 9335 / (Swath × Velocity)
#' This simplifies the formula when using velocity directly.
#'
#' @param data Tibble with Flow, Swath, Velocity_ft_s columns
#' @return Data with Yield_buacre column added
#' @noRd
convert_flow_simple <- function(data) {
  if (!all(c("Flow", "Swath", "Distance") %in% names(data))) {
    rlang::warn("Colonnes Flow, Swath, Distance requises")
    return(data)
  }

  # Using Distance (inches) and Interval (seconds) to get velocity
  data <- data |>
    dplyr::mutate(
      Velocity_ft_s = .data$Distance / .data$Interval / 12,  # ft/s
      Yield_buacre = .data$Flow * 43560 / (56 * .data$Swath / 12 * .data$Velocity_ft_s)
    )

  return(data)
}


#' Run AYCE with proper yield conversion
#'
#' Complete AYCE pipeline that outputs yield in bushels/acre.
#'
#' @param file_path Path to input file
#' @param output_file Path to output CSV
#' @param log_file Path to log file
#' @param params AYCE parameters
#' @return Cleaned data with yield in bushels/acre
#' @noRd
#' @examples
#' \dontrun{
#' cleaned <- ayce_with_yield_conversion("data.txt", "output.csv", "log.txt")
#' }
ayce_with_yield_conversion <- function(file_path, output_file = NULL,
                                        log_file = NULL, params = NULL) {

  rlang::inform("================================================")
  rlang::inform("   AYCE with Yield Conversion                  ")
  rlang::inform("   (LBS/SEC → Bushels/acre)                   ")
  rlang::inform("================================================")

  # Default parameters
  default_params <- list(
    delay_range = 0:20,
    n_iterations = 10,
    noise_level = 0.05,
    yscale = 1.5,
    v_scale = 1.5,
    minv_abs = 0.5,
    miny_abs = 0,
    cellsize_overlap = 0.3,       # 30cm cellsize (USDA standard)
    overlap_threshold = 0.5,      # 50% max overlap
    n_swaths = 5,
    lsd_limit = 3,
    lbs_per_bushel = 56,          # Corn
    n_std = 3                     # Number of std devs for auto-range detection
  )

  params <- modifyList(default_params, params %||% list())

  # Step 1: Read data
  rlang::inform("Step 1: Loading data...")
  data <- read_yield_data(file_path)
  data_raw <- data
  rlang::inform(paste("  -", nrow(data), "rows loaded"))
  rlang::inform(paste("  Flow range:", min(data$Flow), "-", max(data$Flow), "LBS/sec"))

  # Step 2: UTM conversion
  rlang::inform("Step 2: Converting to UTM...")
  data <- latlon_to_utm(data)

  # Step 3: Calculate velocity
  rlang::inform("Step 3: Calculating velocity...")
  data <- data |>
    dplyr::mutate(
      velocity = sqrt((X - dplyr::lag(X))^2 + (Y - dplyr::lag(Y))^2) / Interval
    )

  # Step 4: PCDI
  rlang::inform("Step 4: PCDI - Flow Delay Optimization...")
  pcdi_result <- apply_pcdi(data, params$delay_range, params$n_iterations, params$noise_level)
  rlang::inform(paste("  Optimal delay:", pcdi_result$optimal_delay, "seconds"))

  # Step 5: Auto thresholds
  rlang::inform("Step 5: Calculating auto thresholds...")
  thresholds <- calculate_auto_thresholds(
    data,
    yllim = 0.05, yulim = 0.95, yscale = params$yscale,
    vllim = 0.02, vulim = 0.98, vscale = params$v_scale,
    minv_abs = params$minv_abs, miny_abs = params$miny_abs,
    gbuffer = 100
  )
  rlang::inform(paste("  Yield range:", round(thresholds$min_yield, 1), "-", round(thresholds$max_yield, 1)))

  # Step 6: Velocity filter
  rlang::inform("Step 6: Velocity filter...")
  data <- filter_velocity(data, thresholds$min_velocity, thresholds$max_velocity)
  rlang::inform(paste("  Rows:", nrow(data)))

  # Step 7: Flow delay correction
  if (pcdi_result$optimal_delay > 0) {
    rlang::inform(paste("Step 7: Flow delay correction (", pcdi_result$optimal_delay, "s)..."))
    data <- apply_flow_delay(data, delay = pcdi_result$optimal_delay)
    rlang::inform(paste("  Rows:", nrow(data)))
  }

  # Step 8: Yield range filter
  rlang::inform("Step 8: Yield range filter...")
  data <- filter_yield_range(data, thresholds$min_yield, thresholds$max_yield)
  rlang::inform(paste("  Rows:", nrow(data)))

  # Step 9: Moisture filter (auto-detection based on std dev)
  rlang::inform("Step 9: Moisture filter...")
  data <- filter_moisture_range(data, n_std = params$n_std)
  rlang::inform(paste("  Rows:", nrow(data)))

  # Step 10: Overlap filter
  rlang::inform("Step 10: Overlap filter...")
  data <- apply_overlap_filter(data, params$cellsize_overlap, params$overlap_threshold)
  rlang::inform(paste("  Rows:", nrow(data)))

  # Step 11: Local SD filter
  rlang::inform("Step 11: Local SD filter...")
  data <- apply_local_sd_filter(data, params$n_swaths, params$lsd_limit)
  rlang::inform(paste("  Rows:", nrow(data)))

  # Step 12: Convert to yield (bushels/acre)
  rlang::inform("Step 12: Converting to Bushels/acre...")
  data <- convert_flow_to_yield(data, lbs_per_bushel = params$lbs_per_bushel)

  rlang::inform(paste("  Flow range:", round(min(data$Flow), 1), "-", round(max(data$Flow), 1), "bu/acre"))
  rlang::inform(paste("  Flow mean:", round(mean(data$Flow), 1), "bu/acre"))

  # Step 13: Format output
  rlang::inform("Step 13: Formatting output...")
  output_cols <- c("X", "Y", "Latitude", "Longitude", "Flow", "Moisture",
                   "Swath", "Pass", "GPS_Time", "HeaderStatus", "Altitude", "velocity")

  data_output <- data |>
    dplyr::select(dplyr::any_of(output_cols)) |>
    dplyr::mutate(
      HeaderStatus = as.integer(HeaderStatus),
      velocity = NULL  # Remove velocity column
    ) |>
    dplyr::arrange(GPS_Time)

  # Step 14: Export
  if (!is.null(output_file)) {
    rlang::inform("Step 14: Exporting...")
    export_cleaned_data(data_output, output_file, format = "csv")
  }

  # Step 15: Log
  if (!is.null(log_file)) {
    rlang::inform("Step 15: Generating log...")
    generate_yield_log(data_output, data_raw, params, pcdi_result, thresholds, log_file)
  }

  rlang::inform("")
  rlang::inform("================================================")
  rlang::inform(paste("Complete:", nrow(data_output), "rows at",
                      round(mean(data_output$Flow), 1), "bu/acre"))
  rlang::inform("================================================")

  return(data_output)
}


#' Generate log for yield conversion
#' @noRd
generate_yield_log <- function(data_clean, data_raw, params, pcdi_result,
                               thresholds, log_file) {

  n_raw <- nrow(data_raw)
  n_clean <- nrow(data_clean)

  # Convert raw flow for comparison
  data_raw_vel <- data_raw |>
    dplyr::mutate(
      velocity = sqrt((X - dplyr::lag(X))^2 + (Y - dplyr::lag(Y))^2) / Interval
    ) |>
    convert_flow_to_yield(lbs_per_bushel = params$lbs_per_bushel)

  stats_raw <- data.frame(
    mean = mean(data_raw_vel$Flow, na.rm = TRUE),
    sd = sd(data_raw_vel$Flow, na.rm = TRUE),
    cv = sd(data_raw_vel$Flow, na.rm = TRUE) / mean(data_raw_vel$Flow, na.rm = TRUE) * 100,
    n = n_raw
  )

  stats_clean <- data.frame(
    mean = mean(data_clean$Flow, na.rm = TRUE),
    sd = sd(data_clean$Flow, na.rm = TRUE),
    cv = sd(data_clean$Flow, na.rm = TRUE) / mean(data_clean$Flow, na.rm = TRUE) * 100,
    n = n_clean
  )

  log_lines <- c(
    "=======================================================",
    "    AYCE with Yield Conversion - Log File             ",
    "=======================================================",
    paste0("Date: ", Sys.time()),
    "",
    "--- SUMMARY ---",
    paste0("Original points: ", n_raw),
    paste0("Cleaned points: ", n_clean),
    paste0("Removed: ", n_raw - n_clean, " (", round((n_raw - n_clean)/n_raw*100, 1), "%)"),
    "",
    "--- CONVERSION ---",
    paste0("Formula: Flow_buacre = Flow_lbssec × 43560 / (56 × Swath_ft × Velocity)"),
    paste0("Conversion factor: 1 bushel = 56 lbs (corn)"),
    "",
    "--- PCDI ---",
    paste0("Optimal delay: ", pcdi_result$optimal_delay, " seconds"),
    paste0("RSC: ", round(pcdi_result$rsc_values$mean_rsc[pcdi_result$rsc_values$delay == pcdi_result$optimal_delay], 4)),
    "",
    "--- STATISTICS (Bushels/acre) ---",
    paste0("Raw:    Mean=", round(stats_raw$mean, 1),
           " SD=", round(stats_raw$sd, 1),
           " CV=", round(stats_raw$cv, 1), "% N=", stats_raw$n),
    paste0("Clean:  Mean=", round(stats_clean$mean, 1),
           " SD=", round(stats_clean$sd, 1),
           " CV=", round(stats_clean$cv, 1), "% N=", stats_clean$n),
    "",
    "======================================================="
  )

  writeLines(log_lines, log_file)
}
