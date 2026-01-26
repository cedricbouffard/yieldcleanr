#' Clean Yield Data with Deletion Tracking
#'
#' Similar to clean_yield() but tracks which points are removed at each filtering step
#' and the reason for deletion. Useful for visualization and debugging.
#'
#' @inheritParams clean_yield
#' @return A list containing:
#'   - data_clean: Cleaned data as tibble or SF object
#'   - deletions: Data frame of deleted points with reasons
#'   - stats: Summary statistics
#' @export
#' @examples
#' \dontrun{
#' result <- clean_yield_with_tracking("data.txt", metrique = TRUE, polygon = TRUE)
#' print(result$stats)
#' head(result$deletions)
#' }
clean_yield_with_tracking <- function(file_path, metrique = TRUE, polygon = TRUE,
                                    params = NULL) {
  
  data_raw <- read_yield_data(file_path)
  rlang::inform("================================================")
  rlang::inform("   Yield Data Cleaning with Tracking       ")
  rlang::inform("================================================")
  rlang::inform(paste("  -", nrow(data_raw), "raw observations loaded"))
  
  # Initialize deletion tracking
  deletions <- data.frame(
    orig_row_id = integer(0),
    step = character(0),
    reason = character(0),
    stringsAsFactors = FALSE
  )
  
  data <- data_raw |> dplyr::mutate(orig_row_id = dplyr::row_number())
  
  # Step 2: UTM conversion
  rlang::inform("Step 2: Converting to UTM coordinates...")
  data <- latlon_to_utm(data)
  
  # Step 3: PCDI
  rlang::inform("Step 3: PCDI - Flow Delay Optimization...")
  pcdi_result <- apply_pcdi(data,
    delay_range = params$delay_range %||% -25:10,
    n_iterations = params$n_iterations %||% 5,
    noise_level = params$noise_level %||% 0.05
  )
  flow_delay <- pcdi_result$optimal_delay
  rlang::inform(paste("  Optimal delay:", flow_delay, "seconds"))
  
  # Step 3b: Calculate initial yield
  data <- convert_flow_to_yield(data)
  
  # Step 4: Auto thresholds
  rlang::inform("Step 4: Calculating auto thresholds...")
  thresholds <- calculate_auto_thresholds(data,
    yllim = params$yllim %||% 0.10,
    yulim = params$yulim %||% 0.90,
    yscale = params$yscale %||% 1.1,
    vllim = params$v_lim %||% 0.05,
    vulim = params$v_ulim %||% 0.95,
    vscale = params$v_scale %||% 1.1,
    minv_abs = params$minv_abs %||% 0.5,
    miny_abs = params$miny_abs %||% 0,
    gbuffer = params$gbuffer %||% 100
  )
  
  # Step 5: Header filter
  rlang::inform("Step 5: Header filter...")
  to_keep <- dplyr::filter(data, HeaderStatus %in% c(1, 33) | is.na(HeaderStatus))
  new_deletions <- dplyr::setdiff(data, to_keep) |> dplyr::mutate(step = "Header Filter", reason = "Invalid HeaderStatus")
  deletions <- dplyr::bind_rows(deletions, new_deletions |> dplyr::select(orig_row_id, step, reason))
  data <- to_keep
  rlang::inform(paste("  Removed:", nrow(new_deletions), "points"))
  
  # Step 6: GPS filter
  rlang::inform("Step 6: GPS filter...")
  to_keep <- dplyr::filter(data, is.na(GPSStatus) | GPSStatus >= 4)
  new_deletions <- dplyr::setdiff(data, to_keep) |> dplyr::mutate(step = "GPS Filter", reason = "GPSStatus < 4")
  deletions <- dplyr::bind_rows(deletions, new_deletions |> dplyr::select(orig_row_id, step, reason))
  data <- to_keep
  rlang::inform(paste("  Removed:", nrow(new_deletions), "points"))
  
  # Step 7: Calculate velocity
  rlang::inform("Step 7: Calculating velocity...")
  data <- data |>
    dplyr::mutate(
      velocity = sqrt((X - dplyr::lag(X))^2 + (Y - dplyr::lag(Y))^2) /
                    dplyr::coalesce(Interval, 1)
    )
  data$velocity[is.na(data$velocity) | !is.finite(data$velocity)] <- 0
  
  # Step 8: Velocity filter
  rlang::inform("Step 8: Velocity filter...")
  to_keep <- dplyr::filter(data, velocity >= thresholds$min_velocity & velocity <= thresholds$max_velocity)
  new_deletions <- dplyr::setdiff(data, to_keep) |> 
    dplyr::mutate(
      step = "Velocity Filter", 
      reason = paste0("Velocity outside range [", round(thresholds$min_velocity, 2), ", ", round(thresholds$max_velocity, 2), "] m/s")
    )
  deletions <- dplyr::bind_rows(deletions, new_deletions |> dplyr::select(orig_row_id, step, reason))
  data <- to_keep
  rlang::inform(paste("  Removed:", nrow(new_deletions), "points"))
  
  # Step 9: Flow delay correction
  rlang::inform(paste("Step 9: Flow delay correction (", flow_delay, "s)..."))
  data <- apply_flow_delay(data, delay = -flow_delay)
  
  # Step 9b: Calculate yield from delayed flow
  data <- convert_flow_to_yield(data)
  
  # Step 9c: Recalculate thresholds
  thresholds <- calculate_auto_thresholds(data,
    yllim = params$yllim %||% 0.10,
    yulim = params$yulim %||% 0.90,
    yscale = params$yscale %||% 1.1,
    vllim = params$v_lim %||% 0.05,
    vulim = params$v_ulim %||% 0.95,
    vscale = params$v_scale %||% 1.1,
    minv_abs = params$minv_abs %||% 0.5,
    miny_abs = params$miny_abs %||% 0,
    gbuffer = params$gbuffer %||% 100
  )
  
  # Step 9e: Remove boundary points
  rlang::inform("Step 9e: Removing boundary points...")
  if ("Pass" %in% names(data) && "Interval" %in% names(data) && !all(is.na(data$Pass))) {
    n_before <- nrow(data)
    abs_delay <- abs(flow_delay)
    
    data <- data |>
      dplyr::arrange(GPS_Time) |>
      dplyr::group_by(Pass) |>
      dplyr::mutate(
        interval_cumsum = cumsum(Interval),
        total_interval = sum(Interval, na.rm = TRUE),
        interval_from_end = total_interval - interval_cumsum
      ) |>
      dplyr::ungroup()
    
    if (flow_delay < 0) {
      to_keep <- dplyr::filter(data, interval_from_end > abs_delay | is.na(interval_from_end))
    } else {
      to_keep <- dplyr::filter(data, interval_cumsum > abs_delay | is.na(interval_cumsum))
    }
    
    new_deletions <- dplyr::setdiff(data, to_keep) |> 
      dplyr::mutate(step = "Boundary Removal", reason = paste0("Flow delay boundary (", flow_delay, "s)"))
    deletions <- dplyr::bind_rows(deletions, new_deletions |> dplyr::select(orig_row_id, step, reason))
    data <- to_keep
    rlang::inform(paste("  Removed:", nrow(new_deletions), "points"))
  }
  
  # Step 10: Remove zero yield
  rlang::inform("Step 10: Removing zero yield points...")
  to_keep <- dplyr::filter(data, Yield_buacre > 0)
  new_deletions <- dplyr::setdiff(data, to_keep) |> dplyr::mutate(step = "Zero Yield", reason = "Yield = 0")
  deletions <- dplyr::bind_rows(deletions, new_deletions |> dplyr::select(orig_row_id, step, reason))
  data <- to_keep
  rlang::inform(paste("  Removed:", nrow(new_deletions), "points"))
  
  # Step 11: Yield range filter
  rlang::inform("Step 11: Yield range filter...")
  to_keep <- dplyr::filter(data, 
    Yield_buacre >= thresholds$min_yield & 
    Yield_buacre <= thresholds$max_yield)
  new_deletions <- dplyr::setdiff(data, to_keep) |> 
    dplyr::mutate(
      step = "Yield Range Filter", 
      reason = paste0("Yield outside range [", round(thresholds$min_yield, 2), ", ", round(thresholds$max_yield, 2), "] bu/acre")
    )
  deletions <- dplyr::bind_rows(deletions, new_deletions |> dplyr::select(orig_row_id, step, reason))
  data <- to_keep
  rlang::inform(paste("  Removed:", nrow(new_deletions), "points"))
  
  # Step 12: Moisture filter
  rlang::inform("Step 12: Moisture filter (auto-detection)...")
  moisture_mean <- mean(data$Moisture, na.rm = TRUE)
  moisture_sd <- stats::sd(data$Moisture, na.rm = TRUE)
  n_std <- params$n_std %||% 3
  moisture_min <- moisture_mean - n_std * moisture_sd
  moisture_max <- moisture_mean + n_std * moisture_sd
  
  to_keep <- dplyr::filter(data, 
    is.na(Moisture) | (Moisture >= moisture_min & Moisture <= moisture_max))
  new_deletions <- dplyr::setdiff(data, to_keep) |> 
    dplyr::mutate(
      step = "Moisture Filter", 
      reason = paste0("Moisture outside range [", round(moisture_min, 2), ", ", round(moisture_max, 2), "]")
    )
   deletions <- dplyr::bind_rows(deletions, new_deletions |> dplyr::select(orig_row_id, step, reason))
   data <- to_keep
   rlang::inform(paste("  Removed:", nrow(new_deletions), "points"))
   
   # Step 12b: Heading smoothing for better polygon geometry
   rlang::inform("Step 12b: Smoothing heading for polygon geometry...")
   data <- data |> dplyr::arrange(GPS_Time)
   n <- nrow(data)
   
    if (all(c("X", "Y") %in% names(data))) {
      data <- data |>
        dplyr::mutate(
          heading = atan2(
            dplyr::lag(X, default = X[1]) - X,
            dplyr::lag(Y, default = Y[1]) - Y
          ) * 180 / pi
        )
    } else {
      data <- data |>
        dplyr::mutate(
          heading = atan2(
            dplyr::lag(Longitude, default = Longitude[1]) - Longitude,
            dplyr::lag(Latitude, default = Latitude[1]) - Latitude
          ) * 180 / pi
        )
    }
   data$heading[is.na(data$heading)] <- 0
   
   heading_diff <- c(data$heading[2:n] - data$heading[1:(n-1)], 0)
   heading_diff <- ((heading_diff + 180) %% 360) - 180
   turn_threshold <- 30
   is_turn <- abs(heading_diff) > turn_threshold
   segment_id <- cumsum(c(TRUE, is_turn[1:(n-1)]))
   data$segment_id <- segment_id
   
   data <- data |>
     dplyr::group_by(segment_id) |>
     dplyr::mutate(
       row_in_segment = dplyr::row_number(),
       n_in_segment = dplyr::n(),
       heading_var = {
         angles_rad <- heading * pi / 180
         sin_mean <- mean(sin(angles_rad), na.rm = TRUE)
         cos_mean <- mean(cos(angles_rad), na.rm = TRUE)
         R <- sqrt(sin_mean^2 + cos_mean^2)
         ifelse(R < 0.001, 0, sqrt(-2 * log(R)) * 180 / pi)
       },
       heading_smooth = dplyr::case_when(
         row_in_segment == 1 | row_in_segment == n_in_segment ~ heading,
         heading_var > 15 ~ {
           zoo::rollapply(
             heading,
             width = 3,
             align = "center",
             partial = TRUE,
             FUN = function(x) {
               angles_rad <- x * pi / 180
               atan2(mean(sin(angles_rad), na.rm = TRUE),
                     mean(cos(angles_rad), na.rm = TRUE)) * 180 / pi
             }
           )
         },
         n_in_segment >= 3 ~ {
           angles_rad <- heading * pi / 180
           atan2(mean(sin(angles_rad), na.rm = TRUE),
                 mean(cos(angles_rad), na.rm = TRUE)) * 180 / pi
         },
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
   data$heading_smooth <- NULL
   
   rlang::inform(paste("  Heading smoothed for", nrow(data), "points"))
   
   # Step 13: Overlap filter
  rlang::inform("Step 13: Bitmap Overlap Filter...")
  n_before <- nrow(data)
  current_orig_ids <- data$orig_row_id
  data <- apply_overlap_filter(data,
    cellsize = params$cellsize_overlap %||% 0.3,
    overlap_threshold = params$overlap_threshold %||% 0.5
  )
  n_removed <- n_before - nrow(data)
  if (n_removed > 0) {
    kept_ids <- data$orig_row_id
    removed_ids <- setdiff(current_orig_ids, kept_ids)
    if (length(removed_ids) > 0) {
      new_deletions <- tibble::tibble(
        orig_row_id = head(removed_ids, min(length(removed_ids), 1000)),
        step = "Overlap Filter",
        reason = "Excessive overlap"
      )
      deletions <- dplyr::bind_rows(deletions, new_deletions)
    }
  }
  rlang::inform(paste("  Removed:", n_removed, "points"))

  # Step 13b: Remove overlapping polygons early (after bitmap)
  if (polygon && nrow(data) > 100 && "Pass" %in% names(data)) {
    rlang::inform("Step 13b: Removing overlapping polygons (early)...")
    n_before_poly <- nrow(data)

    tryCatch({
      sf_use_s2(FALSE)

      sf_result <- data_to_sf(data, crs = 4326)

      median_lon <- stats::median(sf_result$Longitude, na.rm = TRUE)
      median_lat <- stats::median(sf_result$Latitude, na.rm = TRUE)
      utm_zone <- floor((median_lon + 180) / 6) + 1
      utm_epsg <- ifelse(median_lat < 0, 32700 + utm_zone, 32600 + utm_zone)
      utm_crs <- sf::st_crs(paste0("EPSG:", utm_epsg))
      sf_proj <- sf::st_transform(sf_result, utm_crs)

      # Calculate polygon areas in projected CRS
      sf_proj$poly_area <- as.numeric(sf::st_area(sf_proj))

      # Get unique passes sorted
      passes <- sort(unique(sf_proj$Pass))
      pass_window <- params$overlap_pass_window %||% 2
      max_candidates <- params$overlap_max_candidates %||% 150
      overlap_area_threshold <- params$overlap_area_threshold %||% 0.08

      # Track rows to remove
      rows_to_remove <- integer(0)

      for (p in passes[-1]) {
        current_mask <- sf_proj$Pass == p
        previous_mask <- sf_proj$Pass < p & sf_proj$Pass >= (p - pass_window)

        if (!any(current_mask) || !any(previous_mask)) next

        current_polys <- sf_proj[current_mask, ]
        previous_polys <- sf_proj[previous_mask, ]

        if (nrow(current_polys) == 0 || nrow(previous_polys) == 0) next

        # Find previous polygons within 10m
        nearby_idx <- sf::st_is_within_distance(
          current_polys,
          previous_polys,
          dist = 10,
          sparse = TRUE
        )

        if (length(nearby_idx) == 0) next

        current_geom <- sf::st_geometry(current_polys)
        previous_geom <- sf::st_geometry(previous_polys)

        for (i in seq_len(nrow(current_polys))) {
          idx <- nearby_idx[[i]]
          if (length(idx) == 0) next
          if (length(idx) > max_candidates) {
            idx <- idx[seq_len(max_candidates)]
          }

          cur_area <- current_polys$poly_area[i]
          if (is.na(cur_area) || cur_area == 0) next

          inter <- tryCatch({
            sf::st_intersection(previous_geom[idx], current_geom[i])
          }, error = function(e) NULL)

          if (is.null(inter) || length(inter) == 0 || inherits(inter, "sfc_EMPTY")) next

          total_inter <- sum(as.numeric(sf::st_area(inter)))

          if (total_inter > overlap_area_threshold * cur_area) {
            rows_to_remove <- c(rows_to_remove, current_polys$orig_row_id[i])
          }
        }
      }

      # Remove overlapping polygons from data
      if (length(rows_to_remove) > 0) {
        data <- data[!(data$orig_row_id %in% rows_to_remove), ]
      }

      n_removed_poly <- n_before_poly - nrow(data)
      if (n_removed_poly > 0) {
        rlang::inform(paste("  Removed", n_removed_poly, "overlapping polygons"))
      } else {
        rlang::inform("  No significant overlaps found")
      }

      sf_use_s2(FALSE)

    }, error = function(e) {
      rlang::warn(paste("Could not remove overlaps:", e$message))
      sf_use_s2(FALSE)
    })
  }

  # Step 14: Local SD filter
  rlang::inform("Step 14: Localized SD Filter...")
  n_before <- nrow(data)
  current_orig_ids <- data$orig_row_id
  data <- apply_local_sd_filter(data,
    n_swaths = params$n_swaths %||% 5,
    lsd_limit = params$lsd_limit %||% 2.4,
    min_cells = params$min_cells %||% 3
  )
  n_removed <- n_before - nrow(data)
  if (n_removed > 0) {
    kept_ids <- data$orig_row_id
    removed_ids <- setdiff(current_orig_ids, kept_ids)
    if (length(removed_ids) > 0) {
      new_deletions <- tibble::tibble(
        orig_row_id = head(removed_ids, min(length(removed_ids), 1000)),
        step = "Local SD Filter",
        reason = "High local variation"
      )
      deletions <- dplyr::bind_rows(deletions, new_deletions)
    }
  }
  rlang::inform(paste("  Removed:", n_removed, "points"))
  
  # Step 15: Pass-to-pass filter
  rlang::inform("Step 15: Pass-to-Pass Filter...")
  if ("Pass" %in% names(data) && !all(is.na(data$Pass))) {
    pass_stats <- data |>
      sf::st_drop_geometry() |>
      dplyr::group_by(Pass) |>
      dplyr::summarise(
        mean_yield = mean(Yield_buacre, na.rm = TRUE),
        median_yield = median(Yield_buacre, na.rm = TRUE),
        n_points = dplyr::n(),
        .groups = "drop"
      )
    
    overall_median <- median(pass_stats$median_yield, na.rm = TRUE)
    pass_sd <- sd(pass_stats$median_yield, na.rm = TRUE)
    pass_threshold <- params$pass_threshold %||% 2.5
    
    abnormal_passes <- pass_stats |>
      dplyr::filter(abs(median_yield - overall_median) > pass_threshold * pass_sd) |>
      dplyr::pull(Pass)
    
    if (length(abnormal_passes) > 0) {
      n_before_pass <- nrow(data)
      data <- data |> dplyr::filter(!(Pass %in% abnormal_passes))
      n_removed_pass <- n_before_pass - nrow(data)
      
      if (n_removed_pass > 0) {
        removed_ids <- setdiff(current_orig_ids, data$orig_row_id)
        new_deletions <- tibble::tibble(
          orig_row_id = head(removed_ids, min(length(removed_ids), n_removed_pass)),
          step = "Pass Filter",
          reason = paste0("Abnormal pass yield (median: ", 
                         round(pass_stats$median_yield[pass_stats$Pass %in% abnormal_passes][1], 1), 
                         " vs overall: ", round(overall_median, 1), ")")
        )
        deletions <- dplyr::bind_rows(deletions, new_deletions)
      }
      rlang::inform(paste("  Removed:", n_removed_pass, "points from", length(abnormal_passes), "abnormal passes"))
    } else {
      rlang::inform("  No abnormal passes detected")
    }
  } else {
    rlang::inform("  Skipping - no Pass column")
  }
  
  # Prepare final output
  data$orig_row_id <- NULL
  
  if (metrique) {
    data$Yield_kg_ha <- data$Yield_buacre * 67.25
    data$Flow_kg_s <- data$Flow * 0.453592
    yield_col <- "Yield_kg_ha"
  } else {
    yield_col <- "Yield_buacre"
  }
  
    if (polygon) {
      # Create SF output
      if (!"Swath_m" %in% names(data)) {
        data$Swath_m <- data$Swath * 0.0254
      }
      if (!"Distance_m" %in% names(data)) {
        data$Distance_m <- data$Distance * 0.0254
      }
      if (!"Altitude_m" %in% names(data)) {
        data$Altitude_m <- data$Altitude * 0.3048
      }
      
      sf_result <- data_to_sf(data, crs = 4326)
      
      # Overlap removal already applied after bitmap filter
      
      data_clean <- sf_result
    } else {
     data_clean <- data
   }
  
  # Stats
  stats <- list(
    n_raw = nrow(data_raw),
    n_clean = nrow(data_clean),
    n_deleted = nrow(data_raw) - nrow(data_clean),
    retention_rate = nrow(data_clean) / nrow(data_raw),
    flow_delay = flow_delay,
    thresholds = thresholds,
    deletions_by_step = deletions |>
      dplyr::group_by(step) |>
      dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
      dplyr::arrange(dplyr::desc(n))
  )
  
  rlang::inform("")
  rlang::inform("================================================")
  rlang::inform(paste("Complete:", nrow(data_clean), "observations cleaned"))
  rlang::inform(paste("Retention rate:", round(stats$retention_rate * 100, 1), "%"))
  rlang::inform("================================================")
  
  list(
    data_clean = data_clean,
    data_raw = data_raw |> dplyr::mutate(orig_row_id = dplyr::row_number()),
    deletions = deletions,
    stats = stats
  )
}
