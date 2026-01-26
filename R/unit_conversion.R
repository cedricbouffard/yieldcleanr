#' Convert yield flow units
#'
#' Converts raw grain flow from LBS/Sec to Bushels/Sec assuming
#' a standard density of 56 lbs/bushel for corn.
#'
#' @param data Tibble with Flow column
#' @param density LBS per bushel (default 56 for corn)
#' @param scale_factor Optional scaling factor (e.g., 1000 for display)
#' @return Data with Flow converted
#' @noRd
#' @examples
#' \dontrun{
#' data <- convert_flow_units(data, density = 56, scale_factor = 100)
#' }
convert_flow_units <- function(data, density = 56, scale_factor = 1) {
  if (!"Flow" %in% names(data)) {
    rlang::warn("Colonne Flow non trouvée")
    return(data)
  }

  data <- data |>
    dplyr::mutate(
      Flow = Flow / density * scale_factor
    )

  rlang::inform(paste("Flow converted:", density, "lbs/bu ×", scale_factor, "scale"))

  return(data)
}


#' Apply yield conversion for display
#'
#' Converts raw flow to yield in bushels per acre equivalent
#' accounting for velocity and swath width.
#'
#' @param data Tibble with Flow, velocity (optional), Swath columns
#' @param density LBS per bushel
#' @return Data with Flow converted to bushels/acre
#' @noRd
convert_to_yield <- function(data, density = 56) {
  if (!"Flow" %in% names(data)) {
    rlang::warn("Colonne Flow non trouvée")
    return(data)
  }

  # Conversion factors
  lbs_per_bu <- density  # 56 for corn
  sqft_per_acre <- 43560
  sqm_per_sqft <- 0.0929
  inch_per_m <- 39.37

  # Convert swath from inches to meters
  if ("Swath" %in% names(data)) {
    swath_m <- data$Swath / inch_per_m

    # Calculate velocity if not present
    if (!"velocity" %in% names(data) && all(c("X", "Y", "Interval") %in% names(data))) {
      data <- data |>
        dplyr::mutate(
          velocity = sqrt((X - dplyr::lag(X))^2 + (Y - dplyr::lag(Y))^2) / Interval
        )
    }

    # Convert to bushels/acre
    # Flow (lbs/sec) / lbs_per_bu = bushels/sec
    # bushels/sec / (velocity * swath_m) = bushels/sec/m²
    # bushels/sec/m² * sqm_per_sqft * sqft_per_acre = bushels/acre/sec
    # Final: Flow * sqm_per_sqft * sqft_per_acre / (lbs_per_bu * velocity * swath_m)

    if ("velocity" %in% names(data)) {
      data <- data |>
        dplyr::mutate(
          Flow_yield = Flow * sqm_per_sqft * sqft_per_acre /
                        (lbs_per_bu * velocity * swath_m)
        ) |>
        dplyr::select(-velocity)
    } else {
      # Simplified conversion
      data <- data |>
        dplyr::mutate(
          Flow_yield = Flow / lbs_per_bu * 100  # Simplified scaling
        )
    }
  } else {
    # Simple conversion
    data <- data |>
      dplyr::mutate(
        Flow_yield = Flow / lbs_per_bu
      )
  }

  return(data)
}
