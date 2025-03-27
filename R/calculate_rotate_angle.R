#' Adjust Angles Based on Specified Ranges
#'
#' This function adjusts angles by subtracting 270 or 90 degrees depending on whether
#' they fall within specified ranges, then stores the result in a new column.
#'
#' @param df A data frame containing angle values.
#' @param angle_col Character string specifying the column name of angles in `df`.
#' @param range_list List of numeric vectors defining angle ranges (e.g., `list(0:90, 180:270)`).
#'    Angles within these ranges will trigger the `-270` adjustment; others use `-90`.
#' @param new_col Character string for the output column name. Defaults to `alb_{angle_col}_ert`.
#'
#' @return A data frame with a new column containing adjusted angles (normalized to 0-360 degrees).
#'
#' @examples
#' \donttest{
#' # Create sample data
#' df <- data.frame(angle = c(45, 135, 225, 315))
#'
#' # Apply angle adjustment
#' result <- rotate_angle(
#'   df = df,
#'   angle_col = "angle",
#'   range_list = list(0:90, 180:270)
#' )
#'
#' print(result)
#' }
#'
#' @export
calculate_rotate_angle <- function(df, angle_col, range_list, new_col = paste0('alb_', angle_col, '_ert')) {
  # ---- Parameter Validation ----
  # Check if angle_col exists in df
  if (!angle_col %in% names(df)) {
    stop("Column '", angle_col, "' not found in data frame")
  }

  # Check if range_list is properly formatted
  if (!is.list(range_list) || any(!sapply(range_list, is.numeric))) {
    stop("range_list must be a list of numeric vectors")
  }

  # ---- Core Logic ----
  # Normalize angles to 0-360 degrees
  angle_normalized <- df[[angle_col]] %% 360

  # Generate dynamic conditions for range matching
  condition <- Reduce(
    "|",
    lapply(range_list, function(rng) {
      angle_normalized >= min(rng) & angle_normalized <= max(rng)
    })
  )

  # Apply angle adjustment
  adjusted_angle <- ifelse(
    condition,
    angle_normalized - 270,
    angle_normalized - 90
  )

  # Store result with normalization
  df[[new_col]] <- adjusted_angle %% 360

  return(df)
}
