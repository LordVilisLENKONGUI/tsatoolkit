#' Rescale a Variable to Match the Range of a Reference Variable
#'
#' Rescales a secondary numeric vector to match the range (minimum to maximum) of a reference
#' numeric vector using min-max scaling. This is useful for plotting two variables with different
#' scales on the same axis in a single figure, such as in economic or financial data analysis.
#'
#' @param var_to_scale Numeric vector to be rescaled.
#' @param ref_var Numeric vector whose range (min, max) defines the target range for scaling.
#' @return A numeric vector of the same length as \code{var_to_scale}, rescaled to the range of
#'   \code{ref_var}. Positions where either \code{var_to_scale} or \code{ref_var} is \code{NA}
#'   return \code{NA}.
#'
#' @export
#'
rescale.to.Ref <- function(var_to_scale, ref_var) {
  # Input validation
  if (!is.numeric(var_to_scale) || !is.numeric(ref_var)) {
    stop("Both 'var_to_scale' and 'ref_var' must be numeric vectors.")
  }
  if (length(var_to_scale) != length(ref_var)) {
    stop("'var_to_scale' and 'ref_var' must have the same length.")
  }
  if (all(is.na(var_to_scale)) || all(is.na(ref_var))) {
    stop("One or both input vectors contain only NA values.")
  }

  # Remove NA values for calculations
  valid_idx <- !is.na(var_to_scale) & !is.na(ref_var)
  var_clean <- var_to_scale[valid_idx]
  ref_clean <- ref_var[valid_idx]

  # Initialize output vector with NAs
  scaled_var <- rep(NA, length(var_to_scale))

  # Check for valid data
  if (length(var_clean) == 0) {
    warning("No valid (non-NA) data pairs available for scaling.")
    return(scaled_var)
  }

  # Compute min and max
  max_ref <- max(ref_clean)
  min_ref <- min(ref_clean)
  max_var <- max(var_clean)
  min_var <- min(var_clean)

  # Handle edge cases
  if (max_ref == min_ref) {
    warning("Reference variable has no range (max = min). Returning centered values.")
    scaled_var[valid_idx] <- (max_ref + min_ref) / 2
    return(scaled_var)
  }
  if (max_var == min_var) {
    warning("Variable to scale has no range (max = min). Returning centered reference values.")
    scaled_var[valid_idx] <- (max_ref + min_ref) / 2
    return(scaled_var)
  }

  # Perform min-max scaling
  scaled_var[valid_idx] <- min_ref + (var_clean - min_var) * (max_ref - min_ref) / (max_var - min_var)

  return(scaled_var)

}
