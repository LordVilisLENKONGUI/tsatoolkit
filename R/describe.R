#' Descriptive Statistics Summary
#'
#' Computes and displays descriptive statistics for numeric data in a formatted table.
#'
#' @param object A numeric vector, matrix, or data frame containing numeric data to summarize.
#' @param type A character string specifying the type of summary: \code{"short"} (default) for basic statistics or \code{"long"} to include skewness and kurtosis.
#' @param format Output format for the table: \code{"rst"} (default if NULL), \code{"pandoc"}, or \code{"latex"}.
#' @param round Number of decimal places to display for statistical measures (default is 2).
#'
#' @return A \code{knitr::kable} formatted table containing descriptive statistics.
#' @details
#' The function calculates basic statistics (min, Q1, median, mean, Q3, max, standard deviation,
#' and coefficient of variation) for \code{type = "short"}. For \code{type = "long"}, it also
#' includes skewness and kurtosis. Non-numeric inputs are handled appropriately: character
#' vectors return a base summary, and data frames are filtered to numeric columns only.
#'
#' @examples
#' # Example with a numeric vector
#' data("longley")
#' describe(longley)
#'
#' # Example with missing values
#' describe(c(NA, rnorm(1997)))
#'
#' @importFrom kableExtra kable
#' @importFrom moments skewness kurtosis
#' @export
describe <- function(object, type = c("short", "long"), format = NULL, round = 2) {

  if (any(sapply(c("mts", "ts", "matrix", "array", "tbl_df", "tbl"), function(x) inherits(object, x)))) {
    object <- base::as.matrix(object)
  }

  type <- match.arg(type)

  #------------------------------------------------/
  # Internal function to calculate statistics
  #------------------------------------------------/
  calc_stat <- function(object, type) {
    moyenne <- base::mean(object, na.rm = TRUE)
    stdev <- stats::sd(object, na.rm = TRUE)

    base_stats <- c(
      Minimum = base::min(object, na.rm = TRUE),
      "Q1" = stats::quantile(object, 0.25, na.rm = TRUE),
      Median = stats::median(object, na.rm = TRUE),
      Mean = base::mean(object, na.rm = TRUE),
      "Q3" = stats::quantile(object, 0.75, na.rm = TRUE),
      Maximum = base::max(object, na.rm = TRUE)
    )

    dispersion <- c(
      "Std.Dev" = stats::sd(object, na.rm = TRUE),
      "Coef.Var" = if (moyenne != 0) (stdev / moyenne) * 100 else NA
    )

    long_stats <- if (type == "long") {
      c(
        "Skewness" = tryCatch(
          moments::skewness(stats::na.omit(object)),
          error = function(e) NA
        ),
        "Kurtosis" = tryCatch(
          moments::kurtosis(stats::na.omit(object)),
          error = function(e) NA
        )
      )
    } else {
      NULL
    }

    obs_stats <- if (sum(is.na(object)) != 0) {
      c(
        "Obs" = floor(as.integer(length(object))),
        "NA's" = floor(sum(is.na(object)))
      )
    } else {
      c("Obs" = floor(as.integer(length(object))))
    }

    if (type == "long") {
      stats <- c(obs_stats, " " = NA, base_stats, " " = NA, dispersion, long_stats)
    } else {
      stats <- c(obs_stats, " " = NA, base_stats, " " = NA, dispersion)
    }
    return(stats)
  }

  #------------------------------------------------/
  # Main function processing
  #------------------------------------------------/
  if (is.vector(object)) {
    if (is.character(object)) {
      return(base::summary(object))
    } else {
      result <- calc_stat(object, type)
      result <- as.matrix(result)
      colnames(result) <- deparse(substitute(object))
    }
  } else {
    if (is.data.frame(object)) {
      numeric_cols <- sapply(object, is.numeric)
      if (sum(numeric_cols) == 0) {
        stop("No numeric columns found in the input DataFrame")
      }
      object <- object[, numeric_cols, drop = FALSE]
    } else if (is.matrix(object)) {
      if (!is.numeric(object)) {
        stop("Matrix must be numeric")
      }
    }
    result <- apply(object, 2, calc_stat, type = type)
  }

  #------------------------------------------------/
  # Format and return output
  #------------------------------------------------/
  options(knitr.kable.NA = "") # Hide NA values in output

  # Construction robuste du vecteur digits
  row_names <- rownames(result)
  digits_vector <- ifelse(row_names %in% c("Obs", "NA's", " "), 0, round)

  if (is.null(format)) {
    result <- kableExtra::kable(result, format = "rst", align = rep('c', ncol(result)),
                                digits = digits_vector)
  } else {
    result <- kableExtra::kable(result, format = format, align = rep('c', ncol(result)),
                                booktabs = TRUE, digits = digits_vector)
  }

  return(result)
}
