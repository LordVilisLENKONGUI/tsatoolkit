#' Ljung-Box Test
#'
#' This function computes the Ljung-Box test statistic for examining the null hypothesis of no autocorrelation in a given time series
#'
#' @param x vector of numerical time series
#' @param lags maximum lag
#' @param format Output format for the table: "rst", "pandoc" or "latex". If NULL, defaults to "rst"
#' @param round numeric value for rounding, default is set to 3
#'
#' @returns a knitr_kable formatted table with Ljung-Box test results
#' @export
#'
Ljung.Box.test <- function(x, lags = 10, format = NULL, round = 3) {
  n <- length(x)
  if (lags >= n) {
    stop("lags must be less than the length of the series")
  }

  # Create vectors to store AC, Q statistics and p-values
  Q_stats <- numeric(lags)
  p_values <- numeric(lags)
  ac_values <- numeric(lags)

  # Calculate ACF once for all lags
  box_ac <- stats::acf(x, lag.max = (lags+1), plot = FALSE, type = "correlation")

  # Extract all AC values at once (excluding lag 0)
  box_ac <- as.numeric(box_ac$acf)[-1]  # Remove lag 0

  # Calculate statistics for each lag from 1 to lags
  for (i in 1:lags) {
    # Use the built-in Box.test function (Ljung-Box test)
    box_result <- stats::Box.test(x, lag = i, type = "Ljung-Box")

    # Extract Q statistic and p-value
    Q_stats[i] <- base::round(box_result$statistic, round)
    p_values[i] <- box_result$p.value
    # Use pre-extracted AC values
    ac_values[i] <- base::round(box_ac[i], round)
  }

  formatted_pvalues <- sapply(p_values, function(p) {
    if (p < 0.001) {
      "0.000"
    } else {
      format(round(p, round), nsmall = round)
    }
  })

  # Create results data frame (not matrix) for better control
  result <- data.frame(
    Lag = 1:lags,
    AC = base::as.numeric(base::sprintf("%12.3f", ac_values)),
    Q = Q_stats,
    "p-value" = formatted_pvalues,
    check.names = FALSE
  )


  # Format table
  if (is.null(format)) {
    result <- kableExtra::kable(result, format = "rst",
                                align = c("c", "r", "c", "c"),
                                caption = "Ljung-Box Autocorrelation Test")
  } else {
    result <- kableExtra::kable(result, format = format,
                                align = c("c", "r", "c", "c"),
                                booktabs = TRUE,
                                caption = "Ljung-Box Autocorrelation Test")
  }

  return(result)
}
