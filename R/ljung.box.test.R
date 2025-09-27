#' Ljung-Box Test
#'
#' This function compute the Ljung-Box test statistic for examining the null hypothesis of no autocorrelation in a given time series
#'
#' @param x vector of numerical time series
#' @param lag maximum lag
#' @param format Output format for the table: "rst", "pandoc" or "latex". If NULL, defaults to "rst"
#' @param round numeric vector for round, default is set to 3
#'
#' @returns a knitr_kable formatted table with descriptive statistics
#' @export
#'
#' @examples
#'set.seed(1)
#'tsatoolkit::ljung.box.test(rnorm(100))
#'
ljung.box.test <- function(x, lag=10, format = NULL, round=3) {

  n <- length(x)
  if (lag >= n) {
    stop("lag must be less than the length of the series")
  }

  # Create vectors to store Q statistics and p-values
  Q_stats <- numeric(lag)
  p_values <- numeric(lag)

  # Calculate statistics for each lag from 1 to lag using stats::Box.test
  for (i in 1:lag) {
    # Use the built-in Box.test function (which implements Ljung-Box test by default)
    box_result <- stats::Box.test(x, lag = i, type = "Ljung-Box")

    # Extract Q statistic and p-value
    Q_stats[i] <- base::round(box_result$statistic, round)
    p_values[i] <- base::round(box_result$p.value, round)
  }

  # Create results matrix
  result <- cbind(1:lag, Q_stats, p_values)
  colnames(result) <- c("lag", "Q", "p-value")

  if (is.null(format)) {
    result <- kableExtra::kable(result, format = "rst", align = rep("c", ncol(result)),
                                caption = "Ljung-Box test")
  } else{
    #format <- match.arg(format)
    result <- kableExtra::kable(result, format = format,
                                align = rep("c", ncol(result)),
                                booktable = T, booktabs = T,
                                caption = "Ljung-Box test")
  }

  cat(sprintf("Ljung-Box Autocorrelation Test"))
  return(result)
}
