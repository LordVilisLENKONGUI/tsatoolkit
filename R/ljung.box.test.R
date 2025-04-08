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

  # Function to calculate autocorrelation
  autocorrelation <- function(x, lag=10) {
    n <- length(x)
    if (lag >= n) {
      stop("Lag must be less than the length of the series")
    }

    # Center the series
    x_mean <- base::mean(x)
    x_centered <- x - x_mean

    # Calculate autocorrelation
    numerator <- base::sum(x_centered[(lag+1):n] * x_centered[1:(n-lag)])
    denominator <- base::sum(x_centered^2)

    return(numerator / denominator)
  }

  n <- length(x)
  if (lag >= n) {
    stop("lag must be less than the length of the series")
  }

  # Create vectors to store Q statistics and p-values
  Q_stats <- numeric(lag)
  p_values <- numeric(lag)

  # Calculate statistics for each lag from 1 to lag
  for (i in 1:lag) {
    # Calculate autocorrelations up to lag i
    rho_squares <- numeric(i)
    for (j in 1:i) {
      rho <- autocorrelation(x, j)
      rho_squares[j] <- (rho^2) / (n-j)
    }

    # Calculate Q statistic for lag i
    Q_stats[i] <- base::round(n * (n + 2) * base::sum(rho_squares), round)

    # Calculate p-value
    p_values[i] <- base::round(1 - stats::pchisq(Q_stats[i], df = i), round)
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
