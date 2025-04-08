#' Quenouille Test for AR Process Order Identification
#'
#' @description
#' Implements the Quenouille test to identify the order of an autoregressive (AR) process
#' by analyzing the partial autocorrelation function (PACF) coefficients.
#'
#' @param x A numeric vector or time series object containing the data
#' @param lags Integer specifying the maximum number of lags to test. If NULL (default),
#'             it is set to floor(10 * log10(n)) where n is the length of the series
#' @param alpha Numeric value between 0 and 1 specifying the significance level (default is 0.05)
#'
#' @importFrom stats pacf
#' @importFrom stats qnorm
#' @importFrom stats residuals
#'
#' @returns A list containing three elements:
#' \itemize{
#'   \item AR.order.p: The suggested order of the AR process
#'   \item Quenouille.limit: The confidence interval threshold
#'   \item pacf.plot: A ggplot2 object showing the PACF with significance bounds
#' }
#'
#' @details
#' The Quenouille test uses asymptotic distribution theory to determine significance bounds
#' for partial autocorrelation coefficients. For a true AR(p) process, PACF values should
#' be zero after lag p. The test identifies the first lag where PACF becomes non-significant.
#'
#' @export
#'
#' @examples
#' set.seed(123)
#' # Generate an AR(4) process
#' ar4 <- arima.sim(n = 200, model = list(ar = c(0.5, -0.3, 0.2, -0.1)), sd = 1)
#' # Apply Quenouille test
#' quenouille.test(ar4)
#'
#' # Custom number of lags
#' quenouille.test(ar4, lags = 20)
#'
#' # Different significance level
#' quenouille.test(ar4, alpha = 0.01)
#'
quenouille.test <- function(x, lags = NULL, alpha = 0.05) {
  # Handle model objects by extracting residuals
  if (inherits(x, c("Arima", "estimate", "dynlm", "lm", "ardl"))) {
    x <- residuals(x)
  }

  # Get length of input series
  n <- length(x)

  # If no lags specified, calculate default using length of series
  if(is.null(lags)) lags <- floor(10 * log10(n))

  # Calculate Quenouille limits (according to theorem)
  # For two-sided test with alpha significance level
  quenouille_limits <- round(qnorm(alpha/2, lower.tail = FALSE), 2)/sqrt(n)

  # Test for significance
  significant <- abs(pacf(x, lag.max = lags, plot = FALSE)$acf) > quenouille_limits

  # Find first non-significant lag
  results <- which(!significant)[1] - 1

  # Display results
  cat(strrep("-----", 8), "\n")
  cat("Quenouille Test\n")
  cat("Confidence interval:", round(quenouille_limits, 4), "\n")

  if (is.na(results) || results == 0) {
    cat("No break corresponding to AR process\n")
    cat(strrep("-----", 8), "\n")
    #return(NULL)
    methods::show( pacf(x,  lag.max = lags, na.action = stats::na.pass) )
  } else {
    cat("Suggested AR process order:", results, "\n")
    cat(strrep("-----", 8), "\n")
    methods::show( pacf(x,  lag.max = lags, na.action = stats::na.pass) )

    # Return results as a list
    invisible(list(
      AR.order.p = results,
      Quenouille.limit = quenouille_limits,
      pacf.plot = pacf(x,  lag.max = lags, na.action = stats::na.pass)
    ))
  }
}
