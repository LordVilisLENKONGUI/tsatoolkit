#' Hamilton Filter for Trend-Cycle Decomposition
#' Decomposes a time series into trend and cycle components using the
#' regression-based approach proposed by Hamilton (2018), as an alternative
#' to the Hodrick-Prescott filter.
#'
#' @param y Numeric vector of the time series.
#' @param h Forecast horizon (default: 8 for quarterly data, i.e. 2 years ahead).
#' @param p Number of lags (default: 4).
#'
#' @return A list containing:
#' \describe{
#' \item{y}{Original series}
#' \item{trend}{Estimated trend component (NA for first h+p-1 observations)}
#' \item{cycle}{Estimated cycle component (residuals)}
#' }
#' @references
#' Hamilton, J. D. (2018). Why You Should Never Use the Hodrick-Prescott Filter.
#' \emph{Review of Economics and Statistics}, 100(5), 831-843.
#' @examples
#' data(JPNGDP)
#' result <- Hamilton.filter(JPNGDP$RGDP, h = 8, p = 4)
#' plot.ts(result$cycle, main = "Japan GDP Cycle")
#'
#' @importFrom stats embed
#' @export
#'
Hamilton.filter <- function(y, h = 8, p = 4) {
  n <- length(y)

  # Lag matrix
  X <- embed(y, p)

  # Align X and Y
  X <- X[base::seq_len(NROW(X) - h), ]
  X <- cbind(1, X)  # intercept
  Y <- y[-c(1:(h + p - 1))]

  # Regression
  fit <-stats::lm.fit(X, Y)

  # Results
  trend <- rep(NA, n)
  cycle <- rep(NA, n)

  trend[(h + p):n] <- fit$fitted.values
  cycle[(h + p):n] <- fit$residuals
  list(y = y,
       trend = trend,
       cycle = cycle)
}
