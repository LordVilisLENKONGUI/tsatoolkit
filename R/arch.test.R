#' ARCH Test for Conditional Heteroskedasticity
#'
#' Performs an ARCH test using the Lagrange Multiplier (LM) approach to detect
#' the presence of Autoregressive Conditional Heteroskedasticity (ARCH) effects
#' in a time series or model residuals.
#'
#' @param x A fitted model object (e.g., \code{lm}, \code{ardl}, \code{dynlm},
#'   \code{arma}, \code{ar}, \code{dynardl}, \code{MSM.lm}) or a numeric vector.
#'   For model objects, residuals are automatically extracted.
#'   For numeric vectors, the series is used directly, for fitted model squared residuals are used
#' @param lags Integer specifying the maximum number of lags to test for ARCH
#'   effects. The function will test all lags from 1 to \code{lag}.
#'   Must be less than the length of the series. Defaults to 10.
#' @param format Character string specifying the output format for the table.
#'   Accepts formats supported by \code{kableExtra::kable()}.
#'   If \code{NULL}, uses "rst" format. Set to specific format for customized output.
#' @param round Integer specifying the number of decimal places to round the
#'   test statistics and p-values. Defaults to 3.
#'
#' @details
#' The ARCH test is implemented using the Lagrange Multiplier (LM) test from
#' \code{FinTS::ArchTest}. This is the standard approach in econometrics for
#' testing ARCH effects. The test statistic follows a chi-squared distribution
#' under the null hypothesis of no ARCH effects.
#'
#' For each lag from 1 to \code{lag}, the function computes:
#' \itemize{
#'   \item The ARCH LM test statistic using auxiliary regression
#'   \item The corresponding p-value using the chi-squared distribution
#' }
#'
#' The null hypothesis is that there are no ARCH effects (homoskedastic errors).
#' The alternative hypothesis is the presence of ARCH effects (heteroskedastic errors).
#'
#' @return A formatted table (using \code{kableExtra::kable}) containing:
#' \itemize{
#'   \item \code{lag}: The lag number (1 to \code{lag})
#'   \item \code{LM}: The ARCH LM test statistic for each lag
#'   \item \code{p-value}: The p-value for each test statistic
#' }
#'
#' @examples
#' \dontrun{
#' # Example with a linear model
#' set.seed(123)
#' data <- data.frame(y = rnorm(100), x = rnorm(100))
#' lm_model <- lm(y ~ x, data = data)
#' ARCH.test(lm_model, lags = 5)
#' }
#'
#' @seealso
#' \code{\link[FinTS]{ArchTest}} for the underlying ARCH LM test implementation.
#'
#' @importFrom FinTS ArchTest
#' @importFrom stats residuals
#' @importFrom kableExtra kable
#' @export
ARCH.test <- function(x, lags=10, format = NULL, round=3) {

  # Extract residuals based on input type
  if (is.numeric(x)) {
    # Store the name for display purposes
    series_name <- deparse(substitute(x))
    # Use the series directly (no squaring needed for FinTS::ArchTest)
    uhat <- x
  } else {
    model.class <- class(x)[1]  # Get class of the model
    # Extract residuals
    uhat <- (switch(model.class,
                        "lm" = residuals(x),
                        "ardl" = residuals(x),
                        "dynlm" = residuals(x),
                        "arma" = residuals(x),
                        "ar" = residuals(x),
                        "dynardl" = x$model$residuals,
                        "MSM.lm" = MSwM::msmResid(x),
                        residuals(x)))^2  # Default to residuals() if class not matched
    series_name <- "Model Residuals"
  }

  n <- length(uhat)
  if (lags >= n) {
    stop("lag must be less than the length of the series")
  }

  # Create vectors to store LM statistics and p-values
  LM_stats <- numeric(lags)
  p_values <- numeric(lags)

  # Calculate statistics for each lag from 1 to lag using FinTS::ArchTest
  for (i in 1:lags) {
    # Use FinTS::ArchTest function
    arch_result <- FinTS::ArchTest(uhat, lags = i)

    # Extract LM statistic and p-value
    LM_stats[i] <- base::round(arch_result$statistic, round)
    p_values[i] <- base::round(arch_result$p.value, round)
  }

  # Create results matrix
  result <- cbind(1:lags, LM_stats, p_values)
  colnames(result) <- c("lag", "ARCH-LM", "p-value")

  if (is.null(format)) {
    result <- kableExtra::kable(result, format = "rst", align = rep("c", ncol(result)),
                                caption = "ARCH test")
  } else{
    #format <- match.arg(format)
    result <- kableExtra::kable(result, format = format,
                                align = rep("c", ncol(result)),
                                booktable = T, booktabs = T,
                                caption = "ARCH test")
  }

  cat(sprintf("ARCH test based on %s", series_name))
  return(result)
}
