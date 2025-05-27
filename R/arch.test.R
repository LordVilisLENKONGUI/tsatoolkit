#' ARCH Lagrange Multiplier Test for Conditional Heteroskedasticity
#'
#' Performs the Lagrange Multiplier test to detect the presence of
#' Autoregressive Conditional Heteroskedasticity (ARCH).
#'
#' @param y A fitted model object (e.g., \code{lm}, \code{ardl},...) or a numeric vector.
#' @param lags Integer specifying the number of lags to test for ARCH effects. If
#'   \code{NULL}, the number of lags is selected based on the Akaike Information
#'   Criterion (AIC) with a maximum lags set to 10,
#' @param max.lags Integer specifying the maximum number of lags to consider when
#'   automatically selecting lags via AIC. Defaults to 10.
#'
#'
#'
#' @return An object of class \code{htest} containing:
#' \itemize{
#'   \item \code{statistic}: The test statistic (Chi-squared).
#'   \item \code{parameter}: The number of lags (degrees of freedom).
#'   \item \code{p.value}: The p-value of the test.
#'   \item \code{method}: The name of the test ("ARCH LM-test").
#'   \item \code{data.name}: The name of the input data or "Model Residuals".
#'   \item \code{alternative}: The alternative hypothesis ("ARCH effects").
#' }
#'
#' @examples
#' # Example with a linear model
#' set.seed(123)
#' data <- data.frame(y = rnorm(100), x = rnorm(100))
#' lm_model <- lm(y ~ x, data = data)
#' ARCH.test(lm_model)
#'
#' # Example with numeric residuals
#' residuals <- rnorm(100)
#' ARCH.test(residuals, lags = 2)
#'
#' @importFrom stats embed lm pchisq residuals
#' @export
#'
ARCH.test <- function(y, lags = NULL, max.lags = 10) {
  # Extract residuals based on input type
  if (is.numeric(y)) {
    uhat <- y
    uhatnames <- deparse(substitute(y))  # Store name of input vector
  } else {
    model.class <- class(y)[1]  # Get class of the model
    uhat <- switch(model.class,
                   "lm" = residuals(y),
                   "ardl" = residuals(y),
                   "dynlm" = residuals(y),
                   "arma" = residuals(y),
                   "ar" = residuals(y),
                   "dynardl" = y$model$residuals,
                   "MSM.lm" = MSwM::msmResid(y),
                   residuals(y))  # Default to residuals() if class not matched
    uhatnames <- "Model Residuals"
  }

  # Calculate length of residuals
  T <- base::length(uhat)

  # Helper function to select optimal lags using AIC
  aic_lags <- function(uhat, max.lags = max.lags) {
    # Validate input
    if (!is.vector(uhat)) {
      stop("uhat must be a vector")
    }
    if (max.lags >= length(uhat)) {
      stop("max.lags must be strictly less than length of uhat")
    }

    # Square residuals for ARCH test
    uhat <- uhat^2
    # Initialize matrix to store lag, AIC, and BIC
    model_aic <- base::matrix(nrow = max.lags, ncol = 3)
    model_aic[, 1] <- 1:max.lags
    # Create lagged matrix of squared residuals
    mat <- stats::embed(uhat, dimension = max.lags + 1)
    # Fit AR models for each lag order and compute AIC/BIC
    for (p in 1:max.lags) {
      armodel <- lm(mat[, 1] ~ mat[, 2:(p + 1)])
      model_aic[p, 2] <- stats::AIC(armodel)
      model_aic[p, 3] <- stats::BIC(armodel)
    }
    # Convert to data frame and assign column names
    model_aic <- base::as.data.frame(model_aic)
    names(model_aic) <- c("lag", "AIC", "BIC")
    # Return the lag with minimum AIC
    return(model_aic[which.min(model_aic[, 2]), ][1, 1])
  }

  # Assign lags if NULL using AIC-based selection
  if (is.null(lags)) {
    lags <- aic_lags(uhat, max.lags = min(max(1, floor(T^0.25)), max.lags))
  }

  # Validate lags
  if (!is.numeric(lags) || lags < 1) {
    stop("lags must be a positive numeric value")
  }
  if (lags >= T) {
    lags <- max(1, T - 2)  # Adjust lags if too large
  }
  if (lags + 1 > T) {
    stop("Insufficient observations for ARCH test")
  }

  # Create lagged matrix of squared residuals for ARCH test
  X <- stats::embed(uhat^2, lags + 1)
  # Fit linear model of squared residuals on their lags
  lm_model <- stats::lm(X[, 1] ~ X[, -1])

  # Compute ARCH test statistic and p-value
  R2 <- summary(lm_model)$r.squared
  arch_stat <- nrow(X) * R2  # Test statistic: T * R^2
  names(arch_stat) <- "Chi-squared"
  p_value <- 1 - stats::pchisq(arch_stat, df = lags)  # P-value from chi-squared distribution

  # Define test method
  METHOD <- "ARCH LM-test"
  # Name the lags parameter for output
  names(lags) <- "df"
  # Create result object
  result <- list(statistic = round(arch_stat, 4),
                 parameter = lags,
                 p.value = round(p_value, 4),
                 method = METHOD,
                 data.name = uhatnames,
                 alternative = "ARCH effects")
  class(result) <- "htest"
  return(result)
}




