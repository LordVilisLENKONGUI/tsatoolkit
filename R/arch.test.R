#' ARCH Test for Conditional Heteroskedasticity
#'
#' Performs an ARCH test using the Lagrange Multiplier (LM) approach to detect
#' the presence of Autoregressive Conditional Heteroskedasticity (ARCH) effects
#' in a time series or model residuals.
#'
#' @param x Numeric vector or following models object are supported : \code{lm}, \code{ardl}, \code{dynlm},
#'   \code{arma}, \code{ar}, \code{dynardl} and \code{MSM.lm}. or a numeric vector.
#' @param lags Integer specifying the maximum number of lags to test for ARCH
#'   effects ; defaults to 10.
#' @param format Character string specifying the output format for the table. Accepts formats supported
#' by \code{kableExtra::kable()}.
#' @param round Integer specifying the number of decimal to round; defaults to 3.
#' @param demean Default is set to TRUE, then the vector not residual is demeaned
#'
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
#' @importFrom stats residuals
#' @importFrom kableExtra kable
#' @export
ARCH.LM.test <- function(x, lags=10, format = NULL, round=3, demean=TRUE) {


  if (is.vector(x)) {
    if (any(is.na(x))) {
      x <- x[!is.na(x)]
    }
  }


  if (is.vector(x) && isTRUE(demean)) {
    # Store the name for display purposes
    series_name <- deparse(substitute(x))
    uhat <- (x-base::mean(x))^2
  }else if (is.vector(x) && !isTRUE(demean)) {
    series_name <- deparse(substitute(x))
    uhat <- (x)^2
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
  arch.test.Matrix <- stats::embed(x, lags)



  # Calculate statistics for each lag from 1 to lag using FinTS::ArchTest
  nlags <- 2:(lags+1)
  for (p in nlags) {

    matarch <- stats::embed(uhat, p)

    arch.lm <- stats::lm( matarch[,1]~matarch[,2:p] )
    arch.lm.sum <- base::summary(arch.lm)

    LM_stats[p] <- base::round(dim(arch.lm$model)[1]*arch.lm.sum$r.squared, round)
    p_values[p] <- base::round(1 - stats::pchisq(LM_stats[p], df = dim(arch.lm.sum$coefficients)[1]-1 ), round)

  }

  arch.ac <- stats::acf(uhat, lag.max = lags, plot = FALSE)$acf
  arch.ac <- base::as.numeric(base::sprintf("%12.3f", arch.ac))

  formatted_pvalues <- sapply(p_values, function(p) {
    if (p < 0.001) {
      "0.000"
    } else {
      format(round(p, round), nsmall = round)
    }
  })

  # Create results matrix
  result <- cbind(1:lags, arch.ac[-1], LM_stats[-1], formatted_pvalues[-1])
  colnames(result) <- c("Lag", "AC", "ARCH-LM", "p-value")

  if (is.null(format)) {
    result <- kableExtra::kable(result, format = "rst", align = c("c", "r", "c", "c" ),
                                caption = "ARCH test")
  } else{
    #format <- match.arg(format)
    result <- kableExtra::kable(result, format = format,
                                align = c("c", "r", "c", "c" ),
                                booktable = T, booktabs = T,
                                caption = "ARCH test")
  }
  #cat(sprintf("ARCH test based on %s", series_name))
  return(result)
}
