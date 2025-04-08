#' Kwiatkowski-Phillips-Schmidt-Shin Test for Stationarity
#'
#' @description
#' Implements an enhanced version of the Kwiatkowski-Phillips-Schmidt-Shin (KPSS) test for stationarity.
#' This implementation offers flexibility in bandwidth selection and computes the long-run variance
#' following both the original KPSS paper methodology and using the Newey-West bandwidth selection.
#' The null hypothesis is that the series is stationary.
#' This approche differ from the one used by other package which instead use trunc or floor method.
#'
#'
#' @param y is a vector of time serie
#' @param type according to KPSS (1992) the stationary test rely on the regression of y on intercept (level) or on both intercept and trend
#' @param bandwidth.method Method to compute bandwidth. Options:
#'   - "Newey-West": Use Newey-West automatic bandwidth selection from sandwich package
#'   - "KPSS": Use original KPSS paper's bandwidth selection method
#'
#' @import sandwich
#' @import methods
#' @importFrom stats as.formula
#' @importFrom stats lm
#' @importFrom stats var
#'
#' @returns the return object differ from the satndard htest instead, i get inspered by the Eviews format.
#' @export
#'
#'
#'
kpss.enhanced.test <- function(y, type = c("level", "trend"), bandwidth.method = c("Newey-West", "KPSS")) {
  # Check required package
  if (!requireNamespace("sandwich", quietly = TRUE)) {
    stop("Package 'sandwich' is required. Please install it with install.packages('sandwich')")
  }

  # Arguments handling
  if(missing(type)) {
    type <- NULL
  } else {
    type <- match.arg(type)
  }
  bandwidth.method <- match.arg(bandwidth.method)

  # Global variables
  T <- length(y)
  trend <- 1:T

  # KPSS bandwidth selection method (Section 3, pages 164-165)
  get_kpss_bandwidth <- function(residuals) {
    T <- length(residuals)

    # Compute ρ(j) - autocorrelation function
    rho <- function(j) {
      if(j == 0) return(1)
      r <- sum(residuals[1:(T-j)] * residuals[(j+1):T]) / sum(residuals^2)
      return(r)
    }

    # Optimal l according to KPSS procedure
    n <- floor(T^0.25)
    s0 <- sum(residuals^2)/T
    s1 <- sum(sapply(1:n, function(j) j * rho(j)))
    alpha <- 1.1447 * ((s1/s0)^2)^(1/3)
    l <- floor(alpha * T^(1/3))
    return(l)
  }

  calculate_kpss <- function(model) {
    et <- residuals(model)
    St <- cumsum(et)
    T <- length(et)

    if(bandwidth.method == "Newey-West") {
      bw <- sandwich::bwNeweyWest(model)
      bw = base::ceiling(bw)
      #if ((bw %% 1) < 0.5) bw = base::floor(bw) else bw = base::ceiling(bw)
    } else {
      bw <- get_kpss_bandwidth(et)
    }

    gamma0 <- sum(et^2)/T
    s2_l <- gamma0

    for(j in 1:bw) {
      gamma_j <- sum(et[1:(T-j)] * et[(j+1):T])/T
      w_j <- 1 - j/(bw + 1)
      s2_l <- s2_l + 2 * w_j * gamma_j
    }

    eta <- sum(St^2)/(T^2 * s2_l)

    return(list(
      statistic = eta,
      bandwidth = bw,
      bandwidth.method = bandwidth.method,
      residual_variance = var(et),
      hac_variance = s2_l
    ))
  }

  # Function to display results
  print_results <- function(results, type, regression) {
    cv <- if(type == "level") {
      c("1%" = 0.739, "2.5%" = 0.574, "5%" = 0.463, "10%" = 0.347)
    } else {
      c("1%" = 0.216, "2.5%" = 0.176, "5%" = 0.146, "10%" = 0.119)
    }

    cat("\n Null Hypothesis: Series is stationary","\n")
    cat("\n----------------------------------------------------------------\n")
    cat("Exogenous:", if(type == "level") "Constant" else "Constant, Linear Trend", "\n")
    cat("Bandwidth:", results$bandwidth,
        sprintf("(%s method) using Bartlett kernel\n", results$bandwidth.method))
    cat("\n----------------------------------------------------------------\n")
    cat("\n")
    cat(sprintf("%46s%14s\n", " ", "LM-Stat"))
    cat(sprintf("%-45s%15.4f\n", "Kwiatkowski-Phillips-Schmidt-Shin test", results$statistic))
    cat("\nAsymptotic critical values*:\n")
    cat(sprintf("%12s%33s%14.3f\n", "1% level", "", cv["1%"]))
    cat(sprintf("%12s%33s%14.3f\n", "5% level", "", cv["5%"]))
    cat(sprintf("%12s%33s%14.3f\n", "10% level", "", cv["10%"]))
    cat("\n*Kwiatkowski-Phillips-Schmidt-Shin (1992, Table 1)\n")
    cat("\n")
    cat(sprintf("%-45s%14.6f\n", "Residual variance (no correction)", results$residual_variance))
    cat(sprintf("%-45s%14.6f\n", "HAC corrected variance (Bartlett kernel)", results$hac_variance))
    cat("\n----------------------------------------------------------------\n")

    cat("\nRegression Summary:\n")
    print(summary(regression))
    cat("\n----------------------------------------------------------------\n")

    return(invisible(list(
      statistic = results$statistic,
      bandwidth = results$bandwidth,
      bandwidth.method = results$bandwidth.method,
      critical_values = cv,
      residual_variance = results$residual_variance,
      hac_variance = results$hac_variance,
      regression = regression
    )))
  }

  # Test execution with automatic trend selection
  if(is.null(type)) {
    # Fit trend model and check significance
    reg_trend <- lm(y ~ trend)
    trend_pvalue <- summary(reg_trend)$coefficients["trend", 4]

    if(trend_pvalue < 0.05) {
      # Trend is significant at 5% level
      results_trend <- calculate_kpss(reg_trend)
      cat("\n================================================================\n")
      cat("\nLinear trend is significant at 5% level (p-value =", format(trend_pvalue, digits=4),")")
      cat("\nProceeding with Constant and Linear Trend model for KPSS Test\n")
      reg_trend$call$formula <- as.formula(paste(deparse(substitute(y)), "~ trend"))
      cat("\n================================================================\n")
      return(invisible(print_results(results_trend, "trend", reg_trend)))
    } else {
      # Trend is not significant, use level model
      reg_level <- lm(y ~ 1)
      results_level <- calculate_kpss(reg_level)
      cat("\n================================================================\n")
      cat("Linear trend is not significant at 5% level (p-value =", format(trend_pvalue, digits=4),")")
      cat("\nProceeding with level model for KPSS Test\n")
      reg_level$call$formula <- as.formula(paste(deparse(substitute(y)), "~ 1"))
      cat("\n================================================================\n")
      return(invisible(print_results(results_level, "level", reg_level)))
    }
  } else {
    # If type is specified, behave as before
    reg <- if(type == "level") lm(y ~ 1) else lm(y ~ trend)
    results <- calculate_kpss(reg)
    reg$call$formula <- as.formula(paste(deparse(substitute(y)), "~ trend"))
    return(invisible(print_results(results, type, reg)))
  }
}
