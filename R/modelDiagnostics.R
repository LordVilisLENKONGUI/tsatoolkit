#' Post-Estimation Diagnostic Tests for Statistical Models
#'
#' @description
#' Performs a comprehensive set of diagnostic tests on statistical models to verify
#' underlying assumptions about residuals: autocorrelation, heteroscedasticity,
#' and normality. The function adapts its tests based on the model type.
#'
#' @param model An object representing a statistical model
#' @param lags Number of lags for time series tests (default: 20)
#' @param significance_level Significance level for hypothesis tests (default: 0.05)
#' @param on.fitted Use fitted values for White test (default: TRUE)
#' @param format Output format for the table: "rst", "pandoc" or "latex". If NULL, defaults to "rst"
#' @param round Number of decimal places in output (default: 4)
#' @param model.class Character vector specifying valid model types. If NULL, uses model's class
#'
#' @return A list object of class 'post_diagnostics' containing:
#'   \item{summary}{A formatted table of test results}
#'   \item{full_tests}{Detailed test results (accessible via $)}
#'   \item{model_info}{Model metadata including type and sample size}
#'
#' @examples
#' # Using built-in cars dataset
#' data(longley)
#' model <- lm(Unemployed ~ GNP, data = longley)
#' diagnostics <- modelDiagnostics(model)
#'
#' # With custom model class
#' data(longley)
#' model <- lm(Unemployed ~ GNP, data = longley)
#' diagnostics <- modelDiagnostics(model, model.class = "lm")
#'
#' @importFrom stats acf na.omit qqline qqnorm residuals model.matrix terms model.frame model.response lm.fit pchisq Box.test
#' @importFrom lmtest bgtest
#' @importFrom tseries jarque.bera.test
#' @importFrom FinTS ArchTest
#' @importFrom kableExtra kable
#' @importFrom graphics par lines abline
#'
#' @export
modelDiagnostics <- function(model, lags = 20, significance_level = 0.05,
                              on.fitted = TRUE, format = NULL, round = 3,
                              model.class = NULL) {
  # Define supported model types
  supported_types <- c("lm", "glm", "ardl", "arma", "dynardl", "ar", "MSM.lm")

  # Define time series classes to prioritize
  ts_classes <- c("ardl", "arma", "dynardl", "ar", "MSM.lm")

  # Get model class if not specified
  if (is.null(model.class)) {
    model_classes <- class(model)
    # First check for time series classes
    ts_match <- intersect(model_classes, ts_classes)
    if (length(ts_match) > 0) {
      # Take the first time series class found
      model.class <- ts_match[1]
    } else {
      # If no time series class found, take the first supported class
      supported_match <- intersect(model_classes, supported_types)
      if (length(supported_match) > 0) {
        model.class <- supported_match[1]
      } else {
        model.class <- model_classes[1]  # Fallback to first class
      }
    }
  }

  # Input validation
  if (!is.character(model.class)) {
    stop("The model.class argument must be a character string or NULL")
  }

  # Check if model type is supported
  if (!any(model.class %in% supported_types)) {
    stop(sprintf("Unsupported model type: %s\nSupported types are: %s",
                 model.class, paste(supported_types, collapse = ", ")))
  }

  # Extract residuals based on model type

  residuals_model <- tryCatch({
    switch(model.class,
           "lm" = residuals(model),
           "glm" = residuals(model, type = "deviance"),
           "ardl" = residuals(model),
           "dynlm" = residuals(model),
           "arma" = residuals(model),
           "ar" = residuals(model),
           "dynardl" = model$model$residuals,
           "MSM.lm" = MSwM::msmResid(model))
  }, error = function(e) {
    stop(sprintf("Error extracting residuals: %s", e$message))
  })

  # Check for missing values
  if (any(is.na(residuals_model))) {
    warning("Warning: Missing values present in residuals")
  }

  # Create results structure
  results <- structure(
    list(
      model_info = list(
        type = model.class,
        n_obs = length(residuals_model),
        test_date = Sys.time()
      )
    ),
    class = "post_diagnostics"
  )

  if ( inherits(model, c("ar", "arma", "ardl", "dynardl", "MSM.lm")) ) {

    par(mfrow=c(2,2),
        bty="l",
        cex.axis = .75,
        font.main = 1)
    #plot(na.omit(fitted(model)), na.omit(abs(residuals(model)/sqrt(abs(residuals(model))))),
    #      main="Residuals vs Fitted",
    #     type="p", xlab="Fitted values", ylab=expression(sqrt(abs("Standardized residuals"))))  # Residuals vs Fitted
    plot(residuals_model, type="l", ylab=expression(Residuals[t]), main=expression("Residuals"[t]*" vs Time"))
    lines(seq_along(residuals_model),stats::predict(stats::loess(residuals_model~seq_along(residuals_model))),col="red", lwd=.45)
    abline(h=0, lwd=.45, lty=2)
    #abline(h = 2 * stats::sd(residuals_model, na.rm = TRUE),  lwd=.5, lty=2)
    #abline(h = -2 * stats::sd(residuals_model, na.rm = TRUE), lwd=.5, lty=2)
    qqnorm(na.omit(residuals_model), main=expression("Q-Q "*"Residuals"[t]))
    qqline(na.omit(residuals_model), lty =2)  # Added Q-Q plot
    forecast::Acf(residuals_model, lag.max = min(lags, length(residuals_model)-1), main=expression("Residuals"[t]))
    forecast::Acf(residuals_model^2, lag.max = min(lags, length(residuals_model)-1), main=base::expression("Residuals"[t]^2*" (ARCH Effects)"))
    par(mfrow=c(1,1),bty="l")
  } else if ( inherits(model, c("lm", "glm") ) ) {

    par(mfrow=c(2,2), bty="l", cex.axis = .75)  # Organise les graphiques sur une ligne
    plot(model, which=1)  # Residuals vs Fitted
    plot(model, which=2, bty="l")  # Normal Q-Q
    plot(model, which=4, bty="l")  # Cook distance
    plot(model, which=3, bty="l")  # Scale-Location
    par(mfrow=c(1,1), bty="l")  # Retour à la disposition normale

  }





  # White test implementation
  white.test <- function(model, on.fitted = TRUE, summary = FALSE) {
    # For dynardl models, extract the underlying lm model
    if (inherits(model, "dynardl")) {
      model <- model$model
    }

    # Extract model matrix and response
    X <- stats::model.matrix(stats::terms(model), stats::model.frame(model))
    X <- X[, which(colSums(X == 1) != nrow(X))]
    y <- stats::model.response(stats::model.frame(model))

    # Calculate residuals and fitted values
    resid <- stats::lm.fit(X, y)$residuals
    squared_residuals <- resid^2
    fitted <- stats::lm.fit(X, y)$fitted

    n <- nrow(X)
    k <- ncol(X)

    # Perform White test
    if (!on.fitted) {
      X_2 <- X^2
      colnames(X_2) <- paste0(colnames(X_2), "^2")
      X <- cbind(X, X_2)
      white.model <- stats::lm(squared_residuals ~ X)
      method <- "White LM test on explanatory"
      df <- k
    } else {
      white.model <- stats::lm(squared_residuals ~ fitted + I(fitted^2))
      method <- "White LM test on fitted"
      df <- 2
    }

    # Calculate test statistics
    R2 <- summary(white.model)$r.squared
    BP_stat <- n * R2
    p_value <- 1 - stats::pchisq(BP_stat, df = df)

    # Return results
    structure(list(
      statistic = c("W" = BP_stat),
      parameter = c("df" = df),
      method = method,
      p.value = p_value,
      null.value = "Homoscedasticity"
    ), class = "htest")
  }

  # Execute diagnostic tests
  tryCatch({
    # Autocorrelation tests
    if (model.class %in% c("lm", "glm")) {
      results$autocorr_bg <- lmtest::bgtest(model)
    } else if (model.class %in% c("ardl", "arma", "dynardl", "MSM.lm")) {
      results$autocorr_lb <- stats::Box.test(residuals_model,
                                             lag = lags,
                                             type = "Ljung-Box")
    }

    # Heteroscedasticity tests
    if (model.class %in% c("lm", "glm")) {
      results$hetero_white <- white.test(model, on.fitted = on.fitted)
    }
    if (model.class %in% c("ardl", "dynardl", "arma", "MSM.lm")) {
      results$hetero_Arch <- FinTS::ArchTest(residuals_model, lags = lags)
    }

    # Normality test
    if (length(residuals_model) >= 2000) {
      warning("Large sample size: Jarque-Bera test may be oversensitive")
    }
    results$normality <- tseries::jarque.bera.test(residuals_model)

    # Prepare results table
    test_results <- data.frame(
      Test = character(),
      Statistic = numeric(),
      "P-value" = numeric(),
      stringsAsFactors = FALSE
    )

    # Helper function for adding test results
    add_test_result <- function(test_name, test_object) {
      if (!is.null(test_object)) {
        data.frame(
          Test = test_name,
          Statistic = test_object$statistic,
          "P-value" = test_object$p.value
        )
      }
    }

    # Test name mapping
    test_map <- c(
      autocorr_bg = "Breusch-Godfrey",
      autocorr_lb = "Ljung-Box",
      hetero_white = "White",
      hetero_Arch = "ARCH",
      normality = "Jarque-Bera"
    )

    # Fill results table
    for (test in names(test_map)) {
      if (!is.null(results[[test]])) {
        test_results <- rbind(test_results,
                              add_test_result(test_map[test],
                                              results[[test]]))
      }
    }

    base::rownames(test_results) <- NULL

    # Format output table
    options(knitr.kable.NA = "")
    fmt <- if (is.null(format)) "rst" else format

    formatted_table <- kableExtra::kable(
      test_results,
      format = fmt,
      align = rep('c', ncol(test_results)),
      booktabs = TRUE,
      digits = c(0, rep(round, ncol(test_results) - 1))
    )

    # Store all results
    results$full_tests <- list(
      autocorr_bg = results$autocorr_bg,
      autocorr_lb = results$autocorr_lb,
      hetero_white = results$hetero_white,
      hetero_Arch = results$hetero_Arch,
      normality = results$normality
    )
    results$summary <- formatted_table

  }, error = function(e) {
    warning(sprintf("Error executing tests: %s", e$message))
    return(NULL)
  })

  # Add attributes
  attr(results, "model.class") <- model.class
  attr(results, "significance_level") <- significance_level

  return(results)
}

#' @export
print.post_diagnostics <- function(x, ...) {
  cat("Diagnostic Tests Results\n\n")
  print(x$summary)
  invisible(x)
}

