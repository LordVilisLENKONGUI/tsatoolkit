#' White Test for Heteroskedasticity
#'
#' @param model fitted model object (lm, dynlm, ardl, etc.)
#' @param summary logical. If set to TRUE summary statistics of the fitted linear model is printed
#'
#' @returns An object of class "htest" containing the test statistic and p-value.
#' @export
#'
white.test <- function(model, summary = FALSE) {

  # Vérification que l'objet est un modèle valide
  valid_classes <- c("lm", "ardl", "dynlm", "arma", "ar", "dynardl", "MSM.lm", "glm")
  if (!inherits(model, valid_classes)) {
    stop("model must be one of: ", paste(valid_classes, collapse = ", "))
  }

  model.class <- class(model)[1]  # Get class of the model

  # Extract residuals
  uhat2 <- (switch(model.class,
                   "lm" = residuals(model),
                   "ardl" = residuals(model),
                   "dynlm" = residuals(model),
                   "arma" = residuals(model),
                   "ar" = residuals(model),
                   "dynardl" = model$model$residuals,
                   "MSM.lm" = MSwM::msmResid(model),
                   residuals(model)))^2  # Default to residuals() if class not matched

  # Extract fitted values
  fitted.values <- (switch(model.class,
                             "lm" = stats::fitted.values(model),
                             "ardl" = stats::fitted.values(model),
                             "dynlm" = stats::fitted.values(model),
                             "arma" = stats::fitted.values(model),
                             "ar" = stats::fitted.values(model),
                             "dynardl" = model$model$fitted.values,
                             "MSM.lm" = MSwM::msmResid(model),
                             residuals(model)))

  # White regression: regress squared residuals on fitted values and their square
  white.model <- stats::lm(uhat2 ~ fitted.values + I(fitted.values^2))

  method <- "White LM test on fitted values"

  # Number of observations and parameters
  n <- nrow(stats::model.matrix(white.model))  # Nb obs
  k <- ncol(stats::model.matrix(white.model)) - 1  # Nb parameters excluding intercept

  # Calculate White statistic: n * R²
  R2 <- summary(white.model)$r.squared
  W_stat <- n * R2

  # Calculate p-value (chi-square distribution with k degrees of freedom)
  p_value <- 1 - stats::pchisq(W_stat, df = k)

  # Print summary if requested
  if (summary == TRUE) {
    cat("\n", strrep("=", 62), "\n", sep = "")
    cat("\n", strrep("=", 62), "\n", sep = "")
    print(stats::summary.lm(white.model))
    cat("\n", strrep("=", 62), "\n", sep = "")
    cat("\n", strrep("=", 62), "\n", sep = "")
  }

  # Create htest object
  RVAL <- structure(
    list(
      statistic = c("W" = W_stat),
      parameter = c("df" = k),
      method = method,
      p.value = p_value,
      data.name = deparse(substitute(model)),
      null.value = c("Homoscedasticity" = 0)
    ),
    class = "htest"
  )

  return(RVAL)
}
