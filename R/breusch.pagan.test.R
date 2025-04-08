#' Breusch-Pagan Test for Heteroskedasticity
#'
#' @param model model to be tested fitted "lm" object
#' @param Koenker The LM version of Breusch-Pagan don't lie on normality assumption of résiduals
#' @param summary Logical. If TRUE, prints summary statistics of the fitted auxiliary regression
#'
#' @return A list with class "htest" containing:
#' \itemize{
#'   \item statistic: The BP test statistic
#'   \item parameter: Degrees of freedom
#'   \item method: Description of the test method used
#'   \item p.value: P-value of the test
#'   \item null.value: Statement of the null hypothesis
#' }
#' @export
#'
#' @examples
#' set.seed(123)
#' y <- rgamma(107,shape = 321, scale = 6)
#' X <- matrix(rexp(321, rate = 6)*1000, ncol = 3)
#' reg1 <- lm(y~X)
#' breusch.pagan.test(reg1)
#'
breusch.pagan.test <- function(model, Koenker = TRUE, summary=FALSE) {

  if (!inherits(model, "formula")) {
    # Exogenous X matrix
    X = stats::model.matrix(stats::terms(model), stats::model.frame(model))
    # Remove intercept columns
    X = X[, which(colSums(X == 1) != nrow(X))]
    # get outcome variable y
    y = stats::model.response(stats::model.frame(model))

    resid = stats::lm.fit(X, y)$residuals # residuals from original model
    squared_residuals <- resid^2
    fitted = stats::lm.fit(X, y)$fitted


  }

  n = nrow(X) # Number of observations
  k = ncol(X) # Number of coefs
  # Calcul de sigma^2
  sigma_hat_2 <- sum(resid^2)/n

  if (Koenker == TRUE) {
    breusch.pagan.test <- stats::lm(squared_residuals ~ X)
    # Compute bp stat
    R2 = summary(breusch.pagan.test)$r.squared
    bp_stat = n * R2
    method <- "Koenker Breusch-Pagan LM test on explanatory"
  } else if (Koenker == FALSE) {
    # original version of BP
    # Centered residuals
    w_hat <- squared_residuals - sigma_hat_2
    Z <- X
    # ξ = (1/2)ŵ'Z(Z'Z)⁻¹Z'ŵ/σ⁴
    ZtZ_inv <- solve(t(Z) %*% Z)
    bp_stat <- (1/2) * t(w_hat) %*% Z %*% ZtZ_inv %*% t(Z) %*% w_hat / (sigma_hat_2^2)
    method <- "Breusch-Pagan test on explanatory"
  }

  # Compute p-value (chi-square distribution)
  df =  k  # degree of freedom
  p_value = 1 - stats::pchisq(bp_stat, df = df)

  if (summary==TRUE) {
    print(stats::summary.lm(breusch.pagan.test))
  }

  RVAL <- structure(list(statistic = c("BP" = bp_stat),
                         parameter = c("df" = df),
                         method = method,
                         p.value= p_value,
                         null.value = "Homoscedasticity" ),
                    class = "htest")


  return(RVAL)
}

