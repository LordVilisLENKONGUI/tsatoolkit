#' White Test for Heteroskedasticity
#'
#' @param model model to be tested itted "lm" object
#' @param on.fitted logical. If set to TRUE instead of same explanatory variables White test fits a linear regression model of the residuals on fitted value
#' @param summary logical. If set to TRUE summary statistics of the fitted linear model is printed
#'
#' @returns A list containing the test statistic and p-value.
#' @export
#'
white.test <- function(model, on.fitted = TRUE, summary=FALSE) {

  if (!inherits(model, "formula")) { # Si un modèle est passé en paramètre
    # Création de la matrice de design X à partir du modèle
    X = stats::model.matrix(stats::terms(model), stats::model.frame(model))
    # Suppression des colonnes constantes (où toutes les valeurs sont 1)
    X = X[, which(colSums(X == 1) != nrow(X))]
    # Extraction de la variable réponse
    y = stats::model.response(stats::model.frame(model))

    resid = stats::lm.fit(X, y)$residuals # residuals from original model
    squared_residuals <- resid^2
    fitted = stats::lm.fit(X, y)$fitted


    # Création des termes quadratiques
    X_2 = X^2
    base::colnames(X_2) = base::paste0(colnames(X_2), "^2")
    # Combinaison des termes linéaires et quadratiques
    X = base::cbind(X, X_2)
  }

  n = nrow(X) # Number of observations
  k = ncol(X) # Number of coefs


  if (on.fitted == FALSE) {
    white.model <- stats::lm(squared_residuals ~ X)
    method <- "White LM test on explanatory"
  } else if (on.fitted == TRUE) {
    white.model <- stats::lm(squared_residuals ~ fitted + I(fitted^2))
    method <- "White LM test on fitted"
  }


  # Calcul de la statistique de BP
  R2 = summary(white.model)$r.squared
  BP_stat = n * R2

  # Calcul de la p-value (distribution chi-carré)
  df = ifelse(on.fitted==TRUE, 2, k)  # degrés de liberté selon l'option fitted
  p_value = 1 - stats::pchisq(BP_stat, df = df)

  if (summary==TRUE) {
    print(stats::summary.lm(white.model))
  }

  RVAL <- structure(list(statistic = c("W" = BP_stat),
                         parameter = c("df" = df),
                         method = method,
                         p.value= p_value,
                         null.value = "Homoscedasticity"),
                    class = "htest")

  return(RVAL)
}
