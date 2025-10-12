#' @title Augmented Dickey-Fuller Test Summary
#'
#' @description
#' Performs and prints comprehensive ADF unit root tests with three model
#' specifications. Results are formatted and displayed automatically.
#'
#' @param y A numeric vector or time series object containing the data to test for
#' unit roots. NA values are automatically removed.
#'
#' @return ADF test summary of the results
#' \describe{
#'   \item{trend}{Matrix of test statistics and critical values for Model 3
#'     (trend and constant). Rows: ρ (rho), φ (phi), βt (beta trend).}
#'   \item{drift}{Matrix of test statistics and critical values for Model 2
#'     (constant only). Rows: ρ (rho), φ (phi).}
#'   \item{none}{Matrix of test statistics and critical values for Model 1
#'     (no deterministic terms). Row: ρ (rho).}
#' }
#' @importFrom urca ur.df
#' @importFrom greekLetters greeks
#'
#' @export


ur.adf.report <- function(y) {


  # check for NA valu
  if (base::sum(base::is.na(y)) != 0 ) {
    y <- stats::na.omit(y)
  }

  # Model with constant and trend (tables a)
  pct1ct <- c(3.6, 3.53, 3.49, 3.48, 3.46)
  pct5ct <- c(2.81, 2.79, 2.79, 2.78, 2.78)
  pct10ct <- c(2.38, 2.38, 2.38, 2.38, 2.38)
  ct <- base::as.matrix(base::cbind(pct1ct, pct5ct, pct10ct))

  # Model with constant (table b)
  pct1c <- c(3.28, 3.22, 3.19, 3.18, 3.18)
  pct5c <- c(2.56, 2.54, 2.53, 2.52, 2.52)
  pct10c <- c(2.18, 2.17, 2.16, 2.16, 2.16)
  const <- base::as.matrix(base::cbind(pct1c, pct5c, pct10c))



  N <- base::NROW(y)

  # Function to determine rowselec based on N
  get_rowselec <- function(N) {
    if (N < 25) return(1)
    if (25 <= N & N < 50) return(1)
    if (50 <= N & N < 100) return(2)
    if (100 <= N & N < 250) return(3)
    if (250 <= N & N < 500) return(4)
    if (N >= 500) return(5)
  }


  ur.df.trend <- urca::ur.df(y, type='trend', lags = trunc(12 * (length(y)/100)^(1/4)), selectlags = "AIC")
  ur.df.trend.rowselec <- get_rowselec(N)
  #trend.stats <- ct[ur.df.trend.rowselec, ]
  ur.df.trend.nlags <- ur.df.trend@lags
  ur.df.trend.stats <- ur.df.trend@teststat
  ur.df.trend.stats[3] <- base::abs(ur.df.trend@testreg$coefficients[3,3])
  ur.df.trend.critval <- ur.df.trend@cval
  ur.df.trend.critval[3, ] <- ct[ur.df.trend.rowselec,] #trend.stats
  ur.df.trend.results <- cbind(t(round(ur.df.trend.stats,2)),ur.df.trend.critval)
  base::rownames(ur.df.trend.results) <- c(greekLetters::greeks("rho"), greekLetters::greeks("phi"), base::paste0(greekLetters::greeks("beta"),"t"))


  ur.df.drift <- urca::ur.df(y, type='drift', lags = trunc(12 * (length(y)/100)^(1/4)), selectlags = "AIC")
  ur.df.drift.rowselec <- get_rowselec(N)
  #ur.df.drift.stats <- const[ur.df.drift.rowselec, ]
  ur.df.drift.nlags <- ur.df.drift@lags
  ur.df.drift.stats <- ur.df.drift@teststat
  ur.df.drift.stats[2] <- base::abs(ur.df.drift@testreg$coefficients[1,3])
  ur.df.drift.critval <- ur.df.drift@cval
  ur.df.drift.critval[2, ] <- const[ur.df.drift.rowselec,] #drift.stats
  ur.df.drift.results <- cbind(t(round(ur.df.drift.stats,2)),ur.df.drift.critval)
  base::rownames(ur.df.drift.results) <- c(greekLetters::greeks("rho"), greekLetters::greeks("phi"))


  ur.df.none <- urca::ur.df(y, type='none', lags = trunc(12 * (length(y)/100)^(1/4)), selectlags = "AIC")
  ur.df.none.nlags <- ur.df.none@lags
  ur.df.none.stats <- ur.df.none@teststat
  ur.df.none.critval <- ur.df.none@cval
  ur.df.none.results <- cbind(t(round(ur.df.none.stats,2)),ur.df.none.critval)
  base::rownames(ur.df.none.results) <- c(greekLetters::greeks("rho"))

  # print results

  # ADF Model 3
  cat("###################################", "\n")
  cat("## ADF Test : regression trend  ##", "\n")
  cat("###################################", "\n")
  print(ur.df.trend.results)
  cat(" ", "\n")
  if (ur.df.trend.results[3,1] < ur.df.trend.results[3,3]) {
    cat("Trend not significant at 5%", "\n")
  }else cat("Trend significant at 5%", "\n")
  if (ur.df.trend.results[1,1] > ur.df.trend.results[1,3]) {
    cat("Evidence of unit root at 5%", "\n")
  }else cat("Rejection of unit root at 5%", "\n")
  cat("Lags :" , base::NROW(ur.df.trend@testreg$coefficients)-3, "\n")
  cat("Obs  :" , N, "\n")
  cat(" ", "\n")


  # ADF Model 2
  cat("###################################", "\n")
  cat("## ADF Test : regression drift  ##", "\n")
  cat("###################################", "\n")
  print(ur.df.drift.results)
  cat(" ", "\n")
  if (ur.df.drift.results[2,1] < ur.df.drift.results[2,3]) {
    cat("Constant not significant at 5%", "\n")
  }else cat("Constant significant at 5%", "\n")
  if (ur.df.drift.results[1,1] > ur.df.drift.results[1,3]) {
    cat("Evidence of unit root at 5%", "\n")
  }else cat("Rejection of unit root at 5%", "\n")
  cat("Lags :" , base::NROW(ur.df.drift@testreg$coefficients)-2, "\n")
  cat("Obs  :" , N, "\n")
  cat(" ", "\n")

  # ADF Model 1
  cat("###################################", "\n")
  cat("## ADF Test : regression none  ##", "\n")
  cat("###################################", "\n")
  print(ur.df.none.results)
  cat(" ", "\n")
  if (ur.df.none.results[1,1] > ur.df.none.results[1,3]) {
    cat("Evidence of unit root at 5%", "\n")
  }else cat("Rejection of unit root at 5%", "\n")
  cat("Lags :" , base::NROW(ur.df.none@testreg$coefficients)-1 , "\n")
  cat("Obs  :" , N, "\n")
  cat(" ", "\n")

  invisible(list(
    trend = ur.df.trend.results,
    drift = ur.df.drift.results,
    none = ur.df.none.results
  ))


}
