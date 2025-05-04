#' Create differences, logarithmic differences, and lags for a given dataset
#'
#' This function adds columns to a dataframe to compute
#'
#' @param dataset A dataframe containing the variables to transform
#' @param diff.vars A character vector of variable names for which to compute simple differences.
#' @param diff.log.vars A character vector of variable names for which to compute logarithmic differences.
#' @param lag.vars A character vector of variable names for which to generate lags.
#'
#' @param max.lags A positive integer specifying the maximum number of lags to generate, defaut is set to 1
#' @param max.differences A positive integer specifying the maximum order of the difference, defaut is set to 1
#'
#' @return A dataframe with the newly added columns
#' @examples
#' #
#' lagsDiff(longley, diff.vars = "GNP", diff.log.vars = "Unemployed")
#' @export
#'
lagsDiff <- function(dataset, diff.vars = NULL, lag.vars = NULL, diff.log.vars = NULL, max.lags=1, max.differences=1) {
  #
  if (!is.data.frame(dataset)) stop("dataset must be a dataframe")
  if (!is.null(lag.vars)  & (missing(max.lags) || !is.numeric(max.lags) || max.lags < 1) ) stop("max.lags must be a positive integer")
  if ((!is.null(diff.vars) || !is.null(diff.log.vars)) & max.differences < 1 ) stop("max.lags must be a positive integer")


  # Step 1: Compute simple differences (if diff.vars is provided)
  if (!is.null(diff.vars)) {
    if (!is.character(diff.vars)) stop("variables must be a character vector")
    for (var in diff.vars) {
      if (!var %in% names(dataset)) stop(paste("Variable", var, "not found in dataset"))
      for (i in 1:max.differences) {
        if (max.differences > 1) {
          diff_col <- c(rep(NA, i), diff(dataset[[var]], differences=i))
          dataset[[paste0("d.", i, ".", var)]] <- round(diff_col, 3)
        } else{
          diff_col <- c(rep(NA, i), diff(dataset[[var]], differences=i))
          dataset[[paste0("d", var)]] <- round(diff_col, 3)
        }
      }
    }
  }

  # Step 2: Compute logarithmic differences (if diff.log.vars is provided)
  if (!is.null(diff.log.vars)) {
    if (!is.character(diff.log.vars)) stop("variables must be a character vector")
    for (var in diff.log.vars) {
      if (!var %in% names(dataset)) stop(paste("Variable", var, "not found in dataset"))
      if (any(dataset[[var]] <= 0, na.rm = TRUE)) stop(paste("Variable", var, "must be strictly positive for log differences"))
      diff_log_col <- c(NA, diff(log(dataset[[var]])))
      dataset[[paste0("dl", var)]] <- round(diff_log_col, 3)
    }
  }

  # Step 3: Generate lags for each variable and lag level

  if (!is.null(lag.vars)) {
    if (!is.character(lag.vars)) stop("variables must be a character vector")
    for (var in lag.vars) {
      if (!var %in% names(dataset)) stop(paste("Variable", var, "not found in dataset"))
      for (i in 1:max.lags) {
        dataset[[paste0("l.", i, ".", var)]] <- c(rep(NA, i), dataset[[var]][1:(length(dataset[[var]]) - i)])
      }
    }
  }


  return(dataset)
}
