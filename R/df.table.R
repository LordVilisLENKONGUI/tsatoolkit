#' Dickey-Fuller Critical Values Table
#'
#' This function returns critical values for the Dickey-Fuller unit root test
#' based on the specified model type and sample size.
#'
#' @param type Character vector specifying the model type. One of:
#'   \itemize{
#'     \item "ct": critical values for constant and trend
#'     \item "c" : critical values for constant only
#'     \item "n" : critical values for Dickey Fuller unit root test of model without constant nor trend
#'   }
#' @param model Integer specifying the unit root test model (1, 2, or 3).
#'   If NULL, returns critical values for the test of trend or constant. If specified, returns
#'   critical values for unit root tests corresponding to the model.
#' @param N Integer specifying the sample size. If provided, returns only
#'   the row corresponding to the appropriate sample size range.
#'
#' @return A data frame containing critical values at 1%, 5%, and 10%
#'   significance levels for different sample sizes.
#'
#' @examples
#' # Get all critical values for constant and trend model
#' df.table(type = "ct")
#'
#' # Get critical values for specific sample size
#' df.table(type = "c", N = 150)
#'
#' # Get unit root test critical values for model 1
#' df.table(type = "ct", model = 1)
#'
#' @export
df.table <- function(type=c("ct", "c", "n"), model=NULL, N=NULL) {
  # Select the first type if multiple are provided
  type <- match.arg(type)
  T <- c(50, 100, 250, 500, "Inf")

  # Model with constant and trend (tables a)
  pct1ct <- c(3.6, 3.53, 3.49, 3.48, 3.46)
  pct5ct <- c(2.81, 2.79, 2.79, 2.78, 2.78)
  pct10ct <- c(2.38, 2.38, 2.38, 2.38, 2.38)
  ct <- data.frame(T, pct1ct, pct5ct, pct10ct)
  colnames(ct) <- c("T", "1pct", "5pct", "10pct")

  # Model with constant (table b)
  pct1c <- c(3.28, 3.22, 3.19, 3.18, 3.18)
  pct5c <- c(2.56, 2.54, 2.53, 2.52, 2.52)
  pct10c <- c(2.18, 2.17, 2.16, 2.16, 2.16)
  c <- data.frame(T, pct1c, pct5c, pct10c)
  colnames(c) <- c("T", "1pct", "5pct", "10pct")

  # Unit root test - Model 1 (ct)
  pct1cn1 <- c(-4.15, -4.04, -3.99, -3.98, -3.96)
  pct5cn1 <- c(-3.50, -3.45, -3.43, -3.42, -3.41)
  pct10cn1 <- c(-3.18, -3.15, -3.13, -3.13, -3.12)
  cn1 <- data.frame(T, pct1cn1, pct5cn1, pct10cn1)
  colnames(cn1) <- c("T", "1pct", "5pct", "10pct")

  # Unit root test - Model 2 (c)
  pct1cn2 <- c(-3.58, -3.51, -3.46, -3.44, -3.43)
  pct5cn2 <- c(-2.93, -2.89, -2.88, -2.87, -2.86)
  pct10cn2 <- c(-2.60, -2.58, -2.57, -2.57, -2.57)
  cn2 <- data.frame(T, pct1cn2, pct5cn2, pct10cn2)
  colnames(cn2) <- c("T", "1pct", "5pct", "10pct")

  # Unit root test - Model 3 (n)
  pct1cn3 <- c(-2.62, -2.60, -2.58, -2.58, -2.58)
  pct5cn3 <- c(rep(-1.95, 5))
  pct10cn3 <- c(-1.61, -1.61, -1.62, -1.62, -1.62)
  cn3 <- data.frame(T, pct1cn3, pct5cn3, pct10cn3)
  colnames(cn3) <- c("T", "1pct", "5pct", "10pct")

  # Function to determine rowselec based on N
  get_rowselec <- function(N) {
    if (is.null(N)) return(NULL)
    if (N < 25) return(1)
    if (25 <= N & N < 50) return(1)
    if (50 <= N & N < 100) return(2)
    if (100 <= N & N < 250) return(3)
    if (250 <= N & N < 500) return(4)
    if (N >= 500) return(5)
  }

  rowselec <- get_rowselec(N)

  # If model is not specified, use critical values for the test
  if (is.null(model) && is.null(N)) {
    if (type == "ct") {
      return(ct)  # Trend test
    } else if (type == "c") {
      return(c)  # Constant test
    } else if (type == "n") {
      return(cn3)  # Test without constant or trend
    }
  } else if (!is.null(model) && is.null(N)) {
    # If model is specified, return corresponding tables
    if (model == 1 && type == "ct") {
      return(cn1)    # Model 1 - constant and trend
    } else if (model == 2 && type == "c") {
      return(cn2)  # Model 2 - constant only
    } else if (model == 3 && type == "n") {
      return(cn3)  # Model 3 - without constant or trend
    } else {
      stop("Invalid combination of model and type")
    }
  } else if (is.null(model) && !is.null(N)) {
    # If N is specified but not model
    if (type == "ct") {
      return(ct[rowselec, ])  # Trend test
    } else if (type == "c") {
      return(c[rowselec, ])  # Constant test
    } else if (type == "n") {
      return(cn3[rowselec, ])  # Test without constant or trend
    }
  } else if (!is.null(model) && !is.null(N)) {
    # If both model and N are specified
    if (model == 1 && type == "ct") {
      return(cn1[rowselec, ])    # Model 1 - constant and trend
    } else if (model == 2 && type == "c") {
      return(cn2[rowselec, ])  # Model 2 - constant only
    } else if (model == 3 && type == "n") {
      return(cn3[rowselec, ])  # Model 3 - without constant or trend
    } else {
      stop("Invalid combination of model and type")
    }
  }

  # If no condition is met
  stop("Invalid parameters")
}
