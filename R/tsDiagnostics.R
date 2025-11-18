#' Plot Ts, ACF and PACF figures
#'
#' @param x Vector of datetime, default is NULL
#' @param y Vector of numerical values
#' @param lags Maximum number of lags to compute, default is 20
#' @return A combined plot of geom_line_plot, ACF and PACF using patchwork and ADF test results
#' @export
#'
#' @importFrom forecast ggAcf
#' @importFrom forecast ggPacf
#' @importFrom forecast autoplot
#' @import ggplot2
#' @import patchwork
#'
#' @examples
#' # Example with date and vector
#' date <- seq.Date(from = as.Date("1997-12-19"), to =as.Date(Sys.Date()), by = "day")
#' value <- rnorm(length(date))
#' tsDiagnostics(x=date, y=value)
#'
#' # Example with only vector
#' set.seed(1)
#' dataNA <- c(NA,rnorm(54321))
#' tsDiagnostics(y=dataNA)
#'
tsDiagnostics <- function(y, x=NULL, lags=20) {
  y_name <- deparse(substitute(y))
  if (!is.null(x)) {
    # Create data frame for time series plot with original column names
    df <- data.frame(date = x)
    df[[y_name]] <- y
    # Remove NA values and count them
    df_clean <- stats::na.omit(df)
    n_missing <- base::as.integer(nrow(df) - nrow(df_clean))
    if(n_missing > 0) {
      warning(paste("Removed", n_missing, "NA values from the input vectors"))
    }
    # Get cleaned y values for ACF/PACF with original name
    ybis <- as.data.frame(df_clean[[y_name]])
    colnames(ybis) <- y_name
    geom_line_plot <- ggplot2::ggplot(df_clean, aes(x = date, y = .data[[y_name]])) +
      geom_line() +
      ggplot2::ggtitle(format(colnames(ybis))) +
      ggplot2::ylab(" ")
  } else {
    # Handle vector-only case
    ybis <- stats::na.omit(y)
    n_missing <- base::as.integer(length(y) - length(ybis))
    if(n_missing > 0) {
      warning(paste("Removed", n_missing, "NA values from the input vector"))
    }
    # Convert to time series object and preserve name
    ybis <- as.data.frame(ybis)
    colnames(ybis) <- y_name
    ybis_ts <- stats::ts(ybis)
    geom_line_plot <- forecast::autoplot(ybis_ts) +
      ggplot2::ggtitle(format(colnames(ybis))) +
      ggplot2::ylab(" ")
  }
  # Convert to time series object and preserve name
  ybis <- as.data.frame(ybis)
  colnames(ybis) <- y_name
  ybis_ts <- stats::ts(ybis)
  # Create line plot
  geom_line_plot <- forecast::autoplot(ybis_ts) +
    ggplot2::ggtitle(format(colnames(ybis))) +
    ggplot2::ylab(" ")
  # Create ACF plot
  acf_plot <- forecast::ggAcf(ybis[[1]], lag.max=lags) +
    ggplot2::ggtitle(format(colnames(ybis)))
  # Create PACF plot
  pacf_plot <- forecast::ggPacf(ybis[[1]], lag.max=lags) +
    ggplot2::ggtitle(format(colnames(ybis)))
  # Combine plots with patchwork
  combined_plot <- geom_line_plot / (acf_plot + pacf_plot)

  # Call ur.adf.report (no devtools::load_all() needed!)
  ur.adf.report(ybis_ts)

  return(combined_plot)
}
