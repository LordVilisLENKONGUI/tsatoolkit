#' Get NBER recession dates
#'
#' @param return_dataframe Logical, if TRUE returns data, if FALSE returns URL
#'
#' @returns Data frame or URL string
#' @export
#'
#' @examples
#' NBER.recession()
#' NBER.recession(FALSE)
#'
#'
#'
NBER.recession <- function(return_dataframe = TRUE) {
  switch(as.character(return_dataframe),
         "TRUE" = base::as.data.frame(lapply(utils::read.csv("http://data.nber.org/data/cycles/20210719_cycle_dates_pasted.csv"), base::as.Date)),
         "FALSE" = "http://data.nber.org/data/cycles/20210719_cycle_dates_pasted.csv")
}



#' Clear Console
#'
#' Clears the R console output in both RStudio and other R environments.
#'
#' @return None
#' @export
#'
#' @examples
#' clearConsole()

clearConsole <- clearConsole <- function() {
  if (base::Sys.getenv("RSTUDIO") == "1") {
    # RStudio-specific method
    base::cat("\014")
  } else {
    # other than RStudio
    base::cat("\f")
  }
}

