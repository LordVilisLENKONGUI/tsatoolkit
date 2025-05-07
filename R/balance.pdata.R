#' Balance a panel dataset
#'
#' @description
#' This function balances a panel dataset by keeping only observations
#' that correspond to the dates available for the individual with the
#' minimum number of observations.
#'
#' @param data A panel dataset (pdata.frame or data.frame)
#' @param index Character string specifying the name of the column that identifies individuals
#'
#' @return A balanced panel dataset as a data.frame
#'
#' @details
#' The function follows these steps:
#' 1. Count the number of observations per individual
#' 2. Find the individual with the minimum number of observations
#' 3. Extract the dates available for this individual
#' 4. Keep only observations from the original dataset that match these dates
#'
#' @importFrom dplyr group_by count sym
#' @importFrom magrittr %>%
#'
#'@export
#'
balance.pdata <- function(data, index) {

  if (!inherits(data, c("pdata.frame", "data.frame" ))) {
    print("Data should be from class pdata.frame or data.frame")
  }


  # 1. Count the number of observations Ti for each individual ni
  count <- data %>% dplyr::group_by(!!sym(index)) %>% dplyr::count() # !!sym(index) transforms "index" into index

  # 2. Extract the name of the individual with the minimum number of observations
  ni.min <- as.character(count[[index]][which.min(count$n)])

  # 3. Filter to get data for this individual
  data_subset <- data[data[[index]] == ni.min, ]

  # 4. Extract the name of the date column
  date_col_name <- names(data_subset)[which(sapply(data_subset, function(x) inherits(x, "Date")))]
  #date_col_name <- names(data_subset)[which(sapply(data_subset, is.Date))]

  # 5. Extract the available dates for this individual
  Ti_ni.min <- unique(data_subset[[date_col_name]])

  # 6. Filter the complete panel for these dates
  data_balanced <- data[data[[date_col_name]] %in% Ti_ni.min, ]

  # 7. Return the result
  return(as.data.frame(data_balanced))
}
