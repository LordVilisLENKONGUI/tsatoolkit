#' Descriptive Statistics Summary
#'
#' Computes and displays descriptive statistics for numeric data in a formatted table.
#'
#' @param object A numeric vector, matrix, or data frame containing numeric data to summarize.
#' @param type A character string specifying the type of summary: \code{"short"} (default) for basic statistics or \code{"long"} to include skewness and kurtosis.
#' @param format Output format for the table: \code{"rst"} (default if NULL), \code{"pandoc"}, or \code{"latex"}.
#' @param round Number of decimal places to display for statistical measures (default is 2).
#'
#' @return A \code{knitr::kable} formatted table containing descriptive statistics.
#' @details
#' The function calculates basic statistics (min, Q1, median, mean, Q3, max, standard deviation,
#' and coefficient of variation) for \code{type = "short"}. For \code{type = "long"}, it also
#' includes skewness and kurtosis. Non-numeric inputs are handled appropriately: character
#' vectors return a base summary, and data frames are filtered to numeric columns only.
#'
#' @examples
#' # Example with a numeric vector
#' data("longley")
#' describe(longley)
#'
#' # Example with missing values
#' describe(c(NA, rnorm(1997)))
#'
#' @importFrom kableExtra kable row_spec
#' @importFrom moments skewness kurtosis
#' @export
describe <- function(object, type = c("short", "long"), format = NULL, round = 2) {

  # Conversion des types spéciaux en matrice
  special_classes <- c("mts", "ts", "matrix", "array", "tbl_df", "tbl")
  if (any(sapply(special_classes, function(x) inherits(object, x)))) {
    object <- as.matrix(object)
  }

  type <- match.arg(type)

  #------------------------------------------------/
  # Fonction interne de calcul des statistiques
  #------------------------------------------------/
  calc_stat <- function(x, type) {
    n <- length(x)
    n_na <- sum(is.na(x))
    moyenne <- mean(x, na.rm = TRUE)
    stdev <- sd(x, na.rm = TRUE)

    stats <- c(
      Obs = n,
      "NA's" = if (n_na > 0) n_na else NA,
      Minimum = min(x, na.rm = TRUE),
      Q1 = unname(quantile(x, 0.25, na.rm = TRUE)),
      Median = median(x, na.rm = TRUE),
      Mean = moyenne,
      Q3 = unname(quantile(x, 0.75, na.rm = TRUE)),
      Maximum = max(x, na.rm = TRUE),
      Std.Dev = stdev
    )

    if (type == "long") {
      x_clean <- na.omit(x)
      stats <- c(stats,
                 Coef.Var = if (moyenne != 0) (stdev / moyenne) * 100 else NA,
                 Skewness = tryCatch(moments::skewness(x_clean), error = function(e) NA),
                 Kurtosis = tryCatch(moments::kurtosis(x_clean), error = function(e) NA)
      )
    }

    stats
  }

  #------------------------------------------------/
  # Fonction pour aligner les nombres sur le point décimal
  #------------------------------------------------/
  align_decimal <- function(x, decimals = 2) {
    int_cols <- c("Obs", "NA's")

    result <- matrix("", nrow = nrow(x), ncol = ncol(x),
                     dimnames = list(rownames(x), colnames(x)))

    for (col in colnames(x)) {
      vals <- x[, col]
      col_name_width <- nchar(col)

      if (col %in% int_cols) {
        # Entiers : pas de décimales
        formatted <- ifelse(is.na(vals), "", sprintf("%d", as.integer(vals)))
      } else {
        # Décimaux : formatage fixe
        formatted <- ifelse(is.na(vals), "", sprintf(paste0("%.", decimals, "f"), vals))
      }

      # Largeur max entre les données et le nom de colonne
      max_data_width <- max(nchar(formatted), na.rm = TRUE)
      target_width <- max(max_data_width, col_name_width)

      # Aligner les données à droite dans cette largeur
      result[, col] <- sprintf(paste0("%", target_width, "s"), formatted)

      # Centrer le nom de colonne
      padding_total <- target_width - col_name_width
      pad_left <- floor(padding_total / 2)
      pad_right <- ceiling(padding_total / 2)
      colnames(result)[colnames(result) == col] <- paste0(
        strrep(" ", pad_left), col, strrep(" ", pad_right)
      )
    }
    result
  }

  #------------------------------------------------/
  # Traitement principal
  #------------------------------------------------/
  if (is.vector(object) && !is.list(object)) {
    if (is.character(object)) {
      return(summary(object))
    }
    result <- t(as.matrix(calc_stat(object, type)))
    rownames(result) <- deparse(substitute(object))
  } else {
    # Filtrer les colonnes numériques pour les data.frames
    if (is.data.frame(object)) {
      numeric_cols <- sapply(object, is.numeric)
      if (sum(numeric_cols) == 0) {
        stop("No numeric columns found in the input DataFrame")
      }
      object <- object[, numeric_cols, drop = FALSE]
    } else if (is.matrix(object) && !is.numeric(object)) {
      stop("Matrix must be numeric")
    }

    # Calcul des statistiques pour chaque colonne
    result <- t(apply(object, 2, calc_stat, type = type))
  }

  # Supprimer la colonne NA's si aucune valeur manquante
  if (all(is.na(result[, "NA's"]))) {
    result <- result[, colnames(result) != "NA's", drop = FALSE]
  }

  # Appliquer l'alignement décimal
  result <- align_decimal(result, decimals = round)

  #------------------------------------------------/
  # Formatage et retour
  #------------------------------------------------/
  options(knitr.kable.NA = "")

  fmt <- if (is.null(format)) "rst" else format

  kableExtra::kable(
    result,
    format = fmt,
    align = rep("r", ncol(result)),
    booktabs = if (!is.null(format)) TRUE else FALSE
  )
}
