#' Clean Environment by Removing Selected Objects
#'
#' A utility function to selectively remove objects from the calling environment.
#' Objects can be removed by name, by index position, or all at once.
#'
#' @param x A vector specifying which objects to remove. Can be:
#'   \itemize{
#'     \item \code{NULL} (default): Display available objects with their indices
#'     \item \code{numeric}: Remove objects by their index positions
#'     \item \code{character}: Remove objects by their names, or use "all" to remove everything
#'   }
#'
#' @return Invisibly returns \code{NULL}. The function is called for its side effects
#'   (removing objects and printing messages).
#'
#' @details
#' When called without arguments, the function displays all available objects
#' in the calling environment with their corresponding index numbers.
#'
#' For numeric input, only valid indices (positive integers within the range
#' of available objects) are processed.
#'
#' For character input, the special value "all" (case-insensitive) removes
#' all objects from the environment. Other character values are treated as
#' object names to be removed.
#'
#' @seealso \code{\link{rm}}, \code{\link{ls}}
#'
#' @export
clearEnv <- function(x = NULL) {
  objects <- ls(envir = parent.frame())

  if(is.null(x)) {
    cat("Available objects:\n")
    for(i in seq_along(objects)) cat(i, ":", objects[i], "\n")
    return(invisible())
  }

  if(is.numeric(x)) {
    valid_indices <- x[x > 0 & x <= length(objects)]
    if(length(valid_indices) > 0) {
      rm(list = objects[valid_indices], envir = parent.frame())
      cat("Removed:", paste(objects[valid_indices], collapse = ", "), "\n")
    }
  } else if(is.character(x)) {
    # Single line to handle "all" and specific names
    valid_names <- if(length(x) == 1 && tolower(x) == "all") objects else x[x %in% objects]
    if(length(valid_names) > 0) {
      rm(list = valid_names, envir = parent.frame())
      cat("Removed:", paste(valid_names, collapse = ", "), "\n")
    }
  }
}
