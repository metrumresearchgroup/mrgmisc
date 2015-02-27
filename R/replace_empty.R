#' Convert empty strings into missing values.
#'
#' @param x A character vector
#' @return A character vector with empty strings replaced by missing values.
#' @export
#' @examples
#' x <- c("a", "", "c")
#' replace_empty(x)
replace_empty <- function(x) {
  stopifnot(is.character(x))
  x[x == ""] <- NA
  x
}