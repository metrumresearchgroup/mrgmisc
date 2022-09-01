#' Convert '.' values into missing values.
#'
#' @param x A character vector
#' @return A character vector with '.' replaced by NA values.
#' @details
#' good for nonmem preprocessing as missing values are represented with '.' in NONMEM
#' @export
#' @examples
#' x <- c(".", "1", "1")
#' replace_dots(x)
replace_dots <- function(x) {
  stopifnot(is.character(x))
  x[x == "."] <- NA
  x
}