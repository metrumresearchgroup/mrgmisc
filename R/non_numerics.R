#' find all unique non-numeric values
#' @param x vector to check on
#' @param na.rm remove existing na values before checking
#' @details
#' This function is especially useful for figuring out what
#' non-numeric unique values are in in a column that should be numeric
#' so one can easily replace them with another flag. This function can work well
#' with \code{replace_char_flags} instead of using nested ifelse statements
#' @export
#' @examples
#' \dontrun{
#' dv <- c(1, 2, 4, "88 (excluded)", "bql", "*")
#' unique_non_numerics(dv)
#' replace_char_flags(dv, c("88 (excluded)" = -99, "bql" = -98, "*" = -97))
#' flag <- replace_char_flags(dv, c("88 (excluded)" = -99, "bql" = -98, "*" = -97), 
#'    nonflag = 0)
#' flag
#' }
unique_non_numerics <- function(x, na.rm = TRUE) {
  if(na.rm) x <- x[!is.na(x)]
  xn <- suppressWarnings(as_numeric(x))
  unique(x[is.na(xn)])
}