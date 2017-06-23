#' convert to numeric passing through character for safety
#' @param x vector
#' @param ... additional argument to as.character
#' @examples 
#' # factor with weird levels that we don't want to keep
#' ex <- factor(c(1, 2, 3, 4), levels = c(2, 3, 1, 4)) 
#' ex
#' 
#' # keeps information about the levels, oh no!
#' as.numeric(ex) 
#' 
#' # keeps the labelled values
#' as_numeric(ex)
#' @export
as_numeric <- function(x, ...) {
  suppressWarnings(as.numeric(as.character(x, ...)))
}