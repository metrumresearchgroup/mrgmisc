#' convert to numeric passing through character for safety
#' @param x vector
#' @param ... additional argument to as.character
#' @export
as_numeric <- function(x, ...) {
  as.numeric(as.character(x, ...))
}