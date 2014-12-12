#' convert to numeric passing through character for safety
#' @param vector
#' @param ... additional argument to as.character
#' @name as_factor
#' @export
as_numeric <- function(x, ...) {
  as.numeric(as.character(x, ...))
}