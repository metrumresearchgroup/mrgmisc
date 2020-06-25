#' Convert to numeric passing through character for safety
#' 
#' @param x vector
#' @param ... additional argument to as.character
#' @param require_conversion If values are converted to \code{NA}, should
#'   nothing be done (\code{NULL}), a warning be printed (\code{FALSE}), or an
#'   error occur (\code{TRUE})?  (Inputs that are \code{NA} do not trigger the
#'   warning or error.)
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
#' 
#' as_numeric(c("1", "A"), require_conversion=FALSE)
#' @export
#' @family Numerics
as_numeric <- function(x, ..., require_conversion=NULL) {
  ret <- suppressWarnings(as.numeric(as.character(x, ...)))
  if (!is.null(require_conversion)) {
    non_numeric <- unique_non_numerics(x)
    if (length(non_numeric)) {
      msg <-
        sprintf(
          "The following non-numeric values were converted to NA: %s",
          paste0("'", non_numeric, "'", collapse=", ")
        )
      if (require_conversion) {
        stop(msg)
      } else {
        warning(msg)
      }
    }
  }
  ret
}
