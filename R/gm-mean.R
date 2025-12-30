#' Geometric mean with NA handling and optional zero propagation
#'
#' Computes the geometric mean of the positive values in \code{x}.
#' Negative values are not allowed: if any negative value is present (ignoring
#' NAs), the function returns \code{NaN}.
#'
#' By default, if \code{na.rm = FALSE} and any \code{NA} is present, the function
#' returns \code{NA_real_}. If \code{na.rm = TRUE}, NAs are removed before
#' computation.
#'
#' Zeros are excluded from the mean calculation unless \code{zero.propagate = TRUE},
#' in which case the function returns \code{0} if any zero is present (after NA
#' handling).
#'
#' If, after filtering, there are no positive values to average (e.g., \code{x}
#' contains only \code{0}s and/or \code{NA}s), the function returns \code{NaN}.
#'
#' @param x A numeric (or coercible-to-numeric) vector.
#' @param na.rm Logical; if \code{TRUE}, remove \code{NA} values before
#'   computation. If \code{FALSE} and any \code{NA} is present, returns
#'   \code{NA_real_}. Default is \code{FALSE}.
#' @param zero.propagate Logical; if \code{TRUE}, return \code{0} when any value
#'   in \code{x} is exactly zero (after NA handling). If \code{FALSE}, zeros are
#'   ignored and only positive values contribute. Default is \code{FALSE}.
#'
#' @examples
#' gm_mean(c(1, 4, 16))
#'
#' gm_mean(c(1, NA, 4)) # Returns NA
#' gm_mean(c(1, NA, 4), na.rm = TRUE)
#'
#' gm_mean(c(0, 1, 4)) # zeros ignored by default
#' gm_mean(c(0, 1, 4), zero.propagate = TRUE)  # returns 0
#'
#' gm_mean(c(-1, 1, 4)) # negatives not allowed -> NaN
#' gm_mean(c(0, 0)) # no positive values -> NaN
#'
#' @export
gm_mean <- function(x, na.rm = FALSE, zero.propagate = FALSE) {
  x <- as.numeric(x)
  
  if (any(x < 0, na.rm = TRUE)) {
    return(NaN)
  }
  
  if (!na.rm && anyNA(x)) {
    return(NA_real_)
  }
  
  if (na.rm) {
    x <- x[!is.na(x)]
  }
  
  if (zero.propagate && any(x == 0)) {
    return(0)
  }
  
  pos <- x[x > 0]
  
  if (length(pos) == 0) {
    return(NaN)
  }
  
  exp(mean(log(pos)))
}