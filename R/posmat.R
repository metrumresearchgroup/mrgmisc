#' Coerce a Matrix to be Positive Definite
#'
#' @description 
#' For a square matrix with an all-positive diagonal, elements are limited to 
#' 6 significant digits by rounding the diagonal and shrinking off diagonal elements
#' toward zero.  The off-diagonals are reduced by 3 percent as necessary
#' until the determinant is positive.
#' 
#' @param x matrix with only positive diagonal elements
#' @param \dots extra arguments, ignored
#' 
#' @author Leonid Gibiansky, modified by Tim Bergsma
#' 
#' @examples
#' posmat(matrix(c(10.00006,20.00006,-30,40),2,2))
#'
#' posmat(matrix(rep(100,4),2,2))
#' 
#' @export
posmat <- function(x,...) {
  if(any(diag(x) <=0)) stop("matrix cannot be made positive-definite")
  if(nrow(x) != ncol(x))stop('x is not square')
  sign <- sign(x)
  x <- abs(x)
  characteristic <- trunc(log(x,10))
  mantissa <- log(x,10) - characteristic
  scale <- 10^characteristic
  digits <- 10^mantissa * 1e5
  diagonal <- round(diag(digits))
  digits <- floor(digits)
  diag(digits) <- diagonal
  digits <- digits/1e5
  x <- sign * scale * digits
  diagonal <- diag(x)
  y <- 0.97 * x
  diag(y) <- diagonal
  if(det(x)>0) x else posmat(y)
}