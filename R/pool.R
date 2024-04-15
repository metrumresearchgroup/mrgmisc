#' Test Set Relations Among Two Vectors
#'
#' @description 
#' Test set relations among two vectors.
#' 
#' @param x vector
#' @param y vector
#' 
#' @details
#' The two vectors are tested for elements unique to x, unique to y, 
#' and common to both.
#' 
#' @note 
#' If length of x is zero, it is returned unmodified. If length of 
#' rule is zero, value is all NA.
#' 
#' @author Tim Bergsma
#' 
#' @examples
#' pool(letters[1:5], letters[4:8])
#' 
#' @export
pool <- function(x,y) {
  list(x=setdiff(x,y),y=setdiff(y,x),both=intersect(x,y))
}