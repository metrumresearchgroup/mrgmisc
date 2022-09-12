#' Count Number of \code{NA} Values in List Items
#'
#' @description 
#' \code{nasum} Count the number of \code{NA} values in each element of a list-like object.
#' 
#' @param x list of vector-like objects; e.g. a data frame
#' @param simplify logical: coerce to vector result if possible
#' 
#' @usage nasum(x,simplify=TRUE)
#' 
#' @details
#' Arguments are passed to \code{simplify}, along with a function 
#' that sums instances of \code{NA}.
#' 
#' @author Natalie Hsiang
#' 
#' @examples
#' nasum(Theoph)
#' 
#' @export
nasum <- function(x, simplify=TRUE)sapply(x,FUN=function(y)sum(is.na(y)),simplify=simplify)