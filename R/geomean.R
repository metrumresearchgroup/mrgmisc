#' Calculate geometric mean of a vector
#' 
#' @param x vector to summarize
#' 
#' @author Samuel P Callisto, PhD
#' 
#' @examples 
#' 
#' # Find the geometric mean for a variable in Theoph
#' geomean(Theoph$Wt)
#' 
#' geomean(c(1, 2, 3, 4, 5))
#' 
#' @export
geomean <- function(x){
  exp(mean(log(x)))
  }