#' Calculate geometric mean of variable
#' 
#' @param x variable to summarize
#' 
#' @export
geomean <- function(x){exp(mean(log(x)))}