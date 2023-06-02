#' Wrap Text in Parentheses
#'
#' @description 
#' Wrap text in parentheses.  Useful when formating latex tables.
#' 
#' @param x character
#' @param \dots ignored
#' 
#' @details
#' Text is wrapped in parentheses, with no intermediate spaces.
#' 
#' @author Tim Bergsma
#' 
#' @examples
#' parens('x')
#' 
#' @export
parens <- function(x,...)paste0('(',x,')')