#' Extract letters from end of string
#' 
#' Based on the RIGHT() function from Microsoft Excel
#' 
#' @param .word string to extract characters from
#' @param .length number of letters to extract from end of .word
#' 
#' @examples 
#' 
#' # If you want to get the end 3 characters from a string
#' right("string-123", 3)
#' 
#' @author Samuel P Callisto, PhD
#' 
#' @export
right <- function(.word, .length){
  substr(.word, nchar(.word)-(.length-1), nchar(.word))
}