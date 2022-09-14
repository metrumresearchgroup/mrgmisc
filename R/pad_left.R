#' Add character to left of every character
#' @param .x vector 
#' @param .n total number of characters result should have
#' @param padding_char padding char to use, defaults to 0
#' 
#' @details
#' Use this function to add specific character to the
#' left of each element in a given vector.
#' 
#' The output is a character vector and default padding
#' character is "0".
#' @examples 
#'pad_left(1, 3)
#'
#'pad_left(c(1, 10), 4, "Z")
#'
#' @export
pad_left <- function(.x, .n, padding_char = "0") {
  if (.n < 1) stop("Number of characters must be positive")
  if (!is.character(padding_char)) {
    padding_char <- as.character(padding_char)
  }
  if (!is.vector(.x, mode="character")) {
    .x <- as.character(.x)
  }
  padLeft(.x, .n, padding_char)
}