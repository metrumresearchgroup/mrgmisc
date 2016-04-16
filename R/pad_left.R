#' add left padding to a vector of values
#' @param .x vector 
#' @param .n total number of characters result should have
#' @param padding_char padding char to use, defaults to 0
#' @examples 
#' pad_left(1, 3)
#' pad_left(c(1, 10, 100), 4)
#' @export
pad_left <- function(.x, .n, padding_char = "0") {
  if (!is.character(padding_char)) {
    padding_char <- as.character(padding_char)
  }
  if (!is.vector(.x, mode="character")) {
    .x <- as.character(.x)
  }
  padLeft(.x, .n, padding_char)
}