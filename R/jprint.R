#' print multiple values joined together
#' @param ... variadic parameter to of objects to combine
#' @param sep separator value
#' @details print can only take one value to be printed, this function j(oined)print
#'  provides a wrapper to obviate the need to do print(paste0(...)) type work-arounds
#' @examples 
#' result <- "world"
#' jprint("hello", result)
#' @export
jprint <- function(..., sep = " ") {
  print(str_c(..., sep = sep))
}
