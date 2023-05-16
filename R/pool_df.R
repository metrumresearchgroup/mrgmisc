#' Display overlapping names between data.frames
#' 
#' List out unique and shared names between data.frames x and y
#' 
#' @param x data.frame to be compared
#' @param y data.frame to be compared
#' 
#' @examples 
#' library(dplyr)
#' 
#' Theoph_mod <- Theoph %>% dplyr::mutate(NEWCOL = 1) %>% dplyr::select(-Dose)
#' pool_df(x = Theoph, y = Theoph_mod)
#' 
#' @author Samuel P Callisto, PhD
#' 
#' @export
pool_df <- function(x,y){
  set_names(
    list(
      x = dplyr::setdiff(names(x), names(y)),
      y = dplyr::setdiff(names(y), names(x)),
      both = dplyr::intersect(names(x), names(y))
    ), 
    c(substitute(x), substitute(y), "both"))
}
