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
  list(
    `in x not y` = dplyr::setdiff(names(x), names(y)),
    `in y not x` = dplyr::setdiff(names(y), names(x)),
    shared = dplyr::intersect(names(x), names(y))
  )
}