#' Display a count of NAs by column in a data.frame
#'
#' @description 
#' \code{nasum} Count the number of \code{NA} values in each element of a list-like object.
#' 
#' @param .df the count of NAs will be calculated for each column in this data 
#' set, with any grouping variables retained
#' 
#' @usage nasum(x,simplify=TRUE)
#' 
#' @details
#' Arguments are passed to \code{simplify}, along with a function 
#' that sums instances of \code{NA}.
#' 
#' @author Tim Waterhouse
#' 
#' @examples
#' nasum(Theoph)
#' 
#' @export
nasum <- function(.df) {
  .df %>% 
    dplyr::summarize(across(everything(), ~ sum(is.na(.))), .groups = "keep") %>% 
    tidyr::pivot_longer(-dplyr::group_cols(), values_to = "n_NA") %>% 
    dplyr::ungroup() %>% 
    dplyr::filter(n_NA > 0)
}