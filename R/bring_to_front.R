#' bring select columns to the front of a dataframe
#' @param df dataframe
#' @param ... columns to bring to front
#' @examples 
#' Theoph %>% bring_to_front(conc, Time)
#' @export
bring_to_front <- function(.df, ...) {
  dplyr::select(.df, ..., dplyr::one_of(names(.df)))
}