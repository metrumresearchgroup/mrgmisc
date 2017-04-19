#' bring select columns to the front of a dataframe
#' @param .df dataframe with column order to adjust
#' @param ... columns to bring to front
#' @examples 
#' head(Theoph)
#' head(bring_to_front(Theoph, conc, Time))
#' @export
bring_to_front <- function(.df, ...) {
  dplyr::select(.df, ..., dplyr::everything())
}