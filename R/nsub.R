#' Count number of IDs in a NONMEM data set
#' 
#' @param .df data.frame containing subject column
#' @param .subject_col column name that will be used to uniquely identify IDs
#' 
#' @examples 
#' 
#' nsub(.df = Theoph, .subject_col = "Subject")
#' 
#' @author Samuel P Callisto, PhD
#' 
#' @export
nsub <- function(.df, .subject_col) {
  nrow(dplyr::distinct(.df, !!rlang::ensym(.subject_col)))
}