#' Count number of IDs in a NONMEM data set
#' 
#' @param .df data.frame containing column .id
#' @param .id column name that will be used to uniquely identify IDs
#' 
#' @examples 
#' 
#' nsub(.df = Theoph, .id = Subject)
#' 
#' @export
nsub <- function(.df, .id="USUBJID") {.df %>% dplyr::distinct({{ .id }}) %>% nrow()}