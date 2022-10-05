#' Count IDs
#' 
#' @param .df data.frame containing column .id
#' @param .id column name that will be used to uniquely identify IDs
#' 
#' @export
nsub <- function(.df, .id=USUBJID) {.df %>% distinct({{ .id }}) %>% nrow()}