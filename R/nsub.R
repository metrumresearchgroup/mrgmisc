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
nsub <- function(.df, .subject_col = NULL) {
  
  if (!is.null(.subject_col) & !inherits(.subject_col, "character")) {
    stop(".subject_col must be character format")
  }
  
  if (is.null(.subject_col)) {
    
    if (!is.null(.df[["ID"]])) {
      .subject_col = "ID"
    }
    
    if (!is.null(.df[["USUBJID"]])) {
      .subject_col = "USUBJID"
    }
    
    if (is.null(.subject_col)) {
      stop("Please define .subject_col")
    }
    
  }
  
  .df %>% 
    dplyr::distinct(!!dplyr::sym(.subject_col)) %>% 
    nrow()
  }