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
nsub <- function(.df, .subject_col = "USUBJID") {
  
  if (!inherits(.subject_col, "character")) {
    stop(".subject_col must be character format")
  }
  
  if (!(.subject_col %in% names(.df))) {
    stop(paste0(.subject_col, " not in .df"))
  }
  
  length(unique(.df[[.subject_col]]))
  }