#' Subject data checks
#' 
#' @description 
#' Use this function to perform quick checks on a data.frame to determine if only one row or
#' values exists for each subject within a column. If `.col_check` is `NULL` (which is the default) 
#' then the function will check if the data.frame only has 1 row per subject.
#' 
#' If a variable is provided to `.col_check` then the function will check if each subject only has 
#' 1 distinct value within that column.
#' 
#' @param .df Data.frame to perform checks on
#' @param .subject_col Character column name of subject identifier. (default to ID or USUBJID)
#' @param .col_check Character column name of variable to check if subject only has 1 distinct value of
#' 
#' @examples 
#' library(dplyr)
#' Theoph2 <- Theoph %>% dplyr::group_by(Subject) %>% dplyr::slice(1) %>% dplyr::ungroup()
#' 
#' # Check if each subject has only one row
#' df_subject_check(Theoph2, .subject_col = "Subject")
#' 
#' # Check if each subject has only one distinct Dose value
#' df_subject_check(Theoph2, .subject_col = "Subject", .col_check = "Dose")
#' 
#' @export
df_subject_check <- function(.df,
                             .subject_col = NULL,
                             .col_check = NULL) {
  
  if (is.null(.subject_col)) {
    
    if (!is.null(.df[["ID"]])) {
      .subject_col = "ID"
    } else if (!is.null(.df[["USUBJID"]])) {
      .subject_col = "USUBJID"
    } else {
      stop("No default .subject_col available in data, please define .subject_col")
    }
  }
  
  if (is.null(.col_check)) {
    
    # One row per subject
    if(nrow(.df) == length(unique(.df[[.subject_col]]))){
      return("Data has 1 row per subject")
    } else{
      return("Data has at least 1 subject with multiple rows")
    }
    
  } else {
    
    if (.col_check %in% names(.df)) {
      # One dose per subject
      one_col_subject <-
        .df %>% 
        dplyr::select(dplyr::all_of(c(.subject_col, .col_check))) %>% 
        dplyr::distinct() %>% 
        dplyr::count(!!dplyr::sym(.subject_col)) %>% 
        dplyr::filter(n>1)
      
      if(nrow(one_col_subject) == 0){
        return(paste0("Each subject only has 1 distinct value of ", .col_check))
      } else{
        return(paste0("At least 1 subject has multiple values of ", .col_check))
      }
    } else {
      return(paste0(.col_check, " not found in data. Please check .col_check."))
    }
  }
}
  