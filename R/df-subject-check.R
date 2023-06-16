#' Subject data checks
#' 
#' @description 
#' Use this function to perform quick checks on a data.frame such as:
#' - One row per subject
#' - One dose level per subject
#' - Multiple EVID's per subject
#' 
#' @param .df Data.frame to perform checks on
#' @param .subject_col Character column name of subject identifier. (default to ID or USUBJID)
#' @param .dose_col Character column name of dose level. (default to DOSE)
#' @param .evid_col Character column name of event indicator variable. (default to EVID)
#' @param .blq_col Character column name of below limit of quantification indicator (optional)
#' 
#' @export
df_subject_check <- function(.df,
                             .subject_col = NULL,
                             .dose_col = "DOSE",
                             .evid_col = "EVID",
                             .blq_col = "BLQ") {
  
  if (is.null(.subject_col)) {
    
    if (!is.null(.df[["ID"]])) {
      .subject_col = "ID"
    } else if (!is.null(.df[["USUBJID"]])) {
      .subject_col = "USUBJID"
    } else {
      stop("No default .subject_col available in data, please define .subject_col")
    }
  }
  
  if (is.null(.df[[.dose_col]])) {
    stop(".dose_col not in provided data, please define .dose_col")
  }
  
  if (is.null(.df[[.evid_col]])) {
    stop(".evid_col not in provided data, please define .evid_col")
  }
  
  # Perform operations
  return_list <- list()
  
  # One row per subject
  if(nrow(.df) == length(unique(.df[[.subject_col]]))){
    return_list$OneRowPerSubject <- TRUE
  } else{
    return_list$OneRowPerSubject <- FALSE
  }
  
  # One dose per subject
  dose_per_subject <-
    .df %>% 
    dplyr::select(c(.subject_col, .dose_col)) %>% 
    dplyr::distinct() %>% 
    dplyr::count(!!dplyr::sym(.subject_col)) %>% 
    dplyr::filter(n>1)
  
  if(nrow(dose_per_subject) == 0){
    return_list$OneDosePerSubject <- TRUE
  } else{
    return_list$OneDosePerSubject <- FALSE
  }
  
  if(!is.null(.df[[.blq_col]])) {
    .df <- .df %>% dplyr::filter(!!dplyr::sym(.blq_col) == 0)
  }
  
  # Subject has multiple EVID
  evid_per_subject <-
    .df %>% 
    dplyr::select(dplyr::all_of(c(.subject_col, .evid_col))) %>% 
    dplyr::distinct() %>% 
    dplyr::count(!!dplyr::sym(.subject_col)) %>% 
    dplyr::filter(n==1)
  
  if(nrow(evid_per_subject) == 0){
    return_list$MultipleEvidPerSubject <- TRUE
  } else{
    return_list$MultipleEvidPerSubject <- FALSE
  }
  
  return(return_list)
  
}