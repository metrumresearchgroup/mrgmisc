#' Check if a group of columns are distinct
#' 
#' @description 
#' Provide a data.frame along with any number of column names that exist in the data and 
#' see if all rows are distinct. Can be useful when checking for duplicates.
#' 
#' @param .df A data.frame
#' @param .cols List of column names to perform check on
#' 
#' @examples 
#' duplicate_df <- 
#'   dplyr::tibble(
#'     ID = c(1, 1, 2, 2),
#'     EVID = c(0, 1, 1, 1),
#'     TIME = c(-0.5, 1, 1, 1))
#' 
#' is_distinct(.df = duplicate_df, .cols = c("EVID", "TIME"))
#' 
#' @export
is_distinct <- function(.df, .cols) {
  
  check_cols_in_data <- all(.cols %in% names(.df))
  
  if(!check_cols_in_data) {
    stop("All .cols not in .df")
  }
  
  .distinct_df <- 
    .df %>% 
    dplyr::select(dplyr::all_of(.cols)) %>% 
    dplyr::distinct()
  
  nrow(.distinct_df) == nrow(.df)
  
}