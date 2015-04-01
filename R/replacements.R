#' replace symbols or other character flags
#' @param x vector to replace
#' @param flag_df dataframe with first column being the flag and second the replacement value
#' @param nonflag what to do with non-flagged elements
#' @param as_numeric whether to return column as numeric
#' @details 
#' because of R's coercion rules being somewhat unpredictable regarding character vs factor
#' the behavior of replace_values is to always treat as character data, even if passed in 
#' as a factor 
#' @examples 
#' df <- data.frame(ID = 1, DV = c(1, "BQL", ".", 5))
#' rflags <- data.frame(flag = c("BQL", "."), replacement = -99)
#' df$DVR <- replace_values(df$DV, rflags)
#' 
#' library(dplyr)
#' df <- df %>% mutate(DVR2 = replace_values(DV, rflags))
#' 
#' # powerful with unique_non_numerics
#' df <- df %>% mutate(DVR3 = replace_values(DV, 
#'                                  data.frame(values = unique_non_numerics(DV), 
#'                                             replacement = NA)))
#' @export
replace_values <- function(x, 
                         flag_df,
                         nonflag = NULL,
                         as_numeric=TRUE) {
  if(is.factor(x)) {
    message("converting factor to character before replacement")
    x <- as.character(x)
  }
  if(!is.character(flag_df[[1]])) {
    flag_df[[1]] <- as.character(flag_df[[1]])
  }
  if(!is.character(flag_df[[2]])) {
    flag_df[[2]] <- as.character(flag_df[[2]])
  }
  if(!is.null(nonflag)) {
    x[!(x %in% flag_df[[1]])] <- nonflag
  }
  
  x <- replace_chars(x, flag_df[[1]], flag_df[[2]])
  if(as_numeric) {
    x <- as_numeric(x)
  }
  
  return(x)
}