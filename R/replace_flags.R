#' replace symbols or other character flags
#' @param x vector to replace
#' @param flag_df dataframe with first column being the flag and second the replacement value
#' @param nonflag what to do with non-flagged elements
#' @param as_numeric whether to return column as numeric
#' @examples 
#' df <- data.frame(ID = 1, DV = c(1, "BQL", ".", 5))
#' rflags <- data.frame(flag = c("BQL", "."), replacement = -99)
#' df$DVR <- replace_char_flags(df$DV, rflags)
#' @export
replace_char_flags <- function(x, 
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