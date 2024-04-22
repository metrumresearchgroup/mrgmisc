#' Find all columns where rows are not identical
#' 
#' @description 
#' Return a data.frame containing only the columns where the rows
#' are not identical. 
#' 
#' @param .df data.frame containing rows to compare
#' 
#' @export
row_compare <- function(.df) {
  
  keep_cols <- list()
  
  for (name.i in names(.df)) {
    
   all_vals.i <- unique(.df[[name.i]])
   
   if (length(all_vals.i) > 1) {
     
     keep_cols <- list(keep_cols, name.i)
     
   }
  }
  
  .df[names(.df) %in% unlist(keep_cols)]
}