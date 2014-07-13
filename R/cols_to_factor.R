#' convert column to type factor
#' @param df dataframe 
#' @param col_names vector of column names or indices to convert to factor
#' @export
#' @rdname cols_to_factor
cols_to_factor <- function(df, col_names) {
  if(length(col_names %in% names(df)) != length(col_names)) {
    warning("Not all columns in col_names found, will convert all available")
  }
  message("converting columns (", col_names[col_names %in% names(df)],") to factors")
 for(i in seq_along(col_names)) {
   df[[col_names[i]]] <- factor(df[[col_names[i]]])
   }
return(df)
}