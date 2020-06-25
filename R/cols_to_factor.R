#' convert column to type factor
#' @param df dataframe 
#' @param col_names vector of column names or indices to convert to factor
#' @export
#' @rdname cols_to_factor
cols_to_factor <- function(df, col_names) {
  # TODO: update to properly handle checks (not just length but spelling check) 
  if(length(col_names %in% names(df)) != length(col_names)) {
    warning("Not all columns in col_names found, will convert all available\n")
  }
  message("converting columns (", paste(col_names[col_names %in% names(df)], collapse = ', '),") to factors\n")
 for(i in seq_along(col_names)) {
   if (col_names[[i]] %in% names(df)){
     df[[col_names[i]]] <- factor(df[[col_names[i]]])
   } else {
     warning("Could not find column: ", col_names[[i]], " in the dataset\n")
   }
   }
return(df)
}