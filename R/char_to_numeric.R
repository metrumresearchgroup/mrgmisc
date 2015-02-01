#' convert all columns to numeric
#' 
#' defaults to convert all character columns except named 'C' to numeric
#' useful for nonmem ready datasets with `.` for missing values
#' @param df dataframe to convert character columns to numeric
#' @param exclude_cols vector of column names to be skipped from conversion
#' @examples
#' \dontrun{
#' nm_dat <- char_to_numeric(nm_dat)
#'    # if 'C' col is 0/1 rather than typical 'C' or '.'
#' nm_dat <- char_to_numeric(nm_dat, exclude_cols = NULL) 
#' }
#' @export
#' @return dataframe
char_to_numeric <- function(df, exclude_cols = "C") {
  all_cols <- unlist(lapply(df, typeof))
  char_cols <- all_cols[all_cols =="character"]
  char_cols <- char_cols[!(names(char_cols) %in% exclude_cols)]
  for(i in seq_along(char_cols)) {
    df[[names(char_cols[i])]] <- suppressWarnings(
      as.numeric(df[[names(char_cols[i])]]))
  }
  converted_cols <- 
    message("Columns Converted to Numeric: \n", 
            paste(names(char_cols), collapse = ', '))
  return(df)
}
