#' round columns
#' @param df data frame
#' @param col_list list of columns and number of digits to round
#' @examples
#' \dontrun{
#' library(PKPDdatasets)
#' col_list <- list(list("AGE", 1), 
#' list("WEIGHT", 2),
#' list("SCREATININE", 2),
#' list("SERUMALT", 2),
#' list("DELDBP", 2),
#' list("DELSBP", 2))
#' round_columns(aht_trial2, col_list)
#' }
#' @export
round_columns <- function(df, col_list) {
  df <- df
  for (i in seq_along(rounded)) {
    df[[col_list[i][[1]][[1]]]] <- round(df[[col_list[i][[1]][[1]]]], 
                                                col_list[i][[1]][[2]])
  }
  return(df)
}