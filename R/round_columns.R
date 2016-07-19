#' round columns
#' @param df data frame
#' @param col_list list of vectors of columns and number of digits to round each
#' @details 
#' col_list should contain a list of list tuples, where the first element
#' is a vector of column names to round, and the second value should be the number
#' of digits to round to
#' @examples
#' \dontrun{
#' library(PKPDdatasets)
#' col_list <- list(
#' list("AGE", 1), 
#' list(c("WEIGHT",
#'        "SCREATININE",
#'        "SERUMALT", 
#'        "DELDBP",
#'        "DELSBP"),
#'      2)
#'  )
#' aht_trial_rounded <- round_columns(aht_trial2, col_list)
#' }
#' @export
round_columns <- function(df, col_list) {
  lapply(col_list, function(list_tuple) {
    for (col in list_tuple[[1]])  {
      df[[col]] <<- round(df[[col]], list_tuple[[2]])
    }
  })
  return(df)
}
col_list <- list(
list("WEIGHT", 1),
list(c("SCREATININE",
       "SERUMALT",
       "DELDBP",
       "DELSBP"),
     2)
 )
aht_trial_rounded <- round_columns(aht_trial2, col_list)
