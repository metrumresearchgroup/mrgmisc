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
  round_col_group <- function(list_tuple) {
    if (!is.numeric(list_tuple[[2]])) {
      stop(paste("Rounding value must be numeric, instead of provided value of:", list_tuple[[2]]))
    }
    for (col in list_tuple[[1]])  {
      if (col %in% names(df)) {
        df[[col]] <<- round(df[[col]], list_tuple[[2]])
      } else {
        warning(paste("No column with name:", col, "detected"))
      }
    }
  }
  lapply(col_list, round_col_group)
  return(df)
}

