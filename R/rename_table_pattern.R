#' simple table name replacement for a given pattern
#' @param df dataframe to rename
#' @param pattern pattern to replace
#' @param replacement value to replace pattern
#' @details
#' a helper to easily wrap around reading tables to perform
#' simple replacement
#' @examples
#' \dontrun{
#' # rename all spaces to underscore
#' df <- rename_table_pattern(read_table("data/data.csv"))
#' # rename "-" to "_"
#' df <- rename_table_pattern(read_table("data/data.csv"), "-", "_")
#' #rename an existing df
#' df <-rename_table_pattern(df, "-", "_")
#' }
#' @export
rename_table_pattern <- function(df, pattern = " ", replacement = "_") {
  names(df) <- gsub(pattern, 
                    replacement = replacement, 
                    x = names(df))
  df
}