#' capitalize all names for a dataframe
#' @param df data frame to capitalize names
#' @details
#' is a simple wrapper function to reduce typing and more easily pass data
#' as it is read from a file
#' @examples 
#' names(Theoph)
#' cTheoph <- capitalize_names(Theoph)
#' names(cTheoph)
#' @export
capitalize_names <- function(df) {
  names(df) <- toupper(names(df))
  return(df)
}

#' lowercase all names for a dataframe
#' @param df data frame to lowercase names
#' @details
#' is a simple wrapper function to reduce typing and more easily pass data
#' as it is read from a file
#' @examples 
#' \dontrun{
#' df <- lowercase_names(df)
#' # make sure all names are lowered as a file is read
#' df <- lowercase_names(read.csv(...))
#' # or via dplyr pipe syntax
#' df <- read.csv(...) %>% lowercase_names
#' }
#' @export
lowercase_names <- function(df) {
  names(df) <- tolower(names(df))
  return(df)
}