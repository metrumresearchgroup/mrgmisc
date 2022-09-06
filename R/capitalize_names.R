#' Capitalize all names for a dataframe
#' @param df data frame to capitalize names
#' @details
#' This is a simple wrapper function to reduce typing and more
#' easily pass data as it is read from a file
#' 
#' @examples 
#' names(Theoph)
#' [1] "Subject" "Wt"      "Dose"    "Time"    "conc" 
#' 
#' cTheoph <- capitalize_names(Theoph)
#' names(cTheoph)
#' [1] "SUBJECT" "WT"      "DOSE"    "TIME"    "CONC" 
#' 
#' @export
capitalize_names <- function(df) {
  if (any(class(df) == "character")) stop("Input must be dataframe")
  if (any(class(df) == "list")) stop("Input must be dataframe")
  names(df) <- toupper(names(df))
  return(df)
}