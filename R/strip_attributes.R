#' strip additional attributes that make dplyr fail
#' @param df dataframe
#' @param attr_names names of attributes that you want to remove
#' @export
#' @details 
#' dplyr as of 0.4 still does not handle columns with non-generic attributes and
#' will error out rather than ignoring them etc.
#' This function will allow one to strip attribute names to allow the data frame to be
#' used within the dplyr pipeline without issue.
#' 
#' This type of data is common when dealing with SAS datasets
#' @examples 
#' foo <- data.frame(a = 1:5, b = 1:5, c=letters[1:5])
#' df <- foo
#' attr(df$a, "label") <- "col a"
#' attr(df$b, "label") <- "col b"
#' attr(df$c, "label") <- "col c"
#' 
#' library(dplyr)
#' df %>% filter(a %in% c(1, 2)) # used to throw an error in old versions
#' df %>% strip_attributes("label") %>% filter(a %in% c(1, 2))
#' 
#' attr(df$a, "notes") <- "a note"
#' # now column a has attributes of label and notes
#' df %>% strip_attributes(c("label", "notes")) %>% filter(a %in% c(1, 2))
strip_attributes <- function(df, attr_names) {
  df[] <- lapply(df, function(x, attr_names) {
    lapply(attr_names, function(attr, x) {
      if(!is.null(attributes(x)[[attr]])) {
        attr(x, attr) <<- NULL
      }
      return(NULL)
    },x)
    
    return(x)
  }, attr_names)
  
  return(df)
}