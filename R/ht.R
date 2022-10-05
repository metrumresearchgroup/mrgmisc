#' Display top and bottom of data frame
#' 
#' @param .df data.frame to display
#' @param .n number of rows to display from each end of data frame
#' 
#' @examples 
#' 
#' # View the top 4 and bottom 4 rows of a dataframe
#' ht(Theoph, 4)
#' 
#' @export
ht <- function(.df,.n=4) rbind(head(.df,.n),tail(.df,.n))