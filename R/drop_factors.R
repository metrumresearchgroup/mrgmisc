#' Drop factor level if not present in data
#' 
#' @description 
#' Returns a data.frame with factor levels removed if they do
#' not exist in the data. Columns that are not factors or all
#' levels are present will not be changed.
#' 
#' @param .df data.frame to be modified
#' 
#' @examples 
#' .df <- data.frame(
#'    X = factor(c("a", "b"), levels = c("a", "b", "c")),
#'    Y = 1:2)
#' 
#' drop_factors(.df)
#' 
#' @export
drop_factors <- function(.df) {
  
  drop_factors <- function(.df){
    purrr::map(
      .df, ~ {
        if(is.factor(.x)){
          .x <- forcats::fct_drop(.x)
        }
        .x
      }
    )
  }
  
}