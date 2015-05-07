#' peek at the results in a dplyr pipeline
#' @param df dataframe in pipeline
#' @param n number of rows to show
#' @details 
#' A wrapper around giving a \code{head} command but only as a side effect
#' so the original data frame is passed along to continue on for further
#' manipulation in the pipeline
#' @export
#' @examples 
#' library(dplyr)
#' Theoph %>% select(Subject, Time, conc) %>% peek %>% group_by(Subject) %>% 
#' summarize(cmax = max(conc)) 
#'
#' Theoph %>% select(Subject, Time, conc) %>% peek(message = "after select") %>% 
#'  group_by(Subject) %>% 
#' summarize(cmax = max(conc)) 
peek <- function(df, n = 5, message = NULL) {
  if (!is.null(message)) {
    message(message)
  } else {
  message("Taking a peek...")
  } 
  print(head(df, n))
  return(df)
}

