#' peek at the results in a dplyr pipeline
#' @param df dataframe in pipeline
#' @param n number of rows to show
#' @details 
#' A wrapper around giving a \code{head} and \code{tail} command
#'  but only as a side effect so the original data frame is passed 
#'  along to continue on for furthermanipulation in the pipeline
#' @export
#' @examples 
#' library(dplyr)
#' Theoph %>% select(Subject, Time, conc) %>% peek %>% group_by(Subject) %>% 
#' summarize(cmax = max(conc)) 
#'
#' Theoph %>% select(Subject, Time, conc) %>% peek(message = "after select") %>% 
#'  group_by(Subject) %>% 
#' summarize(cmax = max(conc)) 
#' 
#' # nice for saving full objects but still seeing their output
#' cmax_theoph <- Theoph %>% select(Subject, Time, conc) %>% 
#' peek(message = "after select") %>% 
#'  group_by(Subject) %>% 
#' summarize(cmax = max(conc)) %>% peek 
#' cmax_theoph # still saves full output
peek <- function(df, n = 5, message = NULL) {
  if (!is.null(message)) {
    message(message)
  } else {
  message("Taking a peek...")
  } 
  print(head(df, n))
  message("...................")
  print(tail(df, n))
  return(df)
}

