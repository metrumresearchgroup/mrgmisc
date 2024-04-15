#' Add hash character to start of each line
#' 
#' @description 
#' The given expression will be returned, with a hash character at the start of each line.
#' 
#' @param x string
#' @param char hash character string
#' 
#' @export
hash <- function(x,char='#'){
  con <- file()
  sink(con)
  result <- try(x)
  if(!inherits(result,"try-error"))print(result)
  comments <- paste(char,readLines(con))
  sink(NULL)
  close(con)
  writeLines(comments)	
}
  