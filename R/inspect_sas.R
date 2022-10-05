#' Function to inspect all SAS files in a given directory
#' either displays files one at a time (default), or opens all files at once in separate windows
#' 
#' @param .dir directory containing SAS files
#' @param .cycle if TRUE, waits for user input to cycle through. if FALSE, opens all files simultaneously.
#' 
#' @export
inspect_sas <- function(.dir, .cycle=TRUE){
  ## find all files with given prefix in directory
  fls <- list.files(path=.dir, pattern="*.sas7bdat", full.names=FALSE)
  for(i in 1:length(fls)){
    ## read file i
    .x <- haven::read_sas(file.path(.dir, fls[[i]]))
    if(!.cycle){
      ## open all files, set window title to filename
      View(.x,fls[[i]])
    }else{
      ## cycle through files all in same window
      View(.x)
      print(fls[[i]])
      readline(prompt="Press [enter] to continue")
    }
  }
}