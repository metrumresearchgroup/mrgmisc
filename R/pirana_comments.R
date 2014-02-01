#' show pirana comments from .mod file
#' @name pirana_comments
#' @param filename name of .mod file 

pirana_comments <- function(filename) {
  listfile <- scan(filename, sep = "\n", what = character(), 
                   quiet = TRUE)
  desc <- grep("Description:", listfile)
      desc <- listfile[(desc - 1):(desc)]
    desc <- substring(desc, 2)
  
  return(desc)
}