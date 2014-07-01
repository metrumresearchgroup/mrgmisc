#' show description output from .mod file
#' @name pirana_description
#' @param filename name of .mod file 
#' @details
#' return line containing description file
#' @return string
#' @seealso nm_notes
#' @export
nm_description <- function(filename) {
  listfile <- scan(filename, sep = "\n", what = character(), 
                   quiet = TRUE)
  desc <- grep("Description:", listfile)
      desc <- listfile[(desc - 1):(desc)]
    desc <- substring(desc, 2)
  
  return(desc)
}

#' show notes output from .mod file
#' @name nm_notes
#' @param filename name of .mod file
#' @details 
#' returns all lines between ;NOTES and ;ENDNOTES in .mod file 
#' if filename not in current wd, need to give path as well
#' @examples
#' \dontrun{
#' nm_notes("run102.mod")
#' }
#' @seealso nm_description
#' @return string
#' @export
nm_notes <- function(filename) {
  listfile <- scan(filename, sep = "\n", what = character(), 
                   quiet = TRUE)
  begin <- grep("NOTES:", listfile)
  end <- grep("ENDNOTES", listfile)
  notes <- listfile[(begin):(end)]
  output <- stringr::str_split(unlist(notes), pattern = ";")
  output <- lapply(output, function(x) x[which(x != "")])
  return(paste(output, collapse = ''))
}