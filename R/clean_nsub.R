#' cleans nsub output
#' @param path path to file
#' @param title keyword to determine title rows (eg. TABLE)
#' @param header keyword to determine header rows (eg DV)
#' @param overwrite whether to overwrite the original file. If FALSE, will add a '_clean'
#'    to existing fle name
#' @export
clean_nsub <- function(path, title = "TABLE", header = "DV", overwrite=T) {
  path <- normalizePath(path)
  temp <- readLines(path)
  gtable <- grep(pattern = "TABLE", x = temp)
  temp <- temp[-gtable]
  
  gr <- grep("DV", temp)
  temp <- temp[-gr[-1]]
  if(isTRUE(overwrite)) {
    message(paste0("writing to ", path))
    writeLines(temp, path)
    return(NULL)
  }
  path <- normalizePath(path)
  end <- stringr::str_extract(path, "\\..*$")
  path <- gsub("\\..*$", "", path)
  path <- paste0(path, "_clean", end)  
  message(paste0("writing to ", path))
  writeLines(temp, path)
  return(NULL)
}