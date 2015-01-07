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