#' count number of rows in a file without needing to read file into R
#' @param file file path to be read in
#' @details
#' uses wc -l system call to count rows
#' @name file_rows
#' @export
file_rows <- function(file) {
  # add check for unix vs windows
  if (file.exists(file)) {
    sysCmd <- paste("wc -l", file)
    rowCount <- system(sysCmd, intern = T)
    rowCount <- sub("^\\s", "", rowCount)
    as.numeric(strsplit(rowCount, "\\s")[[1]][1])
  }
}