#' Write a csv file compatible with nonmem
#' @details 
#' nonmem uses '.' for NA values, does not like quotes in column names
#' and does not handle row names, so these are all presets
#' @param x dataframe to be written to csv
#' @param file character string naming a file or connection open for writing.
#' @param sep field string separator, defaults to comma (",")
#' @param row.names logical value whether to include row names
#' @param na value for NA
#' @param quote whether character or factor columns should be surrounded by double quotes
#' @param ... remaining arguments passed to data.table::fwrite
#' @export
write_nonmem <- function(x, file, sep = ",", row.names=FALSE, na = ".", quote = FALSE, ...) {
  data.table::fwrite(x, file, quote = quote, row.names = row.names, sep = sep, na = na, ...)
}