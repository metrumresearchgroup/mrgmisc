#' replace symbols or other character flags
#' @param x vector to replace
#' @param flag dataframe with first column being the flag and second the replacement value
#' @param nonflag what to do with non-flagged elements
#' @export
replace_char_flags <- function(x, 
                         flag,
                         nonflag = NULL,
                         as_numeric=TRUE) {
  if(!is.null(nonflag)) {x[!(x %in% flag[[1]])] <- nonflag}
  tempx <- x[x %in% flag[[1]]]
  matches <- match(tempx, flag[[1]])
  x[x %in% flag[[1]]] <- unlist(lapply(matches, function(x) flag[[2]][x]))
  if(as_numeric) x <- as_numeric(x)
  x
}