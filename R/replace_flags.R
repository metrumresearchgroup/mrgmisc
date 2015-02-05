#' replace symbols or other character flags
#' @param x vector to replace
#' @param flag vector with names being the flag and value the replacement
#' @param nonflag what to do with non-flagged elements
#' @export
replace_char_flags <- function(x, 
                         flag= c("*" = 1, "-" = 2),
                         nonflag = NULL) {
  if(!is.null(nonflag)) {x[!(x %in% names(flag))] <- nonflag}
  tempx <- x[x %in% names(flag)]
  matches <- match(tempx, names(flag))
  x[x %in% names(flag)] <- unlist(lapply(matches, function(x) flag[x]))
  x
}