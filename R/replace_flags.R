#' replace symbols or other character flags
#' @param x vector to replace
#' @param flag dataframe with first column being the flag and second the replacement value
#' @param nonflag what to do with non-flagged elements
#' @param as_numeric whether to return column as numeric
#' @examples 
#' df <- data.frame(ID = 1, DV = c(1, "BQL", ".", 5))
#' rflags <- data.frame(flag = c("BQL", "."), replacement = -99)
#' df$DVR <- replace_char_flags(df$DV, rflags)
#' @export
replace_char_flags <- function(x, 
                         flag,
                         nonflag = NULL,
                         as_numeric=TRUE) {
  if(is.factor(x)) {
    message("converting factor to character strings before replacement")
    x <- as.character(x)
  }
  if(is.factor(flag[[2]])) {
    warning("replacement flags will be coerced to character  to assure replacement")
    flag[[2]] <- as.character(flag[[2]])
  }
  if(!is.null(nonflag)) {x[!(x %in% flag[[1]])] <- nonflag}
  tempx <- x[x %in% flag[[1]]]
  matches <- match(tempx, flag[[1]])
  x[x %in% flag[[1]]] <- unlist(lapply(matches, function(x) flag[[2]][x]))
  if(as_numeric) x <- as_numeric(x)
  x
}