#' factory to create read fort51 file
#' @param .names names of columns in the fort51 file
#' @examples 
#' rf51 <- rf51_factory(c(
#' "Iteration",
#' "ID",
#' "CL",
#' "V",
#' "LNTVCL",
#' "LNTVV",
#' "CLEGFR",
#' "nCL",
#' "nV"
#' ))
#' # could now do rf51("path/to/fort.51")
#' @export
rf51_factory <- function(.names) {
  return(function(.dir) {
    df <- data.table::fread(file.path(.dir, "fort.51"), na = ".") %>% tibble::as_data_frame()
    if (length(df) != length(.names)) {
      stop("names don't align, trying to set ",
           length(.names), " column names vs ", length(df), " columns in dataset")
    }
    names(df) <- .names 
    return(df)
  })
}