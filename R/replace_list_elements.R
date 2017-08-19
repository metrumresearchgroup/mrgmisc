#' replace list elements by name
#' @param .list original list
#' @param replacement replacement list
#' @param add add replacement values if they don't exist, default = TRUE
#' @details
#' Finds and replaces list elements by name and throws an error if an 
#'    element is not available in the original list.
#' @examples 
#' olist <- list(dv = "dv", idv = "idv", notreplace = "somethingelse")
#' replacement <- list(dv = "conc")
#' combined_list <- replace_list_elements(olist, replacement)
#' @export
replace_list_elements <- function(.list, replacement, add = TRUE) {
  missing <- which(!names(replacement) %in% names(.list))
  if(length(missing) != 0) {
    if (add) {
      warning(paste("Nothing named: ", paste(names(replacement)[missing], collapse= ", ", "found to replace")))
    } else {
      stop(paste("Nothing named: ", paste(names(replacement)[missing], collapse= ", ", "found to replace")))
    }
  }
  return(utils::modifyList(.list, replacement))
}