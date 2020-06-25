#' wrapper to capture groups from a grouped data frame
#' @param df frame to determine groups
#' @examples
#' gTheoph <- dplyr::group_by(Theoph, Subject)
#' capture_groups(gTheoph)
#' @export
capture_groups <- function(df) {
  grps <- dplyr::groups(df)
  return(grps)
}

#' set groups 
#' @param df data frame 
#' @param groups list of groups to pass to group_by_
#' @details
#' useful in tandem with `capture_groups` when concerned about
#' modification of groups by a function, for example when summarizing with dplyr
#' Can easily capture and reset groups to maintain original grouping
#' @examples
#' gTheoph <- dplyr::group_by(Theoph, Subject)
#' grps <- capture_groups(gTheoph) # capture subject
#' theoph_cmax <- summarize(gTheoph, cmax = max(conc)) # lose Subject grouping
#' theoph_cmax <- set_groups(theoph_cmax, grps) # resets the original "Subject" grouping
#' @export
set_groups <- function(df, groups) {
  if(!is.null(groups)) df <- dplyr::group_by(df, !!!rlang::syms(groups))
  return(df)
}