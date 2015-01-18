#' cmax summary function
#' @param df. data frame. Can pass in grouped data frame to calculate by group
#' @param DV. dependent variable column to calculate cmax on 
#' @param na.rm. boolean whether to remove NA values when returning max value
#'    default = TRUE
#' @param name. string name of output column. Default to `CMAX`
#' @param carry. boolean whether to carry other columns. Defaults to FALSE
#' @param check_duplicates. boolean whether to check for duplicates in each group 
#'    after calculation 
#' @details
#' one trick for better performance is to turn off check duplicates
#' the function is already quite fast by using dplyr under the hood, but
#' if used in a simulation context where it may be used over and over, turning
#' this off will result in slightly better performance as it does not perform
#' the additional calculations to check for duplicate entries. 
#' 
#' The duplicates warning is due to the nature of cmax calculations often being used
#' in subsequent steps, either to see the time that cmax was reached, or for 
#' further calculations. Duplicate values per group (eg ID) can lead to odd behavior
#' in later calculations, thus duplicates are checked for and the user is warned by default.
#' @examples
#' \dontrun{
#' library(PKPDdatasets)
#' pr_id <- dplyr::group_by(pain_relief, ID)
#' s_cmax(pr_id, "CONC") 
#' s_cmax(pr_id, "CONC", carry=T) 
#' s_cmax(pr_id, "CONC", name = "max_conc") 
#' }
#' @export
s_cmax <- function(df, DV, na.rm=T, name = "CMAX", carry=FALSE, 
                   check_duplicates=TRUE) {
  if(isTRUE(na.rm) && any(is.na(df[[DV]]))) message("removing NAs")
  dots = list(lazyeval::interp(~ max(var, na.rm = na.rm), 
                               var = as.name(DV)))
  s_df <- df %>% 
    dplyr::summarize_(.dots = setNames(dots, paste0(name)))
  
  if(isTRUE(carry)) {
  s_df <- suppressMessages(dplyr::inner_join(s_df, df))  %>% 
    filter_(.dots = lazyeval::interp(~CMAX == CONC,
                                     CMAX = as.name(name),
                                     CONC = as.name(DV)))

  }
  
  if(isTRUE(check_duplicates)) {
    check_duplicates <- set_groups(s_df, grps) %>% summarize(n = n())
    if(any(check_duplicates$n > 1)) {
      warning("More than one max value found per group, proceed with caution")
    }
  }

  return(s_df)
}