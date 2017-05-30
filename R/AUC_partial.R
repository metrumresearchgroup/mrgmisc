#' Calculate partial AUC
#' @param idv independent variable (such as time)
#' @param dv dependent variable (such as concentration)
#' @param range time range for pauc calculation
#' @details 
#' default range is 0 to tmax
#' is recommended to be used alongside dplyr for ease of calculation
#' if an individual does not have any value within the specified range
#' a warning will be issued and an NA value will be returned. This is important
#' if some individuals dropped out early and do not have all observations other
#' individuals have.
#' @seealso \code{\link{s_pauc}}
#' @examples
#' library(PKPDmisc)
#' library(dplyr, quiet = TRUE)
#' df <- capitalize_names(sd_oral_richpk) 
#' head(df)
#' df %>% group_by(ID) %>% 
#' summarize(pAUC0_10 = auc_partial(TIME, CONC, c(0,10)))
#' 
#' df %>% group_by(ID) %>% 
#' summarize(auc0_tlast = auc_partial(TIME, CONC)) 
#' @export
auc_partial <-function(idv, 
                       dv, 
                       range = c(0, Inf)
                       ){
  if(!is.numeric(idv) || !is.numeric(dv) || !is.numeric(range)) {
    stop("idv, dv, and range inputs must all be numeric")
  }
  if(length(idv) != length(dv)) {
    stop("idv and dv columns must be equal lengths, maybe you filtered NA's only in one?")
  }
  if (length(range) != 2 || range[1] > range[2]) {
    stop("range must be a numeric vector containing a low then high(er) value")
  }
  aucp <- auc_partial_cpp(idv, dv, range)
  tlast <- ifelse(range[2] == Inf, "tlast", range[2])
  # dplyr select NSE does not always like "-" so using "_" 
  return(setNames(aucp, paste0("pAUC", range[1], "_", tlast)))
}
