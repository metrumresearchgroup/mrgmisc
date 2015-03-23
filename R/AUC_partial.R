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
#' @examples
#' \dontrun{
#' library(PKPDdatasets)
#' library(dplyr)
#' df <- capitalize_names(sd_oral_richpk)
#' df2 <- rename(sd_oral_richpk, idv = Time, dv = Conc)
#' df %>% group_by(ID) %>% summarize(pAUC0_10 = auc_partial(TIME, DV, c(0,10)))
#' df2 %>% group_by(ID) %>% summarize(auc0_tlast = auc_partial(idv, dv, c(0, Inf))) # gives auc 0 to tlast
#' }
#' @export
auc_partial <-function(idv, 
                       dv, 
                       range = c(0, Inf)
                       ){
  if(!is.numeric(idv) || !is.numeric(dv) || !is.numeric(range)) {
    stop("time, dv/dv, and range inputs must all be numeric")
  }
  tfirst <- range[1]
  tlast <- range[2]
  
  idv<- idv
  dv <- dv[idv >= tfirst]
  idv <- idv[idv >= tfirst]
  if(length(idv) == 0) {
    warning("no observations in requested time range")
    return(setNames(NA, paste0("pAUC", tfirst, "-", tlast)))
  }
  partial.time <- length(idv[idv <= tlast])
  
  time.points <- length(idv)
  #check to make sure partial time legit option
  ###need to add warning
  
  aucp <- vector("numeric", partial.time-1)
  

  for(i in 1:(partial.time-1)){
    aucp[i]<-(dv[i]+dv[i+1])*(idv[i+1]-idv[i])/2
  }
  
  #calculate the starting part of AUC
  auc.start <-0
  #if(time[1]!=0) auc.start <-time[1]*dv[1]/2 # need to check validity of this start calculation

  AUC.partial <- sum(aucp) + auc.start

  return(setNames(AUC.partial, paste0("pAUC", tfirst, "-", tlast)))

  
}

#' @rdname auc_partial
#' @param ... args to pass to auc_partial
#' @export
AUC_partial <- function(...) {
  warning("AUC_partial is depreciated and will be removed in future versions,
          please use auc_partial (all lowercase)")
  auc_partial(...)
}
