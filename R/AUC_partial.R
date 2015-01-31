#' Calculate partial AUC
#' @param time time column
#' @param conc concentration column
#' @param range time range for pauc calculation
#' @details 
#' default range is 0 to tmax
#' is recommended to be used alongside dplyr for ease of calculation
#' @examples
#' \dontrun{
#' df %>% group_by(ID) %<% summarize(pAUC0_10 = auc_partial(TIME, DV, c(0,10)))
#' }
#' @export
auc_partial <-function(time, 
                       conc, 
                       range = c(0, max(time))
                       ){
  tfirst <- range[1]
  tlast <- range[2]
  
  time<- time
  conc <- conc[time >= tfirst]
  time <- time[time >= tfirst]
  partial.time <- length(time[time <= tlast])
  
  time.points <- length(time)
  #check to make sure partial time legit option
  ###need to add warning
  
  aucp <- vector("numeric", partial.time-1)
  

  for(i in 1:(partial.time-1)){
    aucp[i]<-(conc[i]+conc[i+1])*(time[i+1]-time[i])/2
  }
  
  #calculate the starting part of AUC
  auc.start <-0
  #if(time[1]!=0) auc.start <-time[1]*conc[1]/2 # need to check validity of this start calculation

  AUC.partial <- sum(aucp) + auc.start

  return(setNames(AUC.partial, paste0("pAUC", tfirst, "-", tlast)))

  
}

#' @rdname auc_partial
#' @export
AUC_partial <- function(...) {
  warning("AUC_partial is depreciated and will be removed in future versions,
          please use auc_partial (all lowercase)")
  auc_partial(...)
}
