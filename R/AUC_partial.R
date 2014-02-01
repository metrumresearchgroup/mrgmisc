#' Calculate partial AUC
#' @param time column name for time
#' @param conc column name for conc
#' @param partialtime final time for partial AUC
AUC_partial <-function(time = "TIME", 
                       conc = "DV", 
                       partialtime = 28){
  
  
  time<- time
  conc <- conc

  
  time.points <- length(time)
  #check to make sure partial time legit option
  ###need to add warning
  
  partial.time <- length(time[time <= partialtime])
  aucp <- vector("numeric", partial.time-1)
  auci <-vector("numeric", time.points-1)
  

  for(i in 1:(partial.time-1)){
    aucp[i]<-(conc[i]+conc[i+1])*(time[i+1]-time[i])/2
  }
  
  #calculate the starting part of AUC
  auc.start <-0
  if(time[1]!=0) auc.start <-time[1]*conc[1]/2

  AUC.partial <- sum(aucp) + auc.start

  return(setNames(AUC.partial, paste0("AUC.partial", time[1], "-", partialtime)))

  
}
