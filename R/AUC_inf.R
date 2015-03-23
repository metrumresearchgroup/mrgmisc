#' Calculate AUCt-inf
#' @param idv column name for independent variable such as time
#' @param dv column name for dependent variable such as concentration
#' @param last_points vector of amount of points in terminal phase that will be evaluated for extrapolation
#' @param na.rm remove any NAs from the idv/dv column before calculating AUC
#' @details
#' last_points defaults to 3, 4, 5
#' see auc_partial for other details
#' @export
auc_inf <-function(idv, 
                   dv,
                   last_points = c(3, 4, 5),
                   na.rm = TRUE){
  #checks to add
  #TODO: add check that lambda_z is positive and fail gracefully if not
  #TODO: clean up return data.frame/vector (its uuuugly now)
  if(!na.rm) {
    idv<- idv
    dv <- dv 
  } else {
    if(all(is.na(dv)) | all(is.na(idv))) {
      return(setNames(NA, paste0("AUC0_inf")))
      
    }
    if(any(is.na(dv))) {warning("removing at least 1 NA value")}
    idv<- idv[!is.na(dv)]
    dv <- dv[!is.na(dv)]
  }
  if(isTRUE(all(dv ==0))) {
    return(setNames(0, paste0("AUC0_inf")))
  } 
  time.points <- length(idv)
  #check to make sure partial idv legit option
  ###need to add warning
  
  
  auci <-vector("numeric", time.points-1)
  
  
  #  AUCinf is calculated based on 3 parts: the beginning, the middle, and the extrapolated
  #  calculate the middle part of AUC
  for(i in 1:(time.points-1)){
    auci[i]<-(dv[i]+dv[i+1])*(idv[i+1]-idv[i])/2
  }
  
  
  #calculate the starting part of AUC
  auc.start <-0
  #if(idv[1]!=0) auc.start <-idv[1]*dv[1]/2
  
  #calculate the ending part of AUC
  #assuming to compare the last 3, 4 and 5 idv points for regression.
  last <- last_points
  start<- time.points - last + 1
  # need extra + 1 in start as time.points-last will give one more time than requested otherwise
  # ie if want last 3 time points from 16 total time.points - last will say give 13-16
  # so that would give 13, 14, 15, 16
  auc.end <-vector("numeric", length(last))
  lambda_z <-vector("numeric", length(last))
  adj.r.squared <-vector("numeric", length(last))
  for(j in 1:length(last)){
    t<-idv[start[j]:time.points]
    con <- dv[start[j]:time.points]
    if(isTRUE(all(con == 0))) {
      lambda_z[j]<- 0
      adj.r.squared[j]<- 0
    } else{
      xt <-lm(log(con)~t) # log-linear terminal phase calculation for k
      lambda_z[j]<- as.numeric(xt$coef[2])
      adj.r.squared[j]<-summary(xt)$adj.r.squared
    }

  }
  ### Calculate the final parameters ------------------------------
  #if multiple lm give same best fit will choose the one with the least number of points
  if(length(which(adj.r.squared == max(adj.r.squared))) > 1) {
    best.fit.pointer <- min(which(adj.r.squared == max(adj.r.squared))) 
  } else {
    best.fit.pointer <-   which(adj.r.squared == max(adj.r.squared))
  }
  lambda_z.final <- lambda_z[best.fit.pointer] * (-1)
  AUC.last <- sum(auci) + auc.start 
  
  if(lambda_z.final == 0) {
    return(setNames(AUC.last, paste0("AUC0_inf")))
}
    
  AUC.inf <- AUC.last + dv[length(dv)]/lambda_z.final

  return(setNames(AUC.inf, paste0("AUC0_inf")))

}

#' @rdname auc_inf
#' @param ... args to pass to auc_inf
#' @export
AUC_inf <- function(...) {
  warning("AUC_inf is depreciated and will be removed in future versions,
          please use auc_inf (all lowercase)")
  auc_inf(...)
}