#' Calculate AUCt-inf
#' @param time column name for time
#' @param conc column name for conc
#' @export
AUC_inf <-function(time = "TIME", 
                   conc = "DV"){
  
  
  time<- time
  conc <- conc
  
  time.points <- length(time)
  #check to make sure partial time legit option
  ###need to add warning
  
  
  auci <-vector("numeric", time.points-1)
  
  
  #  AUCinf is calculated based on 3 parts: the beginning, the middle, and the extrapolated
  #  calculate the middle part of AUC
  for(i in 1:(time.points-1)){
    auci[i]<-(conc[i]+conc[i+1])*(time[i+1]-time[i])/2
  }
  
  
  #calculate the starting part of AUC
  auc.start <-0
  #if(time[1]!=0) auc.start <-time[1]*conc[1]/2
  
  #calculate the ending part of AUC
  #assuming to compare the last 3, 4 and 5 time points for regression.
  last <-c(3, 4, 5)
  start<- time.points - last
  auc.end <-vector("numeric", length(last))
  lambda_z <-vector("numeric", length(last))
  adj.r.squared <-vector("numeric", length(last))
  for(j in 1:3){
    t<-time[start[j]:time.points]
    con <- conc[start[j]:time.points]
    xt <-lm(con~t)
    lambda_z[j]<- as.numeric(xt$coef[2])
    adj.r.squared[j]<-summary(xt)$adj.r.squared
    auc.end[j]<- con[3]/lambda_z[j]*(-1)
  }
  #Calculate the final parameters.
  best.fit.pointer <-which(adj.r.squared==max(adj.r.squared))
  AUC.inf <-sum(auci)+ auc.start + auc.end[best.fit.pointer]
  AUC.last <-sum(auci)+ auc.start
  
  
  return(setNames(AUC.inf, paste0("AUC0_inf")))
  
  
}