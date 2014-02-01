#'calculate basic NCA parameters t1/2 Cl, Cmax etc
#' @param pk dataframe
#' @param partialtime partial AUC time value
NCA <-function(pk, partialtime = 28){
  #TODO:needs to be updated to pass in user-defined time, conc, and dose column names
  # 
  time<- pk$TADd
  conc <- pk$DV
  dose <- pk$DOSE
  
  time.points <- length(time)
  #check to make sure partial time legit option
  ###need to add warning
  
  partial.time <- length(time[time <= partialtime])
  aucp <- vector("numeric", partial.time-1)
  auci <-vector("numeric", time.points-1)
  
  
  #AUCinf is calculated based on 3 parts: the beginning, the middle, and the extrapolated
  #calculate the middle part of AUC
  for(i in 1:(time.points-1)){
    auci[i]<-(conc[i]+conc[i+1])*(time[i+1]-time[i])/2
  }
  
  # partial AUC middle
  for(i in 1:(partial.time-1)){
    aucp[i]<-(conc[i]+conc[i+1])*(time[i+1]-time[i])/2
  }
  
  #calculate the starting part of AUC
  auc.start <-0
  if(time[1]!=0) auc.start <-time[1]*conc[1]/2
  
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
  AUC.partial <- sum(aucp) + auc.start
  Extra_percent <-(AUC.inf-AUC.last)/AUC.last*100
  lambda_z.final <- lambda_z[best.fit.pointer]*(-1)
  half.life <-0.693/lambda_z.final
  cl <- dose[1]/AUC.inf
  VoD <- lambda_z.final/cl
  Cmax <-max(conc)
  Tmax <-time[which(conc==Cmax)]
  #deleting the interim values.
  rm(time); rm(conc); rm(dose)
  #Return the results in a data frame.
  return.list <-c(Cmax, Tmax, AUC.last, AUC.inf, AUC.partial, Extra_percent,
                  #lambda_z.final, half.life, cl, VoD, 
                  adj.r.squared[best.fit.pointer])
  return.list <-matrix(return.list, nrow=1)
  return.list <- data.frame(X1=return.list)
  names(return.list)<-c("Cmax", "Tmax", "AUClast", "AUCinf", paste0("AUC.partial", partialtime), "Extra_percent",
                        #"k", "Half.life", "Cl", "V.Distr", 
                        "Adj.R.Sq")
  return(return.list)
}
