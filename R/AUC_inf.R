#' Calculate AUCt-inf
#' @param time column name for time
#' @param conc column name for conc
#' @param last_points vector of amount of points in terminal phase that will be evaluated for extrapolation
#' @param AUCinf_only default TRUE only return single value for AUCinf
#' @details
#' last_points defaults to 3, 4, 5
#' AUCinf_only for use in dplyr/plyr or other summarizations that can only handle a vector length 1 outputs
#' when AUCinf_only is false also return %extrapolated, num points used for lambda calc, adjusted Rsquared value 
#' @export
AUC_inf <-function(time = "TIME", 
                   conc = "DV",
                   last_points = c(3, 4, 5),
                   AUCinf_only = TRUE){
  #checks to add
  #TODO: add check that lambda_z is positive and fail gracefully if not
  #TODO: clean up return data.frame/vector (its uuuugly now)
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
  last <- last_points
  start<- time.points - last + 1
  # need extra + 1 in start as time.points-last will give one more time than requested otherwise
  # ie if want last 3 time points from 16 total time.points - last will say give 13-16
  # so that would give 13, 14, 15, 16
  auc.end <-vector("numeric", length(last))
  lambda_z <-vector("numeric", length(last))
  adj.r.squared <-vector("numeric", length(last))
  for(j in 1:length(last)){
    t<-time[start[j]:time.points]
    con <- conc[start[j]:time.points]
    xt <-lm(log(con)~t) # log-linear terminal phase calculation for k
    lambda_z[j]<- as.numeric(xt$coef[2])
    adj.r.squared[j]<-summary(xt)$adj.r.squared
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
  
  AUC.inf <- AUC.last + conc[length(conc)]/lambda_z.final

  if(AUCinf_only) return(setNames(AUC.inf, paste0("AUC0_inf")))

  Extra_percent <- (AUC.inf - AUC.last)/AUC.last * 100
  Num_points_lambda_z <- last[best.fit.pointer]
  
  return.list <- c(AUC.last, 
                   AUC.inf, 
                   Extra_percent, 
                   adj.r.squared[best.fit.pointer], 
                   lambda_z.final, 
                   Num_points_lambda_z)
  return.list <- matrix(return.list, nrow = 1)
  return.list <- data.frame(X1 = return.list)
  names(return.list) <- c("AUClast", 
                          "AUCinf", 
                          "Extra_percent", 
                          "Adj.R.Sq", 
                          "Lambda_z", 
                          "Num_points_lambda_z")
  return(return.list)
  
  
}