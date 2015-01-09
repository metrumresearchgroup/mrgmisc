#'calculate basic NCA parameters t1/2 Cl, Cmax etc
#' @param TIME vector of times
#' @param DV vector of observations (concentrations)
#' @param DOSE vector or single value of Dose given
#' @param last_times vector of numbers of time points to evaluate for 
#'    AUCinf extrapolation default 3-5
#' @export
NCA <-function(TIME = "TIME", 
               DV = "DV", 
               DOSE = "DOSE", 
               last_times = c(3, 4, 5)) 
{
  #TODO change defaults for last_times to detect all times after cmax
  #TODO clean up how results are returned
  time <- TIME
  conc <- DV
  dose <- DOSE
  time.points <- length(time)
  aucp <- vector("numeric", partial.time - 1)
  auci <- vector("numeric", time.points - 1)
  for (i in 1:(time.points - 1)) {
    auci[i] <- (conc[i] + conc[i + 1]) * (time[i + 1] - time[i])/2
  }
  auc.start <- 0
  last <- last_times
  start <- time.points - last
  auc.end <- vector("numeric", length(last))
  lambda_z <- vector("numeric", length(last))
  adj.r.squared <- vector("numeric", length(last))
  for (j in 1:length(last)) {
    t <- time[start[j]:time.points]
    con <- conc[start[j]:time.points]
    xt <- lm(log(con) ~ t)
    lambda_z[j] <- as.numeric(xt$coef[2])
    adj.r.squared[j] <- summary(xt)$adj.r.squared
    auc.end[j] <- con[length(con)]/lambda_z[j] * (-1)
  }
  if(length(which(adj.r.squared == max(adj.r.squared))) > 1) {
    best.fit.pointer <- min(which(adj.r.squared == max(adj.r.squared))) 
  } else {
    best.fit.pointer <-   which(adj.r.squared == max(adj.r.squared))
  }
  AUC.inf <- sum(auci) + auc.start + auc.end[best.fit.pointer]
  AUC.last <- sum(auci) + auc.start
  Extra_percent <- (AUC.inf - AUC.last)/AUC.last * 100
  lambda_z.final <- lambda_z[best.fit.pointer] * (-1)
  Num_points_lambda_z <- last[best.fit.pointer]
  half.life <- 0.693/lambda_z.final
  cl <- dose[1]/AUC.inf
  VoD <- lambda_z.final/cl
  Cmax <- max(conc)
  Tmax <- time[which(conc == Cmax)]
  final <- data.frame(Cmax, Tmax, AUC.last, AUC.inf,
                   Extra_percent, adj.r.squared[best.fit.pointer], lambda_z.final, half.life, cl, Num_points_lambda_z)
  
  names(final) <- c("Cmax", "Tmax", "AUClast", "AUCinf", "Extra_percent", 
                          "Adj.R.Sq", "Lambda_z", "half_life", "cl", "Num_points_lambda_z")
  return(final)
}
