#' basic curve stripping to get initial estimates
#' @param TIME column for time
#' @param DV column for DV (concentration) values
#' @param number_terminal_points number of points in terminal phase
#' @param oral whether data is oral (instead of IV)
#' @export
#' @details
#' for oral stripping, if multiple cmax values found per ID, will use the first
#' @examples 
#' \dontrun{
#' strip_curves(df$TIME, df$DV, DOSE =1000, 5, oral=TRUE)
#' df %>% group_by(ID) %>% do(data.frame(strip_curves(.$TIME, .$DV, 1000, 5, TRUE)))
#' }
strip_curves <- function(TIME, DV, DOSE, number_terminal_points, oral= FALSE) {
  time<- TIME
  conc <- DV
  
  num_time_points <- length(time)
  #check to make sure partial time legit option
  ###need to add warning
  start_terminal_points <- num_time_points - number_terminal_points + 1
  
  
  terminal_time<-time[start_terminal_points:num_time_points]
  terminal_conc <- conc[start_terminal_points:num_time_points]
  terminal_xt <-lm(log(terminal_conc)~terminal_time) # log-linear terminal phase calculation for k
  terminal_lambda_z<- as.numeric(terminal_xt$coef[2])
  
  predicted_terminal_portion <- exp(predict(terminal_xt, data.frame(terminal_time = time)))
  remaining_alpha_conc <- conc- predicted_terminal_portion
  
  alpha_index <- 1:(num_time_points - number_terminal_points)
  if(oral) {
    # cmax to last point not in terminal phase 
    alpha_index <- which(conc==max(conc))[1]:(num_time_points - number_terminal_points)
  }
  alpha_conc <- remaining_alpha_conc[alpha_index]
  alpha_t <- time[alpha_index]
  alpha_xt <- lm(log(alpha_conc)~alpha_t)
  
  A <- exp(unlist(alpha_xt$coef[1]))
  B <- exp(unlist(terminal_xt$coef[1]))
  alpha <- -as.numeric(unlist(alpha_xt$coef[2]))
  beta <- -as.numeric(unlist(terminal_xt$coef[2]))
  
  k21 <- (A*beta + B*alpha)/(A+B)
  kel <- alpha*beta/k21
  k12 <- alpha + beta - k21 - kel
  
  Vc <- DOSE/max(conc)
  Vp <-  k12/k21*Vc
  Q <- k12*Vc
  CL <- kel*Vc
  
  initial_estimates <- data.frame("Vc" = Vc, "Vp" = Vp, "Q" = Q, "CL" = CL)
  row.names(initial_estimates) <- NULL
  return(round(initial_estimates, 2))
}