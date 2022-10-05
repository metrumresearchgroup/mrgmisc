#' Estimate elimination half-life from 2-compartment model in hours
#' 
#' @param cl clearance estimate (L/hr)
#' @param vc central volume estimate (L)
#' @param vp peripheral volume estimate (L)
#' @param q intercompartmental clearance estimate (L/hr)
#' 
#' @export
half_life <- function(cl,vc,vp,q){
  K10 <- cl/vc
  K12 <- q/vc
  K21 <- q/vp
  L2 = ((K10+K12+K21)-((K10+K12+K21)**2-4*K10*K21)**0.5)/2
  return(0.693/L2)
}