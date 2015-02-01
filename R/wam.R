#' internal WAM function
#' @param k number of covariate thetas
#' @param p total number of parameters in model (theta, omega, sigma)
#' @param n number of observations
#' @param theta vector of covariate parameter values
#' @param cov variance-covariance matrix for covariate parameters
#' @return data.frame
#' @details
#' returns model rank, the LRT, SBC, which covariates selected
#' for internal use with wam()
calculate_wam <- function(k, p, n, theta, cov)
{
  kk    <- 2^((k-1):0)
  maker <- paste( "rep(rep(c(0,1),rep(" , kk , ",2))," , rev(kk) , ")" ,sep="")
  x     <- apply(matrix(maker),1,function(x) eval(parse(text=x)))
  kkk   <- dim(x)[1]
  # i = 1
  ireg <- rep(1,k)
  idel <- 1:k
  c2     <- cov[idel,idel]
  theta2 <- t(theta[idel])
  lrt    <- t(theta2)%*%solve(c2)%*%theta2
  s      <- p-length(idel)
  sbc    <- -0.5*(lrt+s*log(n))
  results <- data.frame( i    = 1:kkk,
                         ireg = rep(0,kkk),
                         lrt  = rep(0,kkk),
                         sbc  = rep(0,kkk) )
  results[1,2:4] <- c(paste(ireg,collapse=","),lrt,sbc)
  for( i in 2:(kkk-1) )
  {
    ireg <- (1:k)[x[i,]>0]
    idel <- (1:k)[x[i,]==0]
    c2     <- cov[idel,idel]
    theta2 <- t(theta[idel])
    lrt    <- t(theta2)%*%solve(c2)%*%theta2
    s      <- p-length(idel)
    sbc    <- -0.5*(lrt+s*log(n))
    results[i,2:4] <- c(paste(ireg,collapse=","),lrt,sbc)
  }
  # i = kkk
  ireg <- (1:k)
  lrt    <- 0
  sbc    <- -0.5*(lrt+p*log(n))
  results[kkk,2:4] <- c(paste(ireg,collapse=","),lrt,sbc)
  results <- results[order(-as.numeric(results$sbc)),]
  results[,"i"] <- 1:kkk
  
  results  
}  

#' wam function
#' @param ncovtheta number of covariate thetas
#' @param cov_theta_nums <- vector of theta numbers for covariate thetas
#' @param npar number of parameters in model (theta, omega, sigma)
#' @param nobs number of observations
#' @param run_num run number
#' @param dir directory where .cov and .ext files located
#' @details
#' for runnum it is recommended to pass as a string or leading 0's will be stripped
#' @export
wam <- function (ncovtheta, cov_theta_nums, npar, nobs, run_num, dir=NULL) {
  # TOADD: check that dir has trailing '/' 
  # TOADD: check runno is a string, if numeric warn leading 0's will be stripped
  cov_thetas <- sprintf(paste0("THETA", "%d"), cov_theta_nums)
  theta_nums <- as.numeric(gsub("THETA", "", cov_thetas))
  
  
  theta_cov_matrix<-read.table(file=paste0(dir, "run",run_num,".cov"),
                 skip=1,header=1,row.names=1)
  theta_param_values<-read.table(file=paste0(dir, "run",run_num,".ext"),
                 skip=1,header=1)
  
  
  cov <- as.matrix(theta_cov_matrix[c(row.names(theta_cov_matrix) %in% cov_thetas), 
                      c(names(theta_cov_matrix) %in% cov_thetas)])
  
  theta<- theta_param_values[theta_param_values$ITERATION==-1000000000, 
                             names(theta_param_values) %in% cov_thetas]
  
  results <- calculate_wam(k=length(cov_thetas), p=npar, n=nobs, theta, cov)
  results$included_thetas<-lapply(strsplit(results$ireg,","),
                                  function(x) paste(theta_nums[as.integer(x)],collapse=","))
  
  results$lrt <- round(as.numeric(results$lrt), 2)
  results$sbc <- round(as.numeric(results$sbc), 2)
  return(results)
}
