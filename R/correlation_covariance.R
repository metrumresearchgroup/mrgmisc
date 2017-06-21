#' correlation to covariance matrix
#' @param .cor_mat correlation matrix
#' @param .sd vector of standard deviations 
#' @export
cor_to_cov <- function(.cor_mat, .sd) {
  dc <- diag(.cor_mat)
  ds <- diag(sd)
  Mo <- ds %*% .cor_mat %*% ds
  Mo <- Mo - 0.5 * (Mo - t(Mo))
  Mo <- Mo - diag(diag(Mo) - sd * sd)
  return(Mo)
}

#' covariance to correlation
#' @param .cov_mat covariance matrix
#' @return list of unit correlation matrix and vector of standard deviations
#' @export
cov_to_cor <- function(.cov_mat) {
  dc <- diag(.cov_mat)
  So <- sqrt(dc)
  ds <- diag(1/So)
  Mo <- ds %*% .cov_mat %*% ds
  Mo <- Mo - 0.5 * (Mo - t(Mo))
  return(list(mat = Mo, sd = So))
}