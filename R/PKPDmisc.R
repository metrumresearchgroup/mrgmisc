#' @useDynLib PKPDmisc
#' @importFrom Rcpp sourceCpp
#' @importFrom stats lm predict quantile setNames
#' @importFrom utils head read.table tail write.table
NULL

#' IV and oral pharmacokinetic data for daptomycin
#' 
#' A dataset containing simulated dapagliflozin PK.  A single IV dose followed by 3 escalating oral doses
#' \itemize{
#'   \item ID. Numerical ID  (1-24)
#'   \item TIME. Nominal Time after first dose (hrs)
#'   \item TAD. Time After Dose (hrs)
#'   \item COBS. Observed Concentration (ug/L)
#'   \item AMT_IV. IV dose amount when given (ug)
#'   \item AMT_ORAL. Oral dose amount when given (ug)
#'   \item OCC. Occasion, associated with each dosing event
#'   \item AGE. Age (years)
#'   \item WEIGHT Weight (kg)
#'   \item GENDER. Gender flag (female=1, male=0)
#'   \item FORMULATION Formulation associated with dose (IV or oral)
#' }
#' @docType data
#' @keywords datasets
#' @name dapa_IV_oral
#' @usage data(dapa_IV_oral)
#' @format A data frame with 1536 rows and 11 variables
NULL
