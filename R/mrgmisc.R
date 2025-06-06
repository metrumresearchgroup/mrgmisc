#' @useDynLib mrgmisc
#' @importFrom Rcpp sourceCpp
#' @importFrom glue glue
#' @importFrom stats lm predict quantile setNames
#' @importFrom utils head read.table tail write.table
#' @importFrom magrittr %>%
#' @importFrom rlang .data caller_env
NULL

#' One-compartment pharmacokinetic data given single oral dose
#' 
#' A dataset containing dose, plasma concentration and time data, as well as 
#'  demographic data of Age, Weight, Gender, and Race for each individual
#' 
#' \itemize{
#'   \item ID. Numerical ID  (1--50)
#'   \item Time. Time in hours (0--24)
#'   \item Amt. Amount of drug given, time dependent, in milligrams
#'   \item Conc. Plasma concentration in mg/L
#'   \item Age. Age in years
#'   \item Weight. Weight in kg
#'	 \item Gender. Male or Female gender identification
#'   \item Race. Ethnicity
#'   \item Dose. Dose given to each individual in milligrams
#' }
#' 
#' @docType data
#' @keywords datasets
#' @name sd_oral_richpk
#' @usage data(sd_oral_richpk)
#' @keywords internal
#' @format A data frame with 4300 rows and 9 variables
NULL

#' IV and oral pharmacokinetic data for daptomycin
#' 
#' A dataset containing simulated dapagliflozin PK.  A single IV dose followed by 3 escalating oral doses
#' 
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
#' @keywords internal
#' @format A data frame with 1536 rows and 11 variables
NULL

#' @keywords internal
"_PACKAGE"

globalVariables(
  c(
    "mrgmisc_idv",
    "mrgmisc_dv"
  )
)