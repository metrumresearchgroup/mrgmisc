#' Overwrite DOSE column into factor and ordered by value
#'
#' @param .dat data.frame with column `.dosecol`
#' @param .dosecol quoted name of column containing dosing information
#' @param .units unit to be appended to output dose column
#' 
#' 
#' @examples 
#' 
#' dose_fix <- pretty_dose(Theoph, .dosecol = "Dose", .units = "mg")
#' dose_fix$Dose
#' 
#' @author Samuel P Callisto, PhD
#' 
#' @export
pretty_dose <- function(.dat, .dosecol="DOSE", .units="mg"){
  .dat$TMP <- .dat[[.dosecol]]
  .dat %>%
    dplyr::mutate(Dose = factor(
      ifelse(.data$TMP == 0, "Placebo", paste(.data$TMP, .units))) %>% stats::reorder(.data$TMP)
    ) %>% 
    dplyr::select(-"TMP")
}