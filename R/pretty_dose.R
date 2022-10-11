#' Overwrite DOSE column into factor and ordered by value
#'
#' @param .dat data.frame with column `.dosecol`
#' @param .dosecol quoted name of column containing dosing information
#' @param .units unit to be appended to output dose column
#' 
#' @examples 
#' 
#' dose_fix <- pretty_dose(Theoph, .dosecol = "Dose", .units = "mg")
#' dose_fix$Dose
#' 
#' @export
pretty_dose <- function(.dat, .dosecol="DOSE", .units="mg"){
  .dat$TMP <- .dat[[.dosecol]]
  .dat %>%
    dplyr::mutate(Dose = factor(
      ifelse(TMP == 0, "Placebo", paste(TMP, .units))) %>% reorder(TMP)
    ) %>% dplyr::select(-TMP)
}