#' function to overwrite DOSE column into factor and ordered by value
#'
#' @param .dat data.frame with column `.dosecol`
#' @param .dosecol quoted name of column containing dosing information
#' @param .units unit to be appended to output dose column
#' 
#' @export
pretty_dose <- function(.dat, .dosecol="DOSE", .units="mg"){
  .dat$TMP <- .dat[[.dosecol]]
  .dat %>%
    mutate(Dose = factor(
      ifelse(TMP == 0, "Placebo", paste(TMP, .units))) %>% reorder(TMP)
    ) %>% select(-TMP)
}