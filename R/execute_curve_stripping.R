#' curve strip a dataframe with multiple IDs
#' @details
#' calculates the geometric mean by dose group then curve strips each mean
#' expects 3 columns will be in dataset, the independent variable (eg time), 
#' dependent variable (eg concentration), and a dose column with the dose given for each ID in all rows
#' @param df dataframe to curve strip
#' @param TIME_name column name for time column
#' @param DV_name column name for dependent variable column eg. conc
#' @param DOSE_name column name for dose column
#' @param number_terminal_points number of data points in terminal phase
#' @param ... additional args to be passed to strip_curves
#' @examples 
#' \dontrun{
#' execute_curve_stripping(dat_IV, number_terminal_points =9)
#' execute_curve_stripping(dat_IV, "time", "CObs", "DOSE", number_terminal_points=6)
#' }
#' @export
execute_curve_stripping <- function(df, TIME_name = "TIME", DV_name = "CONC", DOSE_name = "DOSE", number_terminal_points, ...) {
  df <- df
  names(df)[which(names(df) == DV_name)] <- "CONC"
  if("TIME" %in% names(df) & TIME_name != "TIME") {
    names(df)[which(names(df) == "TIME")] <- NA
    names(df)[which(names(df) == TIME_name)] <- "TIME"
  } else {
    names(df)[which(names(df) == TIME_name)] <- "TIME"
  }
  names(df)[which(names(df) == DOSE_name)] <- "DOSE" 
  mean_IV <- df %>% dplyr::group_by(TIME, DOSE) %>% dplyr::summarize(geomean = exp(mean(log(CONC))))
  result <- mean_IV %>% dplyr::group_by(DOSE) %>% dplyr::do(strip_curves(TIME = .$TIME, .$geomean,DOSE = .$DOSE, number_terminal_points,...))
  result %>% dplyr::group_by(DOSE) %>% dplyr::summarise_each(funs(mean))
}


