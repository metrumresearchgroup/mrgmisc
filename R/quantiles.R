#' summarize quantile
#' @param df data frame
#' @param col_name string name for column to calculate quantiles on
#' @param prob probability for quantiles of interest
#' @param na.rm whether to remove na values
#' @examples
#' \dontrun{
#' Theoph %>% s_quantile("conc", 0.1)
#'}
#' @export
s_quantile <- function(df, col_name, prob, na.rm) {
  dots = list(lazyeval::interp(~ quantile(var, 
                                prob, 
                                na.rm=na.rm, 
                                names = F,), 
                     var = as.name(col_name)))
  df %>% dplyr::summarize_(.dots = setNames(dots, paste0("q",prob*10)))
}


