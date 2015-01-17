#' summarize quantile
#' @param df data frame
#' @param col_name string name for column to calculate quantiles on
#' @param prob probability for quantiles of interest
#' @param na.rm whether to remove na values
s_quantile <- function(df, col_name, prob, na.rm=T) {
  dots = list(lazyeval::interp(~ quantile(var, 
                                prob, 
                                na.rm=na.rm, 
                                names = F), 
                     var = as.name(col_name)))
  df %>% dplyr::summarize_(.dots = setNames(dots, paste0("q",prob*100)))
}

#' summarize quantiles
#' @param df data frame
#' @param col_name string name for column to calculate quantiles on
#' @param prob probability for quantiles of interest
#' @param na.rm whether to remove na values
#' @details
#' can calculate multiple quantiles by passing in a vector of probabilities.
#' Can also pass in a grouped df using dplyr::group_by to get summaries by group
#' @examples
#' \dontrun{
#' library(PKPDdatasets)
#' sd_oral_richpk  %>% s_quantiles("Conc", probs = c(0.1, 0.5)) 
#' sd_oral_richpk  %>% group_by(Gender) %>% s_quantiles("Conc", probs = c(0.1, 0.5)) 
#'}
#' @export
s_quantiles <- function(df, col_name, probs, na.rm=T) {
    quantiles <- lapply(probs, function(x) {
    s_quantile(df, col_name, x, na.rm)
    }
    )
    #check if grouped df and if so adjust behavior to bind together the list
    # of quantiles given back from lapply
    if(any(grepl("grouped", attributes(df)$class))) {
      j_quantiles <- quantiles[[1]]
      for(i in seq_along(quantiles)) {
        j_quantiles <- suppressMessages(
          dplyr::inner_join(j_quantiles, quantiles[[i]])
          )
      }
      return(j_quantiles)
    }
    else {
      return(unlist(quantiles))
    }
}
#sd_oral_richpk %>% group_by(Gender) %>% s_quantiles("Conc", probs = c(0.7))
#sd_oral_richpk %>% group_by(Gender) %>% s_quantiles("Conc", probs = c(0.5, 0.7))
#sd_oral_richpk %>% group_by(Gender) %>% s_quantiles("Conc", probs = c(0.2, 0.5, 0.7))