#' summarize quantile
#' @param df data frame
#' @param col_name string name for column to calculate quantiles on
#' @param prob probability for quantiles of interest
#' @param na.rm whether to remove na values
s_quantile <- function(df, col_name, prob, na.rm=T) {
  col_name <- lazyeval::as.lazy(col_name)
  dots = list(lazyeval::interp(~ quantile(var, 
                                prob, 
                                na.rm=na.rm, 
                                names = F), 
                     var = col_name$expr))
  df %>% dplyr::summarize_(.dots = setNames(dots, paste0("q",prob*100)))
}

#' summarize quantiles
#' @param df data frame
#' @param col_name string name for column to calculate quantiles on
#' @param probs probability for quantiles of interest
#' @param na.rm whether to remove na values
#' @details
#' can calculate multiple quantiles by passing in a vector of probabilities.
#' Can also pass in a grouped df using dplyr::group_by to get summaries by group
#' @examples
#' sd_oral_richpk  %>% s_quantiles(Conc, probs = c(0.1, 0.5)) 
#' sd_oral_richpk  %>% group_by(Gender) %>% s_quantiles_("Conc", probs = c(0.1, 0.5)) 
#' @export
#' @rdname s_quantiles
s_quantiles_ <- function(df, col_name, probs=c(0, 0.25, 0.5, 0.75, 1), na.rm=T) {
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
#' @export
#' @rdname s_quantiles
s_quantiles <- function(df, col_name, probs=c(0, 0.25, 0.5, 0.75, 1), na.rm=T) {
  col_name <- lazyeval::lazy(col_name)
  s_quantiles_(df, col_name, probs, na.rm)
}
#sd_oral_richpk %>% group_by(Gender) %>% s_quantiles("Conc", probs = c(0.7))
#sd_oral_richpk %>% group_by(Gender) %>% s_quantiles("Conc", probs = c(0.5, 0.7))
#sd_oral_richpk %>% group_by(Gender) %>% s_quantiles("Conc", probs = c(0.2, 0.5, 0.7))