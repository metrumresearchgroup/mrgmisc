#' @rdname s_quantiles
#' @export
s_quantiles_<- function(.data, x, probs, na_rm = TRUE) {
  
  quantiles_df <- list()
  
  for (i in 1:length(probs)) {
    
    .df_tmp <-
      .data %>% 
      dplyr::summarise(valMRGMISC = quantile(!!rlang::sym(x), probs[i], na.rm = na_rm), .groups = "keep")
    
    .df_tmp[[paste0(x,"_q",probs[i]*100)]] <- .df_tmp$valMRGMISC
    .df_tmp$valMRGMISC <- NULL
    
    quantiles_df[[i]] <- .df_tmp
    
  }
  
  #check if grouped df and if so adjust behavior to bind together the list
  # of quantiles given back from lapply
  if(any(grepl("grouped", attributes(.data)$class))) {
    j_quantiles_df <- quantiles_df[[1]]
    for(i in seq_along(quantiles_df)) {
      j_quantiles_df <- suppressMessages(
        dplyr::inner_join(j_quantiles_df, quantiles_df[[i]])
      )
    }
    return(j_quantiles_df)
  }
  else {
    # if use unlist, since the internal vectors are named
    # so get 'double' names after unlisting
    # eg 'pAUC0_24.pAUC0_24'
    return(dplyr::bind_cols(quantiles_df))
  }
}

#' Summarize quantiles for a column
#' @param .data data frame
#' @param x column to calculate quantiles for
#' @param probs probabilities to calculate quantiles for
#' @param na_rm remove na's before calculating value for quantile
#' @rdname s_quantiles
#' @examples 
#' library(dplyr)
#' sd_oral_richpk %>% group_by(Gender, Time) %>% s_quantiles(Conc, c(0.05, 0.5, 0.95))
#' @export
s_quantiles <- function(.data, x, probs, na_rm = TRUE) {
  s_quantiles_(.data, deparse(substitute(x)), probs, na_rm)
}
