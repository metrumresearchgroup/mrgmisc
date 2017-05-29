s_quantiles_i <- function(.data, x, prob, na_rm = TRUE) {
  dots = list(lazyeval::interp(~ quantile(x, prob, na.rm = na_rm), 
                               x = as.name(x)))
  # after discussion with hadley, the last group is dropped by design with dplyr
  # given that it is unique at that point
  # for now, I do not want to do that as I want to keep track of all grouped
  # variables to determine how to handle the summaries after (eg will want additional)
  # summaries on all non-group columns (in this case all pauc cols) so don't want
  # the group to be dropped
  grps <- ifelse(!is.null(dplyr::groups(.data)), dplyr::groups(.data), NA)
  out <- .data %>% dplyr::summarize_(.dots = setNames(dots, 
                                                      paste0(x, 
                                                             "_q", 
                                                             prob*100)))
  if(!is.na(grps)) out <- dplyr::group_by_(out, .dots=grps)
  return(out)
}

#' @rdname s_quantiles
#' @export
s_quantiles_<- function(.data, x, probs, na_rm = TRUE) {
  quantiles_df <- lapply(probs, function(p) {
    s_quantiles_i(.data, x, p, na_rm)
  })
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

#' summarize quantiles for a column
#' @param .data data frame
#' @param x column to calculate quantiles for
#' @param quantiles probabilities to calculate quantiles for
#' @param na_rm remove na's before calculating value for quantile
#' @rdname s_quantiles
#' @examples 
#' library(dplyr)
#' sd_oral_richpk %>% group_by(Gender, Time) %>% s_quantiles(Conc, c(0.05, 0.5, 0.95))
#' @export
s_quantiles <- function(.data, x, probs, na_rm = TRUE) {
  s_quantiles_(.data, deparse(substitute(x)), probs, na_rm)
}
