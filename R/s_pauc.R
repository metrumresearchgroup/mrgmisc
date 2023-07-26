# summarize quantile
# @param df data frame
# @param idv string name for time column for pauc slice
# @param dv string name for dependent variable column (eg. dv or cobs)
# @param range whether to remove na values
# @param digits number of digits to pass to round
# @details 
# for internal use in the s_pauc function
s_pauc_i <- function(df, idv, dv, range, digits = Inf) {
  
  df$mrgmisc_idv <- df[[idv]]
  df$mrgmisc_dv <- df[[dv]]

  # after discussion with hadley, the last group is dropped by design with dplyr
  # given that it is unique at that point
  # for now, I do not want to do that as I want to keep track of all grouped
  # variables to determine how to handle the summaries after (eg will want additional)
  # summaries on all non-group columns (in this case all pauc cols) so don't want
  # the group to be dropped
  grps <- if (inherits(df, "grouped_df")) {
    dplyr::groups(df)
  } else {
    NULL
  }

  out <- 
    df %>%
    dplyr::summarize(
      mrgmisc_ANS = 
        round(
          auc_partial(mrgmisc_idv, mrgmisc_dv, range = range),
          digits
        )
    )
  
  out[[paste0("pAUC", range[1], "_", range[2])]] <- out$mrgmisc_ANS
  
  out <- out %>% dplyr::select(-dplyr::starts_with("mrgmisc_"))

  if(!is.null(grps)) out <- dplyr::group_by(out, !!!rlang::syms(grps))
  return(out)
}

#' @rdname s_pauc
#' @export
s_pauc_ <- function(df, idv, dv, paucs, digits = Inf) {
  paucs <- lapply(paucs, function(x) {
    s_pauc_i(df, idv, dv, x, digits)
  }
  )
  #check if grouped df and if so adjust behavior to bind together the list
  # of quantiles given back from lapply
  if(any(grepl("grouped", attributes(df)$class))) {
    j_paucs <- paucs[[1]]
    for(i in seq_along(paucs)) {
      j_paucs <- suppressMessages(
        dplyr::inner_join(j_paucs, paucs[[i]])
      )
    }
    return(j_paucs)
  }
  else {
    # if use unlist, since the internal vectors are named
    # so get 'double' names after unlisting
    # eg 'pAUC0_24.pAUC0_24'
    return(do.call("cbind",paucs))
  }
}

#' summarize paucs
#' @param df data frame
#' @param idv string name for time column for pauc slice
#' @param dv string name for dependent variable column (eg. dv or cobs)
#' @param paucs list of ranges for pauc calculation
#' @param digits number of decimals to round result before returning
#' @rdname s_pauc
#' @examples
#' library(dplyr)
#' sd_oral_richpk  %>% group_by(ID) %>% s_pauc(Time, Conc, list(c(0,8), c(8, 24)))
#' sd_oral_richpk  %>% group_by(ID) %>% s_pauc_("Time", "Conc", list(c(0,8), c(8, 24)))
#' @export
s_pauc <- function(df, idv, dv, paucs, digits = Inf) {
  # currently using lazyeval isn't a good solution as fails
  # ifa column name matches a function name (eg time)
  s_pauc_(df, deparse(substitute(idv)), deparse(substitute(dv)), paucs, digits = digits)
}
