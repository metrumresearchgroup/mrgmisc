#' resampling
#' @param df
#' @param key_cols
#' @param strat_cols
#' @param key_name name of key column
#' @param ... dots
#' @export
resample_df <- function(df, key_cols, strat_cols = NULL, key_col_name = "KEY") {
  names <- names(df)
  key <- get_key(df, key_cols)
  sample <- sample_n(key, size = nrow(key), replace=T)
  if(!(is.null(key_col_name))) {
    names <- c(key_col_name, names)
    sample[[key_col_name]] <- 1:nrow(sample)
  }
  resampled_df <- left_join(sample, df, by = key_cols)
  
  #reorder columns to match original df
  return(resampled_df[,names, drop=F])
}

#' find unique values for key
#' @param df data frame
#' @param key_cols vector of column names
#' @export
get_key <- function(df, key_cols) {
  # add check to see if all key_cols available
  unique_df <- df[, key_cols, drop=F] %>%
    data.table::as.data.table() %>%
    data.table::unique(by=key_cols)
  return(dplyr::tbl_df(unique_df))
}

stratify_df <- function(df, strat_cols = NULL) {
  
}



