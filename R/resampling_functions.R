#' find unique values for key
#' @param df data frame
#' @param key_cols vector of column names
#' @export
get_key <- function(df, 
                    key_cols) {
  # add check to see if all key_cols available
  unique_df <- df[, key_cols, drop=F] %>%
    data.table::as.data.table() %>%
    unique(by=key_cols)
  return(dplyr::tbl_df(unique_df))
}

#' stratify based on some columns
#' @param df dataframe
#' @param strat_cols columns to stratify on
#' @export
stratify_df <- function(df, 
                        strat_cols) {
  df %>% dplyr::group_by_(.dots=strat_cols) %>% 
    dplyr::sample_frac(1, replace=T)
}

#' resampling
#' @param df
#' @param key_cols
#' @param strat_cols
#' @param key_name name of key column
#' @param ... dots
#' @export
resample_df <- function(df, 
                        key_cols, 
                        strat_cols = NULL, 
                        key_col_name = "KEY") {
  names <- c(key_col_name,names(df))
  
  
  if(is.null(strat_cols)) {
    key <- get_key(df, key_cols)
    sample <- dplyr::sample_n(key, size = nrow(key), replace=T)
    sample[[key_col_name]] <- 1:nrow(sample)
  } else {
    key <- get_key(df, c(key_cols, strat_cols))
    sample <- stratify_df(key, strat_cols)
    #drop strat cols so won't possibly mangle later left join
    sample <- ungroup(sample)
    sample <- sample[, key_cols, drop=F] 
    sample[[key_col_name]] <- 1:nrow(sample)
  }
  
  resampled_df <- dplyr::left_join(sample, df, by = key_cols)
  
  
  #reorder columns to match original df with key column appended
  return(resampled_df[,names, drop=F])
}

# tests
# 
# library(PKPDdatasets)
# dat <- sd_oral_richpk
# sid_dat <- filter(dat, !duplicated(ID))
# sid_dat %>% group_by(Gender) %>% summarize(n = n())
# stratify_df(sid_dat, strat_cols="Gender")%>% summarize(n = n())
# sid_dat %>% group_by(Gender, Race) %>% summarize(n = n())
# stratify_df(sid_dat, strat_cols=c("Gender", "Race"))%>% summarize(n = n())

# rep_dat <- rbind_all(lapply(1:5, function(x) dat %>%
#                               filter(ID < 5) %>% 
#                               mutate(REP = x)))
# resample_df(rep_dat, key_cols = c("ID", "REP"))
# rep_dat %>% group_by(Gender) %>% summarize(n = n())
# stratify_df(rep_dat, strat_cols="Gender")%>% summarize(n = n())
# rep_dat %>% group_by(Gender, Race) %>% summarize(n = n())
# stratify_df(rep_dat, strat_cols=c("Gender", "Race"))%>% summarize(n = n())
#resample_df(rep_dat, key_cols=c("ID", "REP"), strat_cols=c("Gender", "Race"))
#resample_df(rep_dat, key_cols=c("ID", "REP"))
