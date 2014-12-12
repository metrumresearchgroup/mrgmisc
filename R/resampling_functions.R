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
#' @param n number of samples
#' @export
stratify_df <- function(df, 
                        strat_cols,
                        n) {
  frac <- n/nrow(df)
  sample <- df %>% dplyr::group_by_(.dots=strat_cols) %>% 
    dplyr::sample_frac(frac, replace=T)
  nsample <- nrow(sample) 
  nleft <- n- nsample
  if(nleft != 0) {
    ifelse(nleft > 0, {
      extras <- dplyr::sample_n(df, size = nleft)
      sample <- rbind(sample, extras)
    }, {
      removals <- sample.int(nsample, abs(nleft))
           sample <- sample[-removals,]
    }
    )

  }
  return(sample)
}

#' resampling
#' @param df
#' @param key_cols
#' @param strat_cols
#' @param key_col_name name of key column
#' @param n number of unique sampled keys, defaults to match dataset
#' @export
resample_df <- function(df, 
                        key_cols, 
                        strat_cols = NULL, 
                        key_col_name = "KEY",
                        n = NULL) {
  names <- c(key_col_name,names(df))
  key <- get_key(df, key_cols)
  if(is.null(n)) n <- nrow(key)
  
  if(is.null(strat_cols)) {

    sample <- dplyr::sample_n(key, size = n, replace=T)
    sample[[key_col_name]] <- 1:n
  } else {
    strat_key <- get_key(df, c(key_cols, strat_cols))
    
    # because getting unique key based on stratification columns as well 
    # (to easily carry columns) if there are multiple stratification values
    # per unique key may introduce a bug
    # eg if stratifying by disease status if an individual has at some time
    # both positive and negative statuses, both will be picked up as two separate
    # instances so will duplicate that individual. For now issue warning but let
    # it proceed
    if(nrow(strat_key) != nrow(key)) {
      warning("Non-unique keys introduced from stratification,
check that all keys only have one stratification variable associated
                                             ")}
    sample <- stratify_df(strat_key, strat_cols, n)
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
# 
# rep_dat <- rbind_all(lapply(1:5, function(x) dat %>%
#                               filter(ID < 5) %>% 
#                               mutate(REP = x)))
# resample_df(rep_dat, key_cols = c("ID", "REP"))
# rep_dat %>% group_by(Gender) %>% summarize(n = n())
# stratify_df(rep_dat, strat_cols="Gender")%>% summarize(n = n())
# rep_dat %>% group_by(Gender, Race) %>% summarize(n = n())
# stratify_df(rep_dat, strat_cols=c("Gender", "Race"))%>% summarize(n = n())
# resample_df(rep_dat, key_cols=c("ID", "REP"), strat_cols=c("Gender", "Race"))
# resample_df(rep_dat, key_cols=c("ID", "REP"))
# 
# resample_df(rep_dat, 
#             key_cols=c("ID", "REP"), 
#             strat_cols=c("Gender", "Race"),
#             n =50) %>% group_by(Gender, Race) %>% filter(!duplicated(KEY)) %>%
#   summarize(n = n())
# 
# rep_dat %>% mutate(totn = n()) %>% 
#   group_by(Gender, Race) %>% summarize(n =n()/mean(totn))
