#' find unique values for key
#' @param df data frame
#' @param key_cols vector of column names. Defaults to all columns
#' @details 
#' used in stratify_df, however also made available to find
#' non-duplicated values across multiple columns 
#' when !duplicated(col) is not sufficient
#' @noRd
get_key <- function(df, 
                    key_cols = names(df)) {
  # add check to see if all key_cols available
  unique_df <- df[, key_cols, drop=F] %>%
    data.table::as.data.table() %>%
    unique(by=key_cols)
  return(tibble::as_tibble(unique_df))
}

#' stratify based on some columns
#' @param df dataframe
#' @param strat_cols columns to stratify on
#' @param n number of samples
#' @param replace whether to resample with replacement
#' @noRd
stratify_df <- function(df, 
                        strat_cols,
                        n,
                        replace = TRUE
                        ) {
  frac <- n/nrow(df)
  sample <- df %>% dplyr::group_by(!!!rlang::syms(strat_cols)) %>% 
    dplyr::sample_frac(frac, replace=replace)
  nsample <- nrow(sample) 
  nleft <- n- nsample
  if(nleft != 0) {
    ifelse(nleft > 0, {
      extras <- dplyr::sample_n(df, size = nleft)
      sample <- dplyr::bind_rows(sample, extras)
    }, {
      removals <- sample.int(nsample, abs(nleft))
           sample <- sample[-removals,]
    }
    )

  }
  return(sample)
}

#' resampling
#' @param df data frame
#' @param key_cols key columns to resample on
#' @param strat_cols columns to maintain proportion for stratification
#' @param n number of unique sampled keys, defaults to match dataset
#' @param key_col_name name of outputted key column. Default to "KEY"
#' @param replace whether to stratify with replacement
#' @details 
#' This function is valuable when generating a large simulated population
#' where you goal is to create resampled sub-populations in addition to being able to
#' maintain certain stratifications of factors like covariate distributions
#' 
#' A new keyed column will be created (defaults to name 'KEY') that contains the uniquely
#' created new samples. This allows one to easily compare against the key'd columns. Eg,
#' if you would like to see how many times a particular individual was resampled you can 
#' check the original ID column against the number of key's associated with that ID number.
#' @examples
#' library(mrgmisc)
#' library(dplyr, quiet = TRUE)
#' 
#' # simple example resampling by ID maintaining Gender distribution, with 10 individuals
#' resample_df(sd_oral_richpk, key_cols = "ID", strat_cols = "Gender", n = 10)
#' 
#' # for a more complex example lets resample "simulated" data with multiple replicates
#' subset_data <- sd_oral_richpk %>%
#'    filter(ID < 20)
#'    
#' # make 'simulated' data with 5 replicates and combine to single dataframe
#' rep_dat <- lapply(1:5, function(x) {
#' subset_data %>% 
#'   mutate(REP = x)
#'   }) %>% bind_rows()
#' 
#' # now when we resample we also want to maintain the ID+REP relationship as resampling
#' # just the ID would give all rows associated for an ID with all reps, rather than 
#' # a single "unit" of ID/REP
#' resample_df(rep_dat, key_cols = c("ID", "REP"))
#' 
#' # check to see that stratification is maintained
#' rep_dat %>% group_by(Gender) %>% tally
#' resample_df(rep_dat, key_cols=c("ID", "REP"), strat_cols="Gender") %>%
#'   group_by(Gender) %>% tally
#'   
#' rep_dat %>% group_by(Gender, Race) %>% tally
#' 
#' resample_df(rep_dat, key_cols=c("ID", "REP"), strat_cols=c("Gender", "Race")) %>%
#'   group_by(Gender, Race) %>% tally
#' @export
resample_df <- function(df, 
                        key_cols,
                        strat_cols = NULL, 
                        n = NULL,
                        key_col_name = "KEY",
                        replace = TRUE) {
  # checks
  if (is.numeric(strat_cols)) {
    message("It looks you are trying to give a numeric value for strat_cols, 
 perhaps you were trying to specify the number to sample instead? 
 If no strat_cols are specified you must explicitly specify 'n = ...' 
 For example resample_df(Theoph, 'Subject', n = 20 )")
    message("-----------------------------------")
    stop("To set the number of samples please explicitly specify 'n = <num>'.")
  }
  
  names <- c(key_col_name,names(df))
  key <- get_key(df, key_cols)
  if(is.null(n)) n <- nrow(key)
  
  if(is.null(strat_cols)) {
    sample <- dplyr::sample_n(key, size = n, replace=replace)
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
    sample <- stratify_df(strat_key, strat_cols, n, replace = replace)
    #drop strat cols so won't possibly mangle later left join
    sample <- dplyr::ungroup(sample)
    sample <- sample[, key_cols, drop=F] 
    sample[[key_col_name]] <- 1:nrow(sample)
  }
  
  # relationship arg added in dplyr 1.1.1
  if (utils::packageVersion("dplyr") >= "1.1.1") {
    # Specify join relationship based on replacement to silence warning:
    #  "Detected an unexpected many-to-many relationship between `x` and `y`"
    # - occurs when there are repeated key_cols in both data frames
    relat <- if(isTRUE(replace)) "many-to-many" else NULL
    resampled_df <- dplyr::left_join(sample, df, by = key_cols, relationship = relat)
  } else {
    resampled_df <- dplyr::left_join(sample, df, by = key_cols)
  }
  
  #reorder columns to match original df with key column appended
  return(resampled_df[,names, drop=F])
}
