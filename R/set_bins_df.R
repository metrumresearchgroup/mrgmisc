#' given a set of bin ranges, assign each value to a bin and provide the label
#' @param .df data frame
#' @param .x name of column
#' @param breaks breaks for each bin, defaults to quantiles
#' @param .name name of new binned column, defaults to appending _bin to column name
#' @param .label name of the new label column, defaults to appending _label to bin column name
#' @param lower_bound set a lower bound for the first bin, defaults to -Inf
#' @param upper_bound set an upper bound for the last bind, defaults to Inf
#' @param quiet whether to give additonal information regarding bins and assigned range for each
#' @param between defaults to NULL, a special case of setting all inside the specified range
#' @param inclusive include max value of largest user defined bin even though lower bins are non-inclusive
#' @details
#' set_bins_df offers the ability to create bins from a dataframe and get both the binning column
#' as well as a label column with the range of values associated with a given bin 
#' @seealso \code{\link{set_bins}}
#' @export
set_bins_df <- function(
  .df, 
  .x, 
  breaks = stats::quantile(.df[[.x]], na.rm = T), 
  .name = NULL, 
  .label = NULL,
  lower_bound = -Inf, 
  upper_bound = Inf, 
  quiet = TRUE,
  between = NULL, 
  inclusive = TRUE
  ) {
  
  if (is.null(.name)) {
    .name <- paste0(.x, "_bins")
  }
  
  if (is.null(.label)) {
    .label <- paste0(.name, "_label")
  }
  
  x <- .df[[.x]]
  breaks <- breaks[order(breaks)]
  
  if (!is.null(between)) {
    if(length(between) != 2) {
      stop("can only have 2 breaks to use the between functionality")
    }
      x_bins <- ifelse(dplyr::between(x, between[1], between[2]), 1, 
                       ifelse(x > between[2], 2, 0))
      if(!quiet) {
        message(paste0("there were 3 bins calculated, with the following
                   range for each bin: "))
        message(paste0("BIN 0: All values less than ", between[1]))
        message(paste0("BIN 1: All values between ", between[1], " and ", between[2]))
        message(paste0("BIN 2: All values greater than ", between[2]))
      }
      return(x_bins)
  }
  
  if (breaks[1] > lower_bound && !is.null(lower_bound)) {
    breaks <- c(lower_bound, breaks)
  } 
  if (breaks[length(breaks)] < upper_bound && !is.null(upper_bound)) {
    breaks <- c(breaks, upper_bound)
  } 
  
  if (inclusive) {
    top_user_bin_index <- ifelse(upper_bound == Inf, length(breaks) - 1, length(breaks))
    top_bin <- breaks[top_user_bin_index]
    breaks[top_user_bin_index] <- top_bin + top_bin*0.0001
  }

 
  if(length(breaks) ==1) breaks <- c(-Inf, breaks, Inf)
  
  lower <- breaks[-length(breaks)]
  upper <- breaks[-1]
  
  
  x_bins <- set_bins_cpp(x, lower, upper)
  
  if(inclusive) {
    # set top_bin_index back to value for labeling purposes
  
  breaks[top_user_bin_index] <- top_bin
  # lazy lets just re-assign lower and upper
  lower <- breaks[-length(breaks)]
  upper <- breaks[-1]
  
    
  }
  possible_bins <- paste("[", paste(lower, upper, sep  = "_"), ")", sep = "")
  bin_factor <- factor(x_bins, levels = 0:(length(lower) - 1), labels = possible_bins)
  if(!quiet) {
    message(paste0("there were ", length(lower), "bins calculated, with the following
                   range for each bin: "))
    for (i in seq_along(lower)) {
      message(paste("BIN:", i-1, "range:", lower[i], "-", upper[i]))
    }
  }
  return(dplyr::bind_cols(.df, setNames(list(x_bins, bin_factor), c(.name, .label))))
  }