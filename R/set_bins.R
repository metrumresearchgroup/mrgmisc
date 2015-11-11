#' given a set of bin ranges, assign each value to a bin
#' @param x numeric vector to assign bins
#' @param breaks breaks for each bin, defaults to quantiles
#' @param lower_bound set a lower bound for the first bin, defaults to -Inf
#' @param upper_bound set an upper bound for the last bind, defaults to Inf
#' @param quiet whether to give additonal information regarding bins and assigned range for each
#' @param between defaults to NULL, a special case of setting all inside the specified range
#' @param return_range return the range for each bin rather than the bin itself
#' @details
#' Given a set of quantiles/bins/etc established from a separate dataset, it can 
#' be useful to assign the same bins to new or simulated data for comparisons
#' or to do additional analysis such as assign dropouts etc. This function can be
#' used to take the breakpoints to establish bins quickly and easily
#' 
#' If there is concern over data being outside the range of the assigned breaks, 
#' one can assign -Inf to lower and/or Inf to upper to make sure all values will be 
#' assigned to a bin
#' 
#' To use the between functionality, you must specify the range you wish to bin between,
#' and those values will be assigned to bin 1, with all values below as 0 and all values
#' above as 2. See the examples for more details
#' @export
set_bins <- function(x, breaks = quantile(x), lower_bound = -Inf, upper_bound = Inf, quiet = TRUE,
                     between = NULL,
                     return_range=FALSE) {
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
    
  if(length(breaks) ==1) breaks <- c(-Inf, breaks, Inf)
  
  lower <- breaks[-length(breaks)]
  upper <- breaks[-1]
  x_bins <- set_bins_cpp(x, lower, upper)
  if(!quiet) {
    message(paste0("there were ", length(lower), "bins calculated, with the following
                   range for each bin: "))
    for (i in seq_along(lower)) {
      message(paste("BIN:", i-1, "range:", lower[i], "-", upper[i]))
    }
  }
  if(return_range) {
    # TODO: fix return_range to work properly with dplyr 
    # currently it will overwrite the bin ranges for the first grouping variable
    # over all the other groups as well
    stop("currently removed as is buggy when dealing with groups in dplyr, will fix")
    # x_bins <- factor(x_bins, labels = paste0("(", lower, "-", upper, "]"))
  }
  return(x_bins)
  
  }

