#' given a set of bin ranges, assign each value to a bin
#' @param x numeric vector to assign bins
#' @param breaks breaks for each bin, defaults to quantiles
#' @param lower_bound set a lower bound for the first bin, defaults to -Inf
#' @param upper_bound set an upper bound for the last bind, defaults to Inf
#' @param quiet whether to give additonal information regarding bins and assigned range for each
#' @details
#' Given a set of quantiles/bins/etc established from a separate dataset, it can 
#' be useful to assign the same bins to new or simulated data for comparisons
#' or to do additional analysis such as assign dropouts etc. This function can be
#' used to take the breakpoints to establish bins quickly and easily
#' 
#' If there is concern over data being outside the range of the assigned breaks, 
#' one can assign -Inf to lower and/or Inf to upper to make sure all values will be 
#' assigned to a bin
#' @export
set_bins <- function(x, breaks = quantile(x), lower_bound = -Inf, upper_bound = Inf, quiet = TRUE) {
  breaks <- breaks[order(breaks)]
  if (breaks[1] > lower_bound && !is.null(lower_bound)) breaks <- c(-Inf, breaks)
  if (breaks[length(breaks)] < upper_bound && !is.null(upper_bound)) breaks <- c(breaks, Inf)
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
  
  return(x_bins)
  
  }

