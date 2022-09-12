#' Given a set of bin ranges, assign each value to a bin
#' @param x numeric vector to assign bins
#' @param breaks breaks for each bin, defaults to quantiles
#' @param lower_bound set a lower bound for the first bin, defaults to -Inf
#' @param upper_bound set an upper bound for the last bind, defaults to Inf
#' @param quiet whether to give additional information regarding bins and assigned range for each
#' @param between defaults to NULL, a special case of setting all inside the specified range
#' @param inclusive include max value of largest user defined bin even though lower bins are non-inclusive
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
#' 
#' 
#' @examples 
#' x <- Theoph$conc
#' 
#' head(x)
#' [1]  0.74  2.84  6.57 10.50  9.66  8.58
#' 
#' #basic example
#' res <- set_bins(x)
#' 
#' head(res)
#' [1] 1 1 3 4 4 4
#' 
#' table(res)
#' res
#' 1  2  3  4 
#' 33 33 32 34
#' 
#' #assign all obs < lower bound to NA
#' res <- set_bins(x, breaks = stats::quantile(x, na.rm = T, probs= c(0.1, 0.5, 1)), lower_bound = 1)
#' 
#' head(res)
#' [1] NA  0  1  1  1  1
#' 
#' table(res)
#' res
#' 0  1 
#' 52 66 
#' 
#' #use inclusive argument to get desired bins
#' ## include max value of largest user defined bin
#' xbreak <- stats::quantile(x, na.rm = T, probs= c(0, 0.5, 1))
#' xupper = Inf
#' 
#' res1 <- set_bins(x, breaks = xbreak, upper_bound = xupper, inclusive = TRUE)
#' 
#' table(res1)
#' res1
#' 1  2 
#' 66 66 
#' 
#' ## do not include max value of largest user-defined bin- create new bin for it
#' res2 <- set_bins(x, breaks = xbreak, upper_bound = xupper, inclusive = FALSE)
#' 
#' table(res2)
#' res2
#' 1  2  3 
#' 66 65  1
#' 
#' # use between argument to cut obs at certain values. For example, want a bin of conc between 3-7
#' res <- set_bins(x,  between = c(3, 7)) 
#' 
#' head(res)
#' [1] 0 0 1 2 2 2
#' 
#' table(res)
#' res
#' 0  1  2 
#' 34 62 36 

#' @seealso \code{\link{set_bins_df}}: This function creates bins from a dataframe and outputs both the binning column
#' as well as a label column with the range of values associated with a given bin
#' 
#' 
#' @export
set_bins <- function(x, breaks = stats::quantile(x, na.rm = T), lower_bound = -Inf, upper_bound = Inf, quiet = TRUE,
                     between = NULL, inclusive = TRUE) {
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
  if(!quiet) {
    message(paste0("there were ", length(lower), " bins calculated, with the following
                   range for each bin: "))
    for (i in seq_along(lower)) {
      message(paste("BIN:", i-1, "range:", lower[i], "-", upper[i]))
    }
  }
  return(x_bins)
  
}
