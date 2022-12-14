% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/set_bins.R
\name{set_bins}
\alias{set_bins}
\title{Given a set of bin ranges, assign each value to a bin}
\usage{
set_bins(
  x,
  breaks = stats::quantile(x, na.rm = TRUE),
  lower_bound = -Inf,
  upper_bound = Inf,
  quiet = TRUE,
  between = NULL,
  inclusive = TRUE
)
}
\arguments{
\item{x}{numeric vector to assign bins}

\item{breaks}{breaks for each bin, defaults to quantiles}

\item{lower_bound}{set a lower bound for the first bin, defaults to -Inf}

\item{upper_bound}{set an upper bound for the last bind, defaults to Inf}

\item{quiet}{whether to give additional information regarding bins and assigned range for each}

\item{between}{defaults to NULL, a special case of setting all inside the specified range}

\item{inclusive}{include max value of largest user defined bin even though lower bins are non-inclusive}
}
\description{
Given a set of bin ranges, assign each value to a bin
}
\details{
Given a set of quantiles/bins/etc established from a separate dataset, it can 
be useful to assign the same bins to new or simulated data for comparisons
or to do additional analysis such as assign dropouts etc. This function can be
used to take the breakpoints to establish bins quickly and easily

If there is concern over data being outside the range of the assigned breaks, 
one can assign -Inf to lower and/or Inf to upper to make sure all values will be 
assigned to a bin

To use the between functionality, you must specify the range you wish to bin between,
and those values will be assigned to bin 1, with all values below as 0 and all values
above as 2. See the examples for more details
}
\examples{
x <- Theoph$conc

head(x)

#basic example
res <- set_bins(x)

head(res)

table(res)
res

#assign all obs < lower bound to NA
res <- set_bins(x,
    breaks = stats::quantile(x, na.rm = TRUE, probs = c(0.1, 0.5, 1)),
    lower_bound = 1)

head(res)

table(res)
res

#use inclusive argument to get desired bins
## include max value of largest user defined bin
xbreak <- stats::quantile(x, na.rm = TRUE, probs= c(0, 0.5, 1))
xupper = Inf

res1 <- set_bins(x, breaks = xbreak, upper_bound = xupper, inclusive = TRUE)

table(res1)

## do not include max value of largest user-defined bin- create new bin for it
res2 <- set_bins(x, breaks = xbreak, upper_bound = xupper, inclusive = FALSE)

table(res2)
res2

# use between argument to cut obs at certain values. For example, want a bin of conc between 3-7
res <- set_bins(x,  between = c(3, 7)) 

head(res)

table(res)
}
\seealso{
\code{\link{set_bins_df}}: This function creates bins from a dataframe and outputs both the binning column
as well as a label column with the range of values associated with a given bin
}
