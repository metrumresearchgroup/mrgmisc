
# Break argument ----------------------------------------------------------

test_that("works with breaks out of order [MRG-MISC-0121]", {
  x <- Theoph$conc
  res <- set_bins(x, breaks = stats::quantile(x, na.rm = T, probs= c(1, 0, 0.5)))
  res2 <- set_bins(x, breaks = stats::quantile(x, na.rm = T, probs= c(0, 0.5, 1)))
  expect_equal(
    res,
    res2
  )
  expect_equal(
    sort(unique(res)),
    c(1,2) 
  )
})

# if one break provided, assumes breaks <- c(-Inf, breaks, Inf)
test_that("works with 1 break provided [MRG-MISC-0121]", {
  x <- Theoph$conc
  res <- set_bins(x, breaks = stats::quantile(x, na.rm = T, probs= c(0.2)))
  expect_equal(
    sort(unique(res)),
    c(0,1) 
  )
})

# if no break specified, do quartiles
test_that("works with default 4 bins quartiles if not break not specified [MRG-MISC-0121]", {
  x <- Theoph$conc
  res <- set_bins(x) 
  res2 <- set_bins(x, breaks = stats::quantile(x, na.rm = T, probs= c(0, 0.25, 0.5, 0.75, 1)))
  expect_equal(
    res,
    res2
  )
  expect_equal(
    sort(unique(res)),
    c(1,2, 3, 4)
  )
})

# bin and break n
test_that("works with 2 bins: n bins = n breaks -1 [MRG-MISC-0121]", {
  x <- Theoph$conc
  xbreak <- stats::quantile(x, na.rm = T, probs= c(0, 0.5, 1))
  res <- set_bins(x, breaks = xbreak)
  expect_equal(
    length(xbreak) - 1,
    length(unique(res))
  )
  expect_equal(
    sort(unique(res)),
    c(1,2)
  )
})


# prob out of range error message- FIX
test_that("works with error message: breaks provided are out of range [MRG-MISC-0121]", {
  x <- Theoph$conc
  res <- set_bins(x, breaks = stats::quantile(x, na.rm = T, probs= c(0.5, 1, 1.5))) # give error message- need to test this 
  expect_equal("Error in set_bins(x, breaks = stats::quantile(x, na.rm = T, probs = c(0.5)),  : \n  can only have 2 breaks to use the between functionality",
               res)
  expect_error()
})

# Upper and lower bounds argument -----------------------------------------

## Lower bound ------------------------------------------------------------
# lower_bound default -Inf
test_that("lower_bound test: -Inf [MRG-MISC-0122]", {
  x <- Theoph$conc
  res <- set_bins(x, breaks = stats::quantile(x, na.rm = T, probs= c(0.4, 0.5, 1)), lower_bound = -Inf)
  expect_true(all(!is.na(res)))
  expect_equal(
    sort(unique(res)),
    c(0,1,2)
  )
})

# if 1st break > lower_bound and lower_bound is not NA, then lower_bound overrides 1st break
test_that("lower_bound test: discrete min number, break[1] > lower_bound [MRG-MISC-0122]", {
  x <- Theoph$conc
  xbreak <- stats::quantile(x, na.rm = T, probs= c(min(x) + 0.4, 0.5, 1))
  res <- set_bins(x, breaks = xbreak, lower_bound = min(x))
  expect_gt(min(xbreak),
            min(x))
  expect_true(all(!is.na(res)))
  expect_equal(
    sort(unique(res)),
    c(0,1,2)
  )
})

# if lower_bound > actual min, then all obs less than lower_bound put into NA bin
test_that("lower_bound test: discrete number > actual min [MRG-MISC-0122]", {
  x <- Theoph$conc
  res <- set_bins(x, breaks = stats::quantile(x, na.rm = T, probs= c(min(x) + 0.1, 0.5, 1)), lower_bound = min(x)+ 0.1)
  expect_false(all(!is.na(res)))
  expect_equal(
    sort(unique(res)),
    c(0,1,2)
  )
})

 
## Upper bound ------------------------------------------------------------
#upper_bound default: Inf
test_that("upper_bound test: -Inf [MRG-MISC-0122]", {
  x <- Theoph$conc
  res <- set_bins(x, upper_bound = Inf)
  expect_true(all(!is.na(res)))
  expect_equal(
    sort(unique(res)),
    c(1, 2, 3, 4)
  )
})

# if last break < upper_bound and upper_bound is not NA, then upper_bound overrides last break
test_that("upper_bound test: discrete max number, break[n] > upper_bound [MRG-MISC-0122]", {
  x <- Theoph$conc
  xbreak <- stats::quantile(x, na.rm = T, probs= c(0.4, 0.5, 0.8))
  res <- set_bins(x, breaks = xbreak, upper_bound = max(x)) 
  expect_lt(max(xbreak),
            max(x))
  expect_true(all(!is.na(res)))
  expect_equal(
    sort(unique(res)),
    c(0, 1, 2, 3)
  )
})

# if upper_bound < actual max, then all obs greater than upper_bound put into NA bin
test_that("upper_bound test: discrete number < actual max [MRG-MISC-0122]", {
  x <- Theoph$conc
  xbreak <- stats::quantile(x, na.rm = T, probs= c(0.4, 0.5, 0.8))
  res <- set_bins(x, breaks = xbreak, upper_bound = max(x)-0.2) 
  expect_lt(max(xbreak),
            max(x))
  expect_false(all(!is.na(res)))
  expect_equal(
    sort(unique(res)),
    c(0, 1, 2, 3)
  )
})

# Inclusive argument ------------------------------------------------------

#include max value of largest user defined bin even though lower bins are non-inclusive
test_that("upper_bound test: discrete number < actual max [MRG-MISC-0123]", {
  x <- Theoph$conc
  xbreak <- stats::quantile(x, na.rm = T, probs= c(0, 0.5, 0.8))
  
  res <- set_bins(x, breaks = xbreak, upper_bound = 5, inclusive = TRUE) 
  res2 <- set_bins(x, breaks = xbreak, upper_bound = 5, inclusive = FALSE) 
  expect_equal(
          res,
          res2
  )
  expect_lt(max(xbreak),
            max(x))
  expect_false(all(!is.na(res)))
  expect_equal(
    sort(unique(res)),
    c(0, 1, 2, 3)
  )
})


if (inclusive) {
  top_user_bin_index <- ifelse(upper_bound == Inf, length(breaks) - 1, length(breaks))
  top_bin <- breaks[top_user_bin_index]
  breaks[top_user_bin_index] <- top_bin + top_bin*0.0001
}



# Between argument --------------------------------------------------------

#' To use the between functionality, you must specify the range you wish to bin between,
#' and those values will be assigned to bin 1, with all values below as 0 and all values
#' above as 2. See the examples for more details
if (!is.null(between)) {
  if(length(between) != 2) {
    stop("can only have 2 breaks to use the between functionality")
  }
  x_bins <- ifelse(dplyr::between(x, between[1], between[2]), 1, 
                   ifelse(x > between[2], 2, 0))
  
  test_that("between test: discrete number []", {
    x <- Theoph$conc
    res <- set_bins(x, breaks = stats::quantile(x, na.rm = T, probs= c(0.4, 0.5, 1)), upper_bound = 5) 
    res2 <- set_bins(x, breaks = stats::quantile(x, na.rm = T, probs= c(0.4, 0.5, 1)), upper_bound = Inf)
    expect_true(all(!is.na(res)))
    expect_equal(
      sort(unique(res)),
      c(0, 1, 2)
    )
    expect_equal(
      res,
      res2
    )
  })

# try to get NAs
test_that("upper_bound test: discrete number [MISC-BINS-002]", {
  x <- Theoph$conc
  res <- set_bins(x, breaks = stats::quantile(x, na.rm = T, probs= c(0.4, 0.5, 1)), upper_bound = 5) #if upper bound set to be less than all data, then ignores it?
  res2 <- set_bins(x, breaks = stats::quantile(x, na.rm = T, probs= c(0.4, 0.5, 1)), upper_bound = Inf)
  expect_true(all(!is.na(res)))
  expect_equal(
    sort(unique(res)),
    c(0, 1, 2)
  )
  expect_equal(
    res,
    res2
  )
})

# Quiet argument ----------------------------------------------------------
# need to work on this//// 

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

test_that("works quiet true: []", {
  x <- Theoph$conc
  res <- set_bins(x, breaks = stats::quantile(x, na.rm = T, probs= c(0, 0.5, 1)), quiet = TRUE)
  expect_message(res)
  # need to add sink here maybe?
})

test_that("works quiet false: []", {
  x <- Theoph$conc
  res <- set_bins(x, breaks = stats::quantile(x, na.rm = T, probs= c(0, 0.5, 1)), quiet = FALSE)
  expect_equal(
    sort(unique(res)),
    c(1,2)
  )
  # need to add sink here maybe?
})

test_that("works quiet false: []",{
  
  # set up tempfile to sink output to
  .f <- tempfile()
  withr::defer(unlink(.f))
  withr::local_message_sink(.f)
  
  x <- Theoph$conc
  xres <- set_bins(x, breaks = stats::quantile(x, na.rm = T, probs= c(0, 0.5, 1)), quiet = FALSE)
  
  # print and read result from temp file
  capture.output(print(xres))
  res <- readLines(.f)
  expect_true(any(grepl_fixed(
    glue::glue("there were 4bins calculated"),
    res
  )))
})

  