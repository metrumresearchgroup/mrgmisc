
# Break argument ----------------------------------------------------------

test_that("set_bins expected output: Breaks out of order", {
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
test_that("set_bins expected output: 1 break provided", {
  x <- Theoph$conc
  res <- set_bins(x, breaks = stats::quantile(x, na.rm = T, probs= c(0.2)))
  res2 <- set_bins(x, breaks = stats::quantile(x, na.rm = T, probs= c(0.2)),lower_bound = -Inf, upper_bound = Inf)
  expect_equal(
    sort(unique(res)),
    c(0,1) 
  )
  expect_equal(
    res,
    res2
  )
})

# if no break specified, do quartiles
test_that("set_bins expected output: Default 4 bins quartiles if break not specified", {
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

# Upper and lower bounds argument -----------------------------------------

## Lower bound ------------------------------------------------------------
# lower_bound default -Inf
test_that("set_bins bound tests: lower_bound -Inf", {
  x <- Theoph$conc
  res <- set_bins(x, breaks = stats::quantile(x, na.rm = T, probs= c(0.4, 0.5, 1)), lower_bound = -Inf)
  expect_true(all(!is.na(res)))
  expect_equal(
    sort(unique(res)),
    c(0,1,2)
  )
})

# if 1st break > lower_bound and lower_bound is not NA, then lower_bound overrides 1st break
test_that("set_bins bound tests: Discrete min number, break[1] > lower_bound", {
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
test_that("set_bins bound tests: Discrete number > actual min", {
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
test_that("set_bins bound tests: upper_bound -Inf", {
  x <- Theoph$conc
  res <- set_bins(x, upper_bound = Inf)
  expect_true(all(!is.na(res)))
  expect_equal(
    sort(unique(res)),
    c(1, 2, 3, 4)
  )
})

# if last break < upper_bound and upper_bound is not NA, then upper_bound overrides last break
test_that("set_bins bound tests: Discrete max number, break[n] > upper_bound", {
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
test_that("set_bins bound tests: Discrete number < actual max", {
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
test_that("set_bins inclusive tests: inclusive = TRUE and upper bound = Inf, n bins = n breaks -1", {
  x <- Theoph$conc
  xbreak <- stats::quantile(x, na.rm = T, probs= c(0, 0.5, 1))
  xupper = Inf
  res <- set_bins(x, breaks = xbreak, upper_bound = xupper, inclusive = TRUE)
  expect_equal(
    length(xbreak)-1,
    length(unique(res))
  )
})

test_that("set_bins inclusive tests: inclusive = FALSE and upper bound = Inf, n bins = n breaks", {
  x <- Theoph$conc
  xbreak <- stats::quantile(x, na.rm = T, probs= c(0, 0.5, 1))
  xupper = Inf
  res <- set_bins(x, breaks = xbreak, upper_bound = xupper, inclusive = FALSE)
  expect_equal(
    length(xbreak),
    length(unique(res))
  )
})

test_that("set_bins inclusive tests: inclusive = TRUE and discrete upper bound, n bins = n breaks -1", {
  x <- Theoph$conc
  xbreak <- stats::quantile(x, na.rm = T, probs= c(0, 0.5, 1))
  xupper = max(x)-2
  res <- set_bins(x, breaks = xbreak, upper_bound = xupper, inclusive = TRUE)
  expect_equal(
    length(xbreak)-1,
    length(unique(res))
  )
})

test_that("set_bins inclusive tests: inclusive = FALSE and discrete upper bound, n bins = n breaks -1", {
  x <- Theoph$conc
  xbreak <- stats::quantile(x, na.rm = T, probs= c(0, 0.5, 1))
  xupper = max(x)-2
  res <- set_bins(x, breaks = xbreak, upper_bound = xupper, inclusive = FALSE)
  expect_equal(
    length(xbreak) -1,
    length(unique(res))
  )
})

# Between argument --------------------------------------------------------

# if val between c(min_between, max_between), then bin 1. Less than min_between, then bin 0. Greater than max_between, then bin 2
  test_that("set_bins between tests: Discrete number with between argument", {
    x <- Theoph$conc
    min_between <- min(x) + 1
    max_between <- max(x) - 5
    res <- set_bins(x,  between = c(min_between, max_between)) 
    res2 <- ifelse(dplyr::between(x, min_between, max_between), 1, 
                   ifelse(x > max_between, 2, 0))
    expect_equal(
      res,
      res2
    )
    expect_true(all(!is.na(res)))
  })

test_that("set_bins between tests: Discrete number with between argument all > max val", {
  x <- Theoph$conc
  min_between <- max(x) + 1
  max_between <- max(x) + 5
  res <- set_bins(x,  between = c(min_between, max_between)) 
  res2 <- ifelse(dplyr::between(x, min_between, max_between), 1, 
                 ifelse(x > max_between, 2, 0))
  expect_equal(
    res,
    res2
  )
  expect_true(all(!is.na(res)))
})

test_that("set_bins between tests: Discrete number with between argument all < min val", {
  x <- Theoph$conc
  min_between <- min(x) + 1
  max_between <- min(x) - 5
  res <- set_bins(x,  between = c(min_between, max_between)) 
  res2 <- ifelse(dplyr::between(x, min_between, max_between), 1, 
                 ifelse(x > max_between, 2, 0))
  expect_equal(
    res,
    res2
  )
  expect_true(all(!is.na(res)))
})

# error message if between length(between) != 2
  test_that("set_bins between tests: Error length(between) !=2", {
    x <- Theoph$conc
    expect_error(set_bins(x,  between = c(1)),
                 "can only have 2 breaks to use the between functionality") 
  })

# Quiet argument ----------------------------------------------------------

test_that("set_bins quiet tests: quiet= false print message", {
  x <- Theoph$conc
  xbreaks = stats::quantile(x, na.rm = T, probs= c(0, 0.5, 1))
  res <- set_bins(x, breaks= xbreaks, quiet = FALSE)
  xbin <- length(xbreaks) + 1
  expect_message(any(grepl(
    glue::glue("there were {xbin} bins calculated, with the following range for each bin:
               BIN: 0 range: -Inf - {xbreaks[1]}
               BIN: 1 range: 0 - {xbreaks[2]}
               BIN: 2 range: {xbreaks[2]} - {xbreaks[3]}
               BIN: 3 range: {xbreaks[3]} - Inf"),
    set_bins(x, breaks = xbreaks, quiet = FALSE),
    fixed =TRUE
  )))
})

  test_that("set_bins quiet tests: quiet= between argument and false print message", {
    x <- Theoph$conc
    xbreaks = stats::quantile(x, na.rm = T, probs= c(0, 0.5, 1))
    res <- set_bins(x, breaks= xbreaks, quiet = FALSE, between = c(5, 8))
    xbin <- as.numeric(paste0(length(xbreaks) + 1))
    expect_message(any(grepl(
      glue::glue("there were {xbin} bins calculated, with the following range for each bin:
               BIN: 0 range: All values less than {xbreaks[1]}
               BIN: 1 range: All values between {xbreaks[1]} and {xbreaks[3]}
               BIN: 2 range: All values greater than {xbreaks[3]}"),
      set_bins(x, breaks = xbreaks, quiet = FALSE),
      fixed =TRUE
    )))
  })
