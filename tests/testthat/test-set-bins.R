
test_that("works with breaks out of order []", {
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

test_that("works with 2 bins []", {
  x <- Theoph$conc
  res <- set_bins(x, breaks = stats::quantile(x, na.rm = T, probs= c(0, 0.5, 1)))
  expect_equal(
    sort(unique(res)),
    c(1,2)
  )
})

test_that("works with default 4 bins if not bin number not specified []", {
  x <- Theoph$conc
  res <- set_bins(x) 
  expect_equal(
    sort(unique(res)),
    c(1,2, 3, 4)
  )
})

# quantile default out of range
test_that("works with 2 bins []", {
  x <- Theoph$conc
  res <- set_bins(x, breaks = stats::quantile(x, na.rm = T, probs= c(0.5, 1, 1.5))) # give error message- need to test this 
  expect_equal("Error in set_bins(x, breaks = stats::quantile(x, na.rm = T, probs = c(0.5)),  : \n  can only have 2 breaks to use the between functionality",
               res)
  expect_equal(
    sort(unique(res)),
    c(1,2)
  )
})

# lower and upper bounds tests
## lower
test_that("lower_bound test: -Inf [MISC-BINS-002]", {
  x <- Theoph$conc
  res <- set_bins(x, breaks = stats::quantile(x, na.rm = T, probs= c(0.4, 0.5, 1)))
  expect_true(all(!is.na(res)))
  expect_equal(
    sort(unique(res)),
    c(0,1,2)
  )
})

test_that("lower_bound test: discrete min number [MISC-BINS-002]", {
  x <- Theoph$conc
  res <- set_bins(x, breaks = stats::quantile(x, na.rm = T, probs= c(0.4, 0.5, 1)), lower_bound = min(x))
  expect_true(all(!is.na(res)))
  expect_equal(
    sort(unique(res)),
    c(0,1,2)
  )
})

test_that("lower_bound test: discrete number [MISC-BINS-002]", {
  x <- Theoph$conc
  res <- set_bins(x, breaks = stats::quantile(x, na.rm = T, probs= c(0.4, 0.5, 1)), lower_bound = min(x)+ 0.1)
  expect_false(all(!is.na(res)))
  expect_equal(
    sort(unique(res)),
    c(0,1,2)
  )
})

## upper
test_that("upper_bound test: -Inf [MISC-BINS-002]", {
  x <- Theoph$conc
  res <- set_bins(x, upper_bound = Inf)
  expect_true(all(!is.na(res)))
  expect_equal(
    sort(unique(res)),
    c(1, 2, 3, 4)
  )
})


test_that("upper_bound test: discrete max number [MISC-BINS-002]", {
  x <- Theoph$conc
  res <- set_bins(x, breaks = stats::quantile(x, na.rm = T, probs= c(0.4, 0.5, 1)), upper_bound = min(x)-0.1) #if upper bound set to be less than all data, then ignores it?
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

# quiet functionality 

test_that("works quiet false: []", {
  x <- Theoph$conc
  res <- set_bins(x, breaks = stats::quantile(x, na.rm = T, probs= c(0, 0.5, 1)), quiet = FALSE)
  expect_equal(
    sort(unique(res)),
    c(1,2)
  )
  # need to add sink here maybe?
})
