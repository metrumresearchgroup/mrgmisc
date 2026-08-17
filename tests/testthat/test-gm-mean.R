test_that("gm_mean computes geometric mean for positive values", {
  expect_equal(gm_mean(c(1, 4, 16)), 4)
  expect_equal(gm_mean(c(2, 8)), 4)
  expect_equal(gm_mean(5), 5)
})

test_that("gm_mean rejects negative values (returns NaN)", {
  expect_true(is.nan(gm_mean(c(-1, 1, 4))))
  expect_true(is.nan(gm_mean(c(-1, NA), na.rm = TRUE)))
  expect_true(is.nan(gm_mean(c(NA, -0.1, 2), na.rm = TRUE)))
})

test_that("gm_mean handles NA values according to na.rm", {
  expect_identical(gm_mean(c(1, NA, 4), na.rm = FALSE), NA_real_)
  expect_equal(gm_mean(c(1, NA, 4), na.rm = TRUE), 2)
  expect_identical(gm_mean(c(NA_real_), na.rm = FALSE), NA_real_)
  expect_true(is.nan(gm_mean(c(NA_real_), na.rm = TRUE))) # no positives left
})

test_that("gm_mean handles zeros with and without zero.propagate", {
  # zeros ignored by default: use only positives
  expect_equal(gm_mean(c(0, 1, 4)), 2)
  
  # propagate zero if requested (after NA handling)
  expect_identical(gm_mean(c(0, 1, 4), zero.propagate = TRUE), 0)
  
  # all zeros -> no positive values => NaN unless propagate
  expect_true(is.nan(gm_mean(c(0, 0, 0), zero.propagate = FALSE)))
  expect_identical(gm_mean(c(0, 0, 0), zero.propagate = TRUE), 0)
})

test_that("gm_mean's zero propagation occurs after NA policy", {
  # If na.rm = FALSE and any NA exists, NA wins even if zero exists
  expect_identical(gm_mean(c(0, NA, 1), na.rm = FALSE, zero.propagate = TRUE), NA_real_)
  
  # If na.rm = TRUE, NA removed, then zero propagation can trigger
  expect_identical(gm_mean(c(0, NA, 1), na.rm = TRUE, zero.propagate = TRUE), 0)
})

test_that("gm_mean returns NaN when there are no positive values", {
  expect_true(is.nan(gm_mean(numeric(0))))
  expect_true(is.nan(gm_mean(c(0))))
  expect_true(is.nan(gm_mean(c(0, NA), na.rm = TRUE)))
})

test_that("gm_mean coerces inputs with as.numeric()", {
  expect_equal(gm_mean(c("1", "4", "16")), 4)
  
  # Non-numeric strings become NA; with na.rm = FALSE this returns NA_real_
  expect_identical(suppressWarnings(gm_mean(c("1", "x", "4"), na.rm = FALSE)), NA_real_)
  
  # With na.rm = TRUE, drops the NA introduced by coercion
  expect_equal(suppressWarnings(gm_mean(c("1", "x", "4"), na.rm = TRUE)), 2)
})

test_that("gm_mean behaves well with Inf", {
  # log(Inf) = Inf, mean(Inf, ...) = Inf, exp(Inf) = Inf
  expect_true(is.infinite(gm_mean(c(1, Inf))))
  # If only Inf is positive, still Inf
  expect_true(is.infinite(gm_mean(c(Inf))))
})
