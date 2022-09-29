context("left-padding")

test_that("pad_left expected outcome: left padding works [MRG-PADL-001]", {
  expect_equal(pad_left(1, 3), "001")
  expect_equal(pad_left(1, 1, 1), "1")
  expect_equal(pad_left(c(1, 10, 100), 3), c("001", "010", "100" ))
  expect_equal(pad_left(c(1, 10, 1000), 3), c("001", "010", "1000" ))
  expect_equal(pad_left(c(1, 10), 4, "Z"), c("ZZZ1", "ZZ10"))
})

test_that("pad_left expected outcome: Appropriate error message occurs [MRG-PADL-001]", {
  expect_error(pad_left(1, -1), "Number of characters must be positive")
})

test_that("pad_left expected outcome: Length of padding characters overrides length of result vector [MRG-PADL-001]", {
  expect_equal(pad_left(1, 2, "LBSOFP"), "LBSOFP1")
})
