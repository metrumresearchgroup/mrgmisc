test_that("half_life accurately calculates the half life [MRG-HALF-001]", {
  expect_equal(half_life(1, 51, 13, 1), 46.51592 + 2.69e-06)
  expect_equal(half_life(12, 0, 0, 13), NaN)
  expect_equal(half_life(0, 1, 2, 0), Inf)
})
