context("non_numerics")

test_that("unique_non_numeric", {
  expect_equal(unique_non_numerics(1), double())
  expect_equal(unique_non_numerics(factor(1, labels="2")), factor(c(), levels="2"))
  expect_equal(unique_non_numerics("A"), "A")
  expect_equal(unique_non_numerics(c("B", "A")), c("A", "B"))
  expect_equal(unique_non_numerics(c("B", "A", NA)), c("A", "B"))
  expect_equal(unique_non_numerics(c(1, "B", "A", NA), na.rm=FALSE), c("A", "B", NA))
})
