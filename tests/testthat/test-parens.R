test_that("parens works as expected", {
  expect_equal(parens('x'), "(x)")
  expect_equal(parens('()'), "(())")
  expect_equal(parens(1), "(1)")
})
