test_that("parens works as expected [MRG-PRNS-001]", {
  expect_equal(parens('x'), "(x)")
  expect_equal(parens('()'), "(())")
  expect_equal(parens(1), "(1)")
})
