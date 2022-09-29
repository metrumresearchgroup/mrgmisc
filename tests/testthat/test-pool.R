test_that("pool creates appropriate groupings from two vectors with similarities [MRG-POOL-001]", {
  tempPOOL <- pool(letters[1:5], letters[4:8])
  expect_equal(tempPOOL$x, c("a", "b", "c"))
  expect_equal(tempPOOL$y, c("f", "g", "h"))
  expect_equal(tempPOOL$both, c("d", "e"))
})
