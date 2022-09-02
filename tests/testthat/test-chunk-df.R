test_that("Error message occurs if input is not correct type", {
  expect_error(chunk_df(c(1, 2, 3, 4)))
})

test_that("Expected number of chunks is created", {
  chk <- chunk_df(Theoph, .nchunks = 6)
  expect_equal(length(chk), 6)
})
