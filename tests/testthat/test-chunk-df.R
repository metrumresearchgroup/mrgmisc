test_that("Error message occurs if input is not correct type", {
  expect_error(chunk_df(c(1, 2, 3, 4)))
  expect_error(chunk_df(Theoph, .nchunks = 0))
  expect_error(chunk_df(Theoph, .nchunks = -1))
})

test_that("Expected number of chunks is created", {
  chk <- chunk_df(Theoph, .nchunks = 6)
  expect_equal(length(chk), 6)
  
  chk2 <- chunk_df(Theoph, .nchunks = 133)
  expect_equal(length(chk2), nrow(Theoph))
})

test_that("Column added to dataframe if list set to false", {
  chk_df <- Theoph %>% dplyr::group_by(Subject) %>% chunk_df(.nchunks = 3, .as_list = FALSE)
  expect_equal(length(unique(chk_df$chunk__)), 3)
})
