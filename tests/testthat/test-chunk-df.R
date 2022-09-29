test_that("chunk_df unexpected input: Error message occurs if input is not correct type [MRG-CKDF-002]", {
  expect_error(chunk_df(c(1, 2, 3, 4)))
  expect_error(chunk_df(Theoph, .nchunks = 0))
  expect_error(chunk_df(Theoph, .nchunks = -1))
  expect_error(chunk_df(Theoph, 14, .nchunks = 5), "grouping vars passed to dots must be a single character vector or unquoted column names")
})

test_that("chunk_df expected output: Expected number of chunks is created [MRG-CKDF-001]", {
  chk <- chunk_df(Theoph, .nchunks = 6)
  expect_equal(length(chk), 6)
  
  chk2 <- chunk_df(Theoph, .nchunks = 133)
  expect_equal(length(chk2), nrow(Theoph))
})

test_that("chunk_df unexpected input: Column added to dataframe if list set to false [MRG-CKDF-002]", {
  chk_df <- Theoph %>% dplyr::group_by(Subject) %>% chunk_df(.nchunks = 3, .as_list = FALSE)
  expect_equal(length(unique(chk_df$chunk__)), 3)
})

test_that("chunk_df expected output: Chunk df with column name [MRG-CKDF-001]", {
  chk_sub <- chunk_df(Theoph, Subject, .nchunks = 3)
  expect_equal(length(chk_sub), 3)
})
