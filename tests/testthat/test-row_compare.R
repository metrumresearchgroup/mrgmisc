test_that("row_compare returns only columns that have differences between rows", {
  df1 <- mtcars[1:2, ]
  df2 <- mtcars[1:5, ]
  df3 <- Theoph[1:8, ]
  
  df1_rc <- row_compare(df1)
  df2_rc <- row_compare(df2)
  df3_rc <- row_compare(df3)
  
  expect_equal(ncol(df1_rc), 2)
  expect_equal(ncol(df3_rc), 2)
  
  expect_true(all(names(df1_rc) == c("wt", "qsec")))
  expect_true(identical(df2, df2_rc))
  
})
