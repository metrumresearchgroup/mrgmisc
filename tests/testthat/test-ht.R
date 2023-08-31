test_that("ht outputs the top 4 and bottom 4 rows in a data set", {
  expect_equal(nrow(ht(Theoph, 4)), 8)
  
  newdf <- ht(Theoph, 4)
  expect_equal(newdf$Time[3], 0.57)
  expect_equal(newdf$conc[7], 4.57)
})
