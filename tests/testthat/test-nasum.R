Theoph2 <- Theoph
Theoph2$Subject[2:4] = NA_real_
Theoph2$conc[5:9] = NA_real_
Theoph2$Time = NA_real_

test_that("Finds NA within dataframe", {
  Theoph_na <- nasum(Theoph2)
  expect_equal(Theoph_na[1], setNames(3, "Subject"))
  expect_equal(Theoph_na[2], setNames(0, "Wt"))
  expect_equal(Theoph_na[3], setNames(0, "Dose"))
  expect_equal(Theoph_na[4], setNames(132, "Time"))
  expect_equal(Theoph_na[5], setNames(5, "conc"))
})
