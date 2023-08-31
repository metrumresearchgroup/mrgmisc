Theoph2 <- Theoph
Theoph2$Subject[2:4] = NA_real_
Theoph2$conc[5:9] = NA_real_
Theoph2$Time = NA_real_

test_that("Finds NA within dataframe", {
  Theoph_na <- nasum(Theoph2)
  expect_equal(Theoph_na[1], dplyr::tibble(name = c("Subject", "Time", "conc")))
  expect_equal(Theoph_na[2], dplyr::tibble(n_NA = c(3, 132, 5)))
})
