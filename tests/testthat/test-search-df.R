
# Testing dataframe
test_df <-
  dplyr::tibble(
    Name = c("A", "B", "C", "C", "C", "D"),
    Value = c(12, 1, -99, 13, 5, 2)
  )

test_that("search_df provides accurate count of all columns with a specified numeric value", {
  
  search1 <- search_df(Theoph, 0)
  expect_equal(search1$name, c("Time", "conc"))
  expect_equal(names(search1), c("name", "n_0"))
  expect_equal(search1$n_0[search1$name == "Time"], 12)
  expect_equal(search1$n_0[search1$name == "conc"], 9)
  
  search2 <- search_df(Theoph, 12)
  expect_equal(search2$name, c("Subject", "Time"))
  expect_equal(names(search2), c("name", "n_12"))
  expect_equal(search2$n_12[search2$name == "Subject"], 11)
  expect_equal(search2$n_12[search2$name == "Time"], 2)
  
  search3 <- search_df(Theoph, -99)
  expect_true(nrow(search3) == 0)
  
})

test_that("search_df provides accurate count of all columns with a specified character value", {
  
  search1 <- search_df(test_df, "D")
  expect_equal(search1$name, c("Name"))
  expect_equal(names(search1), c("name", "n_D"))
  expect_equal(search1$n_D[search1$name == "Name"], 1)

  search2 <- search_df(test_df, 1)
  expect_equal(search2$name, c("Value"))
  expect_equal(names(search2), c("name", "n_1"))
  expect_equal(search2$n_1[search2$name == "Value"], 1)
  
})
