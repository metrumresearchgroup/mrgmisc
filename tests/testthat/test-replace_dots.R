x <- c(".", "1", "1")
x2 <- c(".", "1", "1", ".", ".")
x3 <- c(NA, 1, 1, NA, NA)
x4 <- c("1", "2", "3")

test_that("repalce_dots replaces . with NA", {
  x_rep <- replace_dots(x)
  expect_true(is.na(x_rep[1]))
  
  x_rep2 <- replace_dots(x2)
  expect_true(is.na(x_rep2[4]))
})

test_that("replace_dots unexpected input: Provides sufficient error message", {
  expect_error(replace_dots(x3))
})

test_that("replace_dots unexpected input: Does not change string with no dots", {
  x4_dots <- replace_dots(x4)
  expect_equal(x4_dots, x4)
})
