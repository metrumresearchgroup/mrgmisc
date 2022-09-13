x <- c(".", "1", "1")
x2 <- c(".", "1", "1", ".", ".")
x3 <- c(NA, 1, 1, NA, NA)
x4 <- c("1", "2", "3")

test_that("Replaces . with NA [MRG-RDOT-001]", {
  x_rep <- replace_dots(x)
  expect_true(is.na(x_rep[1]))
  
  x_rep2 <- replace_dots(x2)
  expect_true(is.na(x_rep2[4]))
})

test_that("Provides sufficient error message [MRG-RDOT-002]", {
  expect_error(replace_dots(x3))
})

test_that("Does not change string with no dots [MRG-RDOT-002]", {
  x4_dots <- replace_dots(x4)
  expect_equal(x4_dots, x4)
})
