test_that("return current file name", {
  skip_if_not_installed("this.path")
  x <- this_file_name()
  expect_identical(x, "test-path.R")
})

test_that("return current file name with absolute path", {
  skip_if_not_installed("this.path")
  x <- this_file_path()
  cwd <- getwd()
  file <- basename(x)
  path <- sub(file, "", x, fixed = TRUE)
  path <- sub("/$", "", path)
  path <- sub("\\$", "", path)
  expect_identical(file, "test-path.R")
  expect_identical(path, cwd)
})

test_that("return current file name with relative path", {
  skip_if_not_installed("this.path")
  skip_if_not_installed("here")
  skip_if_not_installed("fs")
  x <- this_file_here()
  expect_identical(x, "tests/testthat/test-path.R")
})
