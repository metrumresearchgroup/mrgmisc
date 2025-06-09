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
  skip_if_not_installed("fs")

  tdir <- withr::local_tempdir("mrgmisc-")
  fs::dir_create(tdir, "subdir")
  fname <- file.path(tdir, "subdir", "foo.R")
  cat("x <- this_file_proj()", file = fname)

  expect_error(source(fname))

  cat("Version: 1.0\n", file = file.path(tdir, "foo.Rproj"))
  source(fname)
  expect_identical(x, "subdir/foo.R")
})
