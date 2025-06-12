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

test_that("this.path wrappers work from sourced script", {
  skip_if_not_installed("this.path")
  skip_if_not_installed("fs")

  tdir <- fs::path_real(withr::local_tempdir("mrgmisc-"))
  fs::dir_create(tdir, "sub", "subsub")
  script <- file.path(tdir, "sub", "subsub", "foo.R")
  writeLines(
    c(
      "fname <- this_file_name()",
      "dname <- this_dir_name()",
      "fpath <- this_file_path()",
      "dpath <- this_dir_path()",
      "fproj <- this_file_proj()",
      "dproj <- this_dir_proj()"
    ),
    script
  )

  expect_error(source(script))

  cat("Version: 1.0\n", file = file.path(tdir, "foo.Rproj"))
  source(script)

  expect_identical(fname, "foo.R")
  expect_identical(dname, "subsub")

  expect_identical(fpath, file.path(tdir, "sub", "subsub", "foo.R"))
  expect_identical(dpath, file.path(tdir, "sub", "subsub"))

  expect_identical(fproj, file.path("sub", "subsub", "foo.R"))
  expect_identical(dproj, file.path("sub", "subsub"))
})
