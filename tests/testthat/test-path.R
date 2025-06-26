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

test_that("set path options", {
  skip_if_not_installed("this.path")
  skip_if_not_installed("fs")
  
  tdir <- fs::path_real(withr::local_tempdir("mrgmisc-two"))
  cat("Version: 1.0\n", file = file.path(tdir, "foo.Rproj"))
  fs::dir_create(tdir, "deliv", "figure", "eda")
  fs::dir_create(tdir, "deliv", "table", "sims")
  fs::dir_create(tdir, "script")
  script <- file.path(tdir, "script", "foo.R")
  
  # Set script
  writeLines("mrg_script()", script)
  source(script)
  expect_identical(
    options()$mrg.script,
    file.path("script", "foo.R")
  )

  tf_options_clear(quietly = TRUE)
  expect_null(options()$mrg.script)

  # Set figure output dir and script
  writeLines(
    "figures_to('../deliv/figure/eda')",
    script
  )
  withr::with_dir(
    dirname(script), 
    source(script)
  )
  expect_identical(
    options()$mrggsave.dir, 
    file.path(tdir, "deliv", "figure", "eda")
  )
  expect_identical(
    options()$mrg.script, 
    "script/foo.R"
  )
  
  # Clear options
  tf_options_clear(quietly = TRUE)
  expect_null(options()$mrg.script)
  expect_null(options()$pmtables.dir)
  expect_null(options()$mrggsave.dir)
  
  # Set table output dir
  writeLines(
    "tables_to('../deliv/table/sims')",
    script
  )
  withr::with_dir(
    dirname(script), 
    source(script)
  )
  expect_identical(
    options()$pmtables.dir, 
    file.path(tdir, "deliv", "table", "sims")
  )
  expect_identical(
    options()$mrg.script, 
    "script/foo.R"
  )
  
  # Clear
  expect_message(tf_options_clear(), regexp = "option not set")
  expect_null(options()$mrg.script)
  expect_null(options()$pmtables.dir)
  expect_null(options()$mrggsave.dir)
  
  # Set table output dir, but not script
  writeLines(
    "tables_to('../deliv/table/sims', set_script = FALSE)",
    script
  )
  withr::with_dir(
    dirname(script), 
    source(script)
  )
  expect_identical(
    options()$pmtables.dir, 
    file.path(tdir, "deliv", "table", "sims")
  )
  expect_null(options()$mrg.script)
  
  # Clear
  tf_options_clear(quietly = TRUE)
  
  # Set output figure dir, but not script
  writeLines(
    "figures_to('../deliv/figure/eda', set_script = FALSE)",
    script
  )
  withr::with_dir(
    dirname(script), 
    source(script)
  )
  expect_null(options()$mrg.script)
  
  tf_options_clear(quietly = TRUE)

  # tables_to warns when path does not exist

  writeLines("tables_to('foo/bar')", script)
  expect_warning(
    source(script),
    regexp = "The table output path could not be found"
  )

  # figures_to warns when path does not exist

  writeLines("figures_to('foo/bar')", script)
  expect_warning(
    source(script),
    regexp = "The figure output path could not be found"
  )
})
