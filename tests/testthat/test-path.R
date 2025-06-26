skip_if_not_installed("this.path")
skip_if_not_installed("fs")

test_that("return current file name", {
  x <- this_file_name()
  expect_identical(x, "test-path.R")
})

test_that("return current file name with absolute path", {
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
  tdir <- fs::path_real(withr::local_tempdir("mrgmisc-two"))
  cat("Version: 1.0\n", file = file.path(tdir, "foo.Rproj"))
  fs::dir_create(tdir, "deliv", "figure", "eda")
  fs::dir_create(tdir, "deliv", "table", "sims")
  fs::dir_create(tdir, "script")
  script <- file.path(tdir, "script", "foo.R")
  
  withr::local_dir(dirname(script))

  # Set script
  writeLines(
    c(
      "mrg_script()",
      "tf_options()"
    ),
    script
  )

  source_quietly <- purrr::quietly(source)

  res <- source_quietly(script)
  expect_length(res[["warnings"]], 0)
  expect_match(
    res[["messages"]],
    paste0("script  ", file.path("script", "foo.R")),
    all = FALSE,
    fixed = TRUE
  )
  expect_match(
    res[["messages"]],
    "tables .*not set",
    all = FALSE
  )
  expect_match(
    res[["messages"]],
    "figures .*not set",
    all = FALSE
  )

  expect_identical(
    options()$mrg.script,
    file.path("script", "foo.R")
  )

  tf_options_clear(quietly = TRUE)
  expect_null(options()$mrg.script)

  # Set figure output dir and script
  writeLines(
    c(
      "figures_to('../deliv/figure/eda')",
      "tf_options()"
    ),
    script
  )
  res <- source_quietly(script)
  expect_length(res[["warnings"]], 0)
  expect_match(
    res[["messages"]],
    paste0("script  ", file.path("script", "foo.R")),
    all = FALSE,
    fixed = TRUE
  )
  expect_match(
    res[["messages"]],
    "tables .*not set",
    all = FALSE
  )
  expect_match(
    res[["messages"]],
    paste0("figures ", file.path("deliv", "figure", "eda")),
    all = FALSE,
    fixed = TRUE
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
    c(
      "tables_to('../deliv/table/sims')",
      "tf_options()"
    ),
    script
  )
  res <- source_quietly(script)
  expect_length(res[["warnings"]], 0)
  expect_match(
    res[["messages"]],
    paste0("script  ", file.path("script", "foo.R")),
    all = FALSE,
    fixed = TRUE
  )
  expect_match(
    res[["messages"]],
    paste0("tables  ", file.path("deliv", "table", "sims")),
    all = FALSE,
    fixed = TRUE
  )
  expect_match(
    res[["messages"]],
    "figures .*not set",
    all = FALSE
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
  tf_options_clear(quietly = TRUE)
  expect_null(options()$mrg.script)
  expect_null(options()$pmtables.dir)
  expect_null(options()$mrggsave.dir)
  
  # Set table output dir, but not script
  writeLines(
    c(
      "tables_to('../deliv/table/sims', set_script = FALSE)",
      "tf_options()"
    ),
    script
  )
  res <- source_quietly(script)
  expect_length(res[["warnings"]], 0)
  expect_match(
    res[["messages"]],
    "script .*not set",
    all = FALSE
  )
  expect_match(
    res[["messages"]],
    paste0("tables  ", file.path("deliv", "table", "sims")),
    all = FALSE,
    fixed = TRUE
  )
  expect_match(
    res[["messages"]],
    "figures .*not set",
    all = FALSE
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
    c(
      "figures_to('../deliv/figure/eda', set_script = FALSE)",
      "tf_options()"
    ),
    script
  )
  res <- source_quietly(script)
  expect_length(res[["warnings"]], 0)
  expect_match(
    res[["messages"]],
    "script .*not set",
    all = FALSE
  )
  expect_match(
    res[["messages"]],
    "tables .*not set",
    all = FALSE
  )
  expect_match(
    res[["messages"]],
    paste0("figures ", file.path("deliv", "figure", "eda")),
    all = FALSE,
    fixed = TRUE
  )
  expect_null(options()$mrg.script)
  
  tf_options_clear(quietly = TRUE)

  # tables_to warns when path does not exist

  writeLines(
    c(
      "tables_to('foo/bar')",
      "tf_options()"
    ),
    script
  )
  res <- source_quietly(script)
  expect_match(
    res[["warnings"]],
    "The table output path could not be found"
  )
  expect_match(
    res[["messages"]],
    paste0("script  ", file.path("script", "foo.R")),
    all = FALSE,
    fixed = TRUE
  )
  expect_match(
    res[["messages"]],
    "tables .*does not exist",
    all = FALSE
  )
  expect_match(
    res[["messages"]],
    "figures .*not set",
    all = FALSE
  )

  tf_options_clear(quietly = TRUE)

  # figures_to warns when path does not exist

  writeLines(
    c(
      "figures_to('foo/bar')",
      "tf_options()"
    ),
    script
  )
  res <- source_quietly(script)
  expect_match(
    res[["warnings"]],
    "The figure output path could not be found"
  )
  expect_match(
    res[["messages"]],
    paste0("script  ", file.path("script", "foo.R")),
    all = FALSE,
    fixed = TRUE
  )
  expect_match(
    res[["messages"]],
    "tables .*not set",
    all = FALSE
  )
  expect_match(
    res[["messages"]],
    "figures .*does not exist",
    all = FALSE
  )

  tf_options_clear(quietly = TRUE)
})
