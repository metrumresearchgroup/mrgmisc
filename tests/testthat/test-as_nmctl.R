test_that("read nmctl works as anticipated", {
  ctl_z <- read.nmctl(system.file("extdata/exam_ctl.ctl", package = "mrgmisc"))
  expect_equal(ctl_z$prob, "RUN# 100 - fit of Phase I data base model")
  expect_equal(ctl_z$pk[5], "KA   = EXP(THETA(1)+ETA(1))")
})

test_that("nmctl writer works", {
  ctl_z <- read.nmctl(system.file("extdata/exam_ctl.ctl", package = "mrgmisc"))
  writeTemp <- tempfile(fileext = ".ctl")
  write.nmctl(ctl_z$prob, file = writeTemp)
  expect_true(file.exists(writeTemp))
})

test_that("as character nmctl reader works", {
  ctl_z <- read.nmctl(system.file("extdata/exam_ctl.ctl", package = "mrgmisc"))
  expect_equal(as.character.nmctl(ctl_z$omega)[1], " BLOCK(3)")
})  

test_that("expected output for NULL input to nmctl character", {
  expect_equal(as.character.nmctl(NULL), character(0))
})

test_that("nmctl print works", {
  ctl_z <- read.nmctl(system.file("extdata/exam_ctl.ctl", package = "mrgmisc"))
  temp_str <- suppressMessages(print.nmctl(ctl_z$prob))
  expect_equal(temp_str, "RUN# 100 - fit of Phase I data base model")
})
