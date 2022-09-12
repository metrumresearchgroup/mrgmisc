test_that("read nmctl works as anticipated", {
  ctl_z <- read.nmctl(here::here("inst/extdata/exam_ctl.ctl"))
  expect_equal(ctl_z$prob, "RUN# 100 - fit of Phase I data base model")
  expect_equal(ctl_z$pk[5], "KA   = EXP(THETA(1)+ETA(1))")
})

test_that("as character nmctl reader works", {
  ctl_z <- read.nmctl(here::here("inst/extdata/exam_ctl.ctl"))
  expect_equal(as.character.nmctl(ctl_z$omega)[1], " BLOCK(3)")
})
  