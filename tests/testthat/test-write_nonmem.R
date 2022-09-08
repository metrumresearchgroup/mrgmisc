test_that("File is written to correct location", {
  write_nonmem(Theoph, file = here::here("inst/extdata/nonmem.csv"))
  expect_true(file.exists(here::here("inst/extdata/nonmem.csv")))
})

test_that("NA is converted to dots", {
  Theoph$Dose[2] = NA_real_
  write_nonmem(Theoph, file = here::here("inst/extdata/nonmem.csv"))
  Theoph2 <- readr::read_csv(here::here("inst/extdata/nonmem.csv"))
  expect_true(Theoph2$Dose[2] == ".")
})

test_that("Custom characters for NA", {
  Theoph$Dose[2] = NA_real_
  write_nonmem(Theoph, file = here::here("inst/extdata/nonmem.csv"), na = "&")
  Theoph2 <- readr::read_csv(here::here("inst/extdata/nonmem.csv"))
  expect_true(Theoph2$Dose[2] == "&")
})

file.remove(here::here("inst/extdata/nonmem.csv"))