
writeTemp <- tempfile(fileext = ".csv")

test_that("write_nonmem writes file to correct location", {
  write_nonmem(Theoph, file = writeTemp)
  expect_true(file.exists(writeTemp))
})

test_that("write_nonmem special cases: NA is converted to dots", {
  Theoph$Dose[2] = NA_real_
  write_nonmem(Theoph, file = writeTemp)
  Theoph2 <- readr::read_csv(writeTemp)
  expect_true(Theoph2$Dose[2] == ".")
})

test_that("write_nonmem special cases: Custom characters for NA", {
  Theoph$Dose[2] = NA_real_
  write_nonmem(Theoph, file = writeTemp, na = "&")
  Theoph2 <- readr::read_csv(writeTemp)
  expect_true(Theoph2$Dose[2] == "&")
})
