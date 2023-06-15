test_that("nsub accurately determines the number of patients in a data set [MRG-NSUB-001]", {
  expect_equal(nsub(.df = Theoph, .subject_col = "Subject"), 12)
  
  charID <- Theoph %>% dplyr::mutate(USUBJID = as.character(Subject))
  expect_equal(nsub(.df = charID), 12)
})

test_that("nsub will prevent user from inputting a non-character argument to .subject_col", {
  
  expect_error(nsub(.df = Theoph, .subject_col = 1), ".subject_col must be character format")
  
})

test_that("nsub will provide number of subjects whether column is numeric or character", {
  
  expect_equal(nsub(mtcars, .subject_col = "cyl"), 3)
  
})
