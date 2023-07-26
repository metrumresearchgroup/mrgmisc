test_that("nsub accurately determines the number of patients in a data set [MRG-NSUB-001]", {
  expect_equal(nsub(.df = Theoph, .subject_col = "Subject"), 12)
  expect_equal(nsub(.df = Theoph, .subject_col = Subject), 12)
  
  charID <- Theoph %>% dplyr::mutate(USUBJID = as.character(Subject))
  expect_equal(nsub(.df = charID, "Subject"), 12)
})

test_that("nsub will provide number of subjects whether column is numeric or character", {
  
  expect_equal(nsub(mtcars, .subject_col = "cyl"), 3)
  expect_equal(nsub(mtcars, .subject_col = cyl), 3)
  
})
