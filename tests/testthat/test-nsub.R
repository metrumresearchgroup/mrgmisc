test_that("nsub accurately determines the number of patients in a data set [MRG-NSUB-001]", {
  expect_equal(nsub(.df = Theoph, .id = Subject), 12)
  
  charID <- Theoph %>% dplyr::mutate(NEWCOL = as.character(Subject))
  expect_equal(nsub(.df = charID, .id = NEWCOL), 12)
})
