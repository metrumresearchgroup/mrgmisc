test_that("df_subject_check returns expected outputs", {
  checks <- df_subject_check(Theoph %>% dplyr::mutate(EVID = 1), .subject_col = "Subject", .dose_col = "Dose")
  expect_true(!checks$OneRowPerSubject)
  expect_true(checks$OneDosePerSubject)
  expect_true(!checks$MultipleEvidPerSubject)
  
  checks <- df_subject_check(
    Theoph %>% dplyr::mutate(EVID = 1:dplyr::n(), DOSE = conc, ID = Subject))
  expect_true(!checks$OneRowPerSubject)
  expect_true(!checks$OneDosePerSubject)
  expect_true(checks$MultipleEvidPerSubject)
  
  checks <- df_subject_check(
    Theoph %>% dplyr::mutate(EVID = 1) %>% dplyr::group_by(Subject) %>% dplyr::slice(1), 
    .subject_col = "Subject", 
    .dose_col = "Dose")
  expect_true(checks$OneRowPerSubject)
  expect_true(checks$OneDosePerSubject)
  expect_true(!checks$MultipleEvidPerSubject)
})

