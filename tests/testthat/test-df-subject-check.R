test_that("df_subject_check returns expected outputs", {
  expect_equal(df_subject_check(Theoph, .subject_col = "Subject"), "Data has at least 1 subject with multiple rows")
  expect_equal(df_subject_check(
    Theoph %>% dplyr::group_by(Subject) %>% dplyr::slice(1) %>% dplyr::ungroup(), .subject_col = "Subject"), 
    "Data has 1 row per subject")
  expect_equal(df_subject_check(Theoph %>% dplyr::rename(ID = Subject)), "Data has at least 1 subject with multiple rows")
  expect_equal(df_subject_check(Theoph %>% dplyr::rename(USUBJID = Subject)), "Data has at least 1 subject with multiple rows")
  
  # Check distinct values
  expect_equal(
    df_subject_check(
      Theoph, 
      .subject_col = "Subject",
      .col_check = "Dose"), 
    "Each subject only has 1 distinct value of Dose")
  
  expect_equal(
    df_subject_check(
      Theoph, 
      .subject_col = "Subject",
      .col_check = "conc"), 
    "At least 1 subject has multiple values of conc")
  
  expect_equal(
    df_subject_check(
      Theoph, 
      .subject_col = "Subject",
      .col_check = "Dose2"), 
    "Dose2 not found in data. Please check .col_check.")
})

