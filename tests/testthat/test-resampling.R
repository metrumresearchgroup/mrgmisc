context("resampledf")


dapa<- sd_oral_richpk %>% capitalize_names() %>%
  dplyr::filter(TIME %in% c(0, 0.5), ID %in% 5:7)%>%
  dplyr::select(ID, AGE, TIME)

no_extras <- resample_df(dapa, key_cols = "ID", strat_cols = "AGE", key_col_name = "Key", n=5) 
has_extras <- resample_df(dapa, key_cols = "ID", strat_cols = "AGE", key_col_name = "Key", n=10)


test_that("resample_df properly recombines when it needs to make extra draws", {
  expect_equal(nrow(no_extras), 10)
  expect_equal(dplyr::n_distinct(no_extras$Key), 5)
  expect_equal(nrow(has_extras), 20)
  expect_equal(dplyr::n_distinct(has_extras$Key), 10)
})

test_that("resample_df unexpected input: Appropriate error occurs", {
  expect_error(resample_df(Theoph, key_cols = "Subject", strat_cols = 2,key_col_name = "Key", n=20),
               "To set the number of samples please explicitly specify 'n = <num>'")
})

test_that("resample_df expected output: Works with no stratification", {
  df1 <- resample_df(Theoph, key_cols = "Subject", key_col_name = "Key", n=20)
  expect_equal(nrow(df1), 220)
  expect_equal(dplyr::n_distinct(df1$Key), 20)
})

test_that("resample_df expected output: Works with no replacement", {
  df1 <- resample_df(Theoph, key_cols = "Subject", key_col_name = "Key", n=5, replace = FALSE)
  expect_equal(nrow(df1), 55)
  expect_equal(dplyr::n_distinct(df1$Key), 5)
})

test_that("resample_df unexpected input: Works with n as Null", {
  df2 <- resample_df(Theoph, key_cols = "Subject")
  expect_equal(nrow(df2), 132)
})
