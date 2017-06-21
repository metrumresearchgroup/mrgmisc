context("resampledf")


dapa<-dapa_IV_oral%>%
  filter(TIME==0|TIME==0.5, ID==5|ID==6|ID==7)%>%
  select(ID, AGE, TIME)

no_extras <- resample_df(dapa, key_cols = "ID", strat_cols = "AGE", key_col_name = "Key", n=5) 
has_extras <- resample_df(dapa, key_cols = "ID", strat_cols = "AGE", key_col_name = "Key", n=10)


test_that("resample df properly recombines when it needs to make extra draws", {
  expect_equal(nrow(no_extras), 10)
  expect_equal(n_distinct(no_extras$Key), 5)
  expect_equal(nrow(has_extras), 20)
  expect_equal(n_distinct(has_extras$Key), 10)
})