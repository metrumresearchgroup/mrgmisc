
test_that("AUC_inf works as intended", {
  expect_equal(auc_inf(Theoph$Time, Theoph$conc), setNames(1062.008 + 3.16E-05, "AUC0_inf"))
  expect_true(auc_inf(Theoph$Time, Theoph$conc) > auc_partial(Theoph$Time, Theoph$conc))
})

test_that("AUC_inf NA testing: Remove NA works", {
  df1 <- Theoph
  df1$conc[4] <- NA
  expect_warning(auc_inf(df1$Time, df1$conc))
  expect_equal(auc_inf(df1$Time, df1$conc, na.rm = FALSE), setNames(NA_real_, "AUC0_inf"))
  expect_equal(suppressWarnings(auc_inf(df1$Time, df1$conc, na.rm=TRUE)), setNames(1060.009 - 0.000468, "AUC0_inf"))
})

test_that("AUC_inf NA testing: Correct output if all input NA", {
  df2 <- Theoph
  df2$conc <- NA_real_
  expect_equal(auc_inf(df2$Time, df2$conc), setNames(NA, "AUC0_inf"))
})

test_that("AUC_inf unexpected input testing: Correct output if all DV is 0", {
  df2 <- Theoph
  df2$conc <- 0
  expect_equal(auc_inf(df2$Time, df2$conc), setNames(0, "AUC0_inf"))
})

test_that("AUC_inf unexpected input testing: Correct output if all time is 0", {
  df2 <- Theoph
  df2$Time <- 0
  expect_error(auc_inf(df2$Time, df2$conc), "lambda_z is NA")
  expect_error(auc_inf(idv = c(1, 2, 4, 5, 8), dv = c(1, 0, 0, 0, 6)), "NA/NaN/Inf in 'y'")
})

