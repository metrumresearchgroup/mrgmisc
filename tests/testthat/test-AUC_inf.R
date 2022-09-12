
test_that("AUC_inf works", {
  expect_equal(auc_inf(Theoph$Time, Theoph$conc), setNames(1062.008 + 3.16E-05, "AUC0_inf"))
  expect_true(auc_inf(Theoph$Time, Theoph$conc) > auc_partial(Theoph$Time, Theoph$conc))
})

test_that("Remove NA works", {
  df1 <- Theoph
  df1$conc[4] <- NA
  expect_warning(auc_inf(df1$Time, df1$conc))
  expect_equal(auc_inf(df1$Time, df1$conc, na.rm = FALSE), setNames(NA_real_, "AUC0_inf"))
  expect_equal(suppressWarnings(auc_inf(df1$Time, df1$conc, na.rm=TRUE)), setNames(1060.009 - 0.000468, "AUC0_inf"))
})

test_that("Correct output if all input NA", {
  df2 <- Theoph
  df2$conc <- NA_real_
  expect_equal(auc_inf(df2$Time, df2$conc), setNames(NA, "AUC0_inf"))
})

test_that("Correct output if all DV is 0", {
  df2 <- Theoph
  df2$conc <- 0
  expect_equal(auc_inf(df2$Time, df2$conc), setNames(0, "AUC0_inf"))
})

test_that("Correct output if all time is 0", {
  df2 <- Theoph
  df2$Time <- 0
  expect_error(auc_inf(df2$Time, df2$conc), "lambda_z is NA")
})

