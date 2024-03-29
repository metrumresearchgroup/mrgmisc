test_that("s_quantiles expected output: Generates correct quantiles", {
  df1 <- sd_oral_richpk %>% dplyr::group_by(Gender, Time) %>% s_quantiles(Conc, c(0.05, 0.5, 0.95))
  expect_equal(df1$Conc_q5[2], setNames(8.266324 + 4.73e-07, "5%"))
  expect_equal(df1$Conc_q95[6], setNames(95.82737, "95%"))
})

test_that("s_quantiles unexpected input: Appropriate error occurs", {
  expect_error(sd_oral_richpk %>% dplyr::group_by(Gender, Time) %>% s_quantiles(Conc, c(0.05, 0.5, 1.95)))
  expect_error(sd_oral_richpk %>% dplyr::group_by(Gender, Time) %>% s_quantiles(Conc, c(-0.05, 0.5, 1.95)))
})

test_that("s_quantiles expected output: Works with variety of quantile inputs", {
  df1 <- sd_oral_richpk %>% dplyr::group_by(Gender, Time) %>% s_quantiles(Conc, c(0.05, 0.5, 0.6, 0.7, 0.95))
  df2 <- sd_oral_richpk %>% dplyr::group_by(Gender, Time) %>% s_quantiles(Conc, c(0.05))
  expect_true(class(df1$Conc_q70) == "numeric")
  expect_true(class(df2$Conc_q5) == "numeric")
})

