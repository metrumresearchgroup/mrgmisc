test_that("s_pauc expected output: Correct column names are made [MRG-SPAC-001]", {
  df1 <- sd_oral_richpk  %>% dplyr::group_by(ID) %>% s_pauc(Time, Conc, list(c(0,8), c(8, 24)))
  expect_equal(df1$pAUC0_8[1], setNames(204 + 0.21 - 0.000439, "pAUC0_8"))
  expect_equal(df1$pAUC8_24[1], setNames(181 + 0.366345, "pAUC8_24"))
})

test_that("s_pauc expected output: Overlapping interval is okay [MRG-SPAC-001]", {
  df1 <- sd_oral_richpk  %>% dplyr::group_by(ID) %>% s_pauc(Time, Conc, list(c(0,8), c(6, 24)))
  expect_equal(df1$pAUC6_24[1], setNames(228.2556 + 1.05e-05, "pAUC6_24"))
})

test_that("s_pauc expected output: Digits work as intended [MRG-SPAC-001]", {
  df1 <- sd_oral_richpk  %>% dplyr::group_by(ID) %>% s_pauc(Time, Conc, list(c(0,8), c(8, 24)), digits = 0)
  expect_equal(df1$pAUC0_8[1], setNames(204, "pAUC0_8"))
})

test_that("s_pauc expected output: Correct column names are made for Theoph [MRG-SPAC-001]", {
  df1 <- Theoph  %>% dplyr::group_by(Subject) %>% s_pauc(Time, conc, list(c(0,8), c(8, 24), c(24, 36)), digits = 1)
  expect_equal(df1$pAUC0_8[1], setNames(34.7, "pAUC0_8"))
})
