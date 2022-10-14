test_that("pool_df returns the shared names across two data.frames [MRG-PODF-001]", {
  Theoph_mod <- Theoph %>% dplyr::mutate(NEWCOL = 1) %>% dplyr::select(-Dose)
  comp <- pool_df(x = Theoph, y = Theoph_mod)
  expect_equal(comp$shared, c("Subject", "Wt", "Time", "conc"))
})

test_that("pool_df returns the unshared names across data.frames [MRG-PODF-002]", {
  Theoph_mod <- Theoph %>% dplyr::mutate(NEWCOL = 1) %>% dplyr::select(-Dose)
  comp <- pool_df(x = Theoph, y = Theoph_mod)
  expect_equal(comp$`in x not y`, c("Dose"))
  expect_equal(comp$`in y not x`, c("NEWCOL"))
})