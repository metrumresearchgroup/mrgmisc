test_that("pool_df returns the shared names across two data.frames", {
  Theoph_mod <- Theoph %>% dplyr::mutate(NEWCOL = 1) %>% dplyr::select(-Dose)
  comp <- pool_df(x = Theoph, y = Theoph_mod)
  expect_equal(comp$both, c("Subject", "Wt", "Time", "conc"))

  mtcars_mod <-
    mtcars %>% 
    dplyr::select(-mpg, -cyl, -disp) %>% 
    dplyr::mutate(engine = 1)

  comp2 <- pool_df(mtcars, mtcars_mod)

  expect_true(length(comp2$mtcars) == 3)
  expect_true(comp2$mtcars_mod == "engine")
})

test_that("pool_df returns the unshared names across data.frames", {
  Theoph_mod <- Theoph %>% dplyr::mutate(NEWCOL = 1) %>% dplyr::select(-Dose)
  comp <- pool_df(x = Theoph, y = Theoph_mod)
  expect_equal(comp$Theoph, c("Dose"))
  expect_equal(comp$Theoph_mod, c("NEWCOL"))
})