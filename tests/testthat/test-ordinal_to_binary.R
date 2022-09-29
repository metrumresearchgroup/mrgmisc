df <- dplyr::tibble(OCC = c(1, 1, 2, 3))
df2 <- dplyr::tibble(OCC = c(1, 5, 2, 3, 1, 4, 6, 3, 7, 2, 8))

df3 <- dplyr::tibble(OCC = c(1, 1, 2, 3), OCC1 = 999)

test_that("ordinal_to_binary_ expected output: Expected number of binary flag variables are generated [MRG-ORBI-001]", {
  occvars <- df %>% ordinal_to_binary_("OCC")
  expect_equal(length(occvars), 4)
  
  occvars2 <- df2 %>% ordinal_to_binary_("OCC")
  expect_equal(length(occvars2), 9)
})

test_that("ordinal_to_binary_ unique case: Only 1 binary flag made per row [MRG-ORBI-002]", {
  occvars2 <- df2 %>% ordinal_to_binary_("OCC")
  occvars2 <- occvars2 %>% dplyr::mutate(SUM = rowSums(dplyr::across(OCC1:OCC8)))
  expect_equal(min(occvars2$SUM), 1)
})

test_that("ordinal_to_binary_ expected output: Binary flags made in correct columns [MRG-ORBI-001]", {
  occvars2 <- df2 %>% ordinal_to_binary_("OCC")
  occvars2 <- occvars2 %>% dplyr::filter(OCC == 1)
  expect_true(all(occvars2$OCC1 == 1))
  
  occvars2 <- occvars2 %>% dplyr::filter(OCC == 2)
  expect_true(all(occvars2$OCC2 == 1))
})  

test_that("ordinal_to_binary_ unique case: Function will not overwrite existing columns by default [MRG-ORBI-002]", {
  expect_warning(df3 %>% ordinal_to_binary_("OCC"))
  
  occvars3 <- suppressWarnings(df3 %>% ordinal_to_binary_("OCC"))
  expect_true(all(occvars3$OCC1 == 999))
  expect_equal(max(occvars3$`_OCC1`), 1)
})

test_that("ordinal_to_binary_ unique case: Function will overwrite when called explicitly [MRG-ORBI-002]", {
  occvars3 <- suppressWarnings(df3 %>% ordinal_to_binary_("OCC", overwrite = TRUE))
  expect_true(max(occvars3$OCC1) == 1)
})

test_that("ordinal_to_binary_ unique case: Prefix used for columns when specified [MRG-ORBI-002]", {
  occvars3 <- suppressWarnings(df3 %>% ordinal_to_binary_("OCC", prefix = "EXAMPLE"))
  expect_true(max(occvars3$EXAMPLE1) == 1)
  expect_true(all(occvars3$OCC1 == 999))
})
