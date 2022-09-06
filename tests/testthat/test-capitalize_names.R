lower_str <- c("apple", "banana", "carrot")
lower_list <- list("apple", "banana", "carrot")

test_that("character string returns appropriate error", {
  expect_error(capitalize_names(lower_str), "Input must be dataframe")
  expect_error(capitalize_names(lower_list), "Input must be dataframe")
})

test_that("Capitalizes tibble names", {
  cTheoph <- capitalize_names(Theoph)
  cTheoph_n <- names(cTheoph)
  expect_equal(cTheoph_n[1], "SUBJECT")
})
