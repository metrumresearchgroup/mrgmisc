lower_str <- c("apple", "banana", "carrot")
lower_list <- list("apple", "banana", "carrot")

test_that("capitalize_names outputs expectation: Character string returns appropriate error", {
  expect_error(capitalize_names(lower_str), "Input must be dataframe")
  expect_error(capitalize_names(lower_list), "Input must be dataframe")
})

test_that("capitalize_names outputs expectation: Function capitalizes tibble names", {
  cTheoph <- capitalize_names(Theoph)
  cTheoph_n <- names(cTheoph)
  expect_equal(cTheoph_n[1], "SUBJECT")
})
