context("replacement-flags")

vec <- c(1, 1, "b", "a", 5)    
correct_outputn99_repNA <- c(NA, NA, -99, -98, NA)

test_that("list elements replaced properly", {
  replacementn99 <- data.frame(flag = c("b", "a"), rep = c(-99, -98))
  correct_outputn99 <- c(1, 1, -99, -98, 5)
  replacementn99unn <- data.frame(flag = unique_non_numerics(vec), rep=c(-99, -98))
  expect_equal(replace_values(vec, replacementn99), correct_outputn99)
  expect_equal(replace_values(vec, replacementn99,nonflag = NA), correct_outputn99_repNA)

})

test_that("non numerics flags correctly", {
  expect_equal(unique_non_numerics(vec) , c("a", "b")) 
  expect_equal(unique_non_numerics(vec, na.rm = FALSE) , c("a", "b")) 
  expect_equal(unique_non_numerics(vec, .sort = FALSE) , c("b", "a")) 
})