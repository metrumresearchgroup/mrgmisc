test_that("ghere interpolates variables from the caller environment", {
  folder <- "data"
  file_name <- "summary"
  
  # Standard here::here() combined with static strings
  expected <- as.character(glue::glue(here::here("data", "summary.csv")))
  
  # ghere execution
  result <- ghere("{folder}", "{file_name}.csv")
  
  expect_equal(result, expected)
  expect_type(result, "character")
})

test_that("ghere handles empty arguments gracefully", {
  expected <- as.character(glue::glue(here::here()))
  result <- ghere()
  
  expect_equal(result, expected)
})
