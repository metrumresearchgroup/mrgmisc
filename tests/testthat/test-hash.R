test_that("multiplication works", {
  expect_output(hash("x"), "#")
  expect_output(hash("x", char = "^"), "^")
})
