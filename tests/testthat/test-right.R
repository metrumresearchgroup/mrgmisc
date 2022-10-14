test_that("right grabs the appropriate amount of characters from the end of the string [MRG-RGHT-001]", {
  expect_equal(right("string-123", 3), "123")
  expect_equal(right("string-123", 1), "3")
  expect_equal(right("string-123", 0), "")
})
