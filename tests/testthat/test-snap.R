test_that("works with different type input vectors [MRG-SNAP-001]", {
  expect_equal(snap(c(0.0, 1.0, 1.2, 2.0, 2.9, 3)), c(0, 1, 1, 2, 3, 3))
  expect_error(snap(c("0.0", "1.0", "1.2", "2.0", "2.9", "3")))
  
  expect_equal(snap(-3:3, 0.3), c(-3.0, -2.1, -0.9, 0.0, 0.9, 2.1, 3.0))
})

test_that("works with non-unqiue elements in c [MRG-SNAP-001]", {
  expect_equal(snap(c(1, 1, 1, 2, 3), 0.3), c(0.9, 0.9, 0.9, 2.1, 3.0))
})

test_that("works with NA [MRG-SNAP-001]", {
  expect_equal(snap(c(3,NA,5), c(2,3,6)), c(3, NA, 6))
})

test_that("returns all NA if rule is 0 [MRG-SNAP-001]", {
  expect_true(is.na(all(snap(c(3,NA,5), numeric(0)))))
})
