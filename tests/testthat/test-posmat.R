test_that("posmat error: Correct error occurs with non-square matrix [MRG-PSMT-002]", {
  expect_error(posmat(matrix(c(10.00006,20.00006,-30, 5, 5, 40),3,2)), "x is not square")
})

test_that("posmat expected output: Correct matrix is produced [MRG-PSMT-001]", {
  tempMat <- posmat(matrix(c(10.00006,20.00006,-30,40),2,2))
  expect_equal(tempMat[1], 10.0001)
  expect_equal(tempMat[3], -29.9999)
  expect_equal(tempMat[4], 40.0000)
})

test_that("posmat expected output: Appropriate matrix elements are made [MRG-PSMT-001]", {
  tempMat2 <- posmat(matrix(rep(100,4),2,2))
  expect_equal(tempMat2[1], tempMat2[4])
  expect_equal(tempMat2[2], tempMat2[3])
})
