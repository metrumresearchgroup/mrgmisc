context("numeric")

test_that("as_numeric", {
  expect_equal(as_numeric(1), 1)
  expect_equal(
    as_numeric(factor(1, labels="2")),
    2,
    info="Character conversion makes it respect the label and not the value."
  )
  expect_equal(
    expect_silent(as_numeric("A")),
    NA_real_
  )
  expect_equal(
    expect_warning(
      as_numeric("A", require_conversion=FALSE),
      regexp="The following non-numeric values were converted to NA: 'A'",
      fixed=TRUE
    ),
    NA_real_
  )
  expect_error(
    as_numeric("A", require_conversion=TRUE),
    regexp="The following non-numeric values were converted to NA: 'A'",
    fixed=TRUE
  )
  expect_error(
    as_numeric(c("B", "A"), require_conversion=TRUE),
    regexp="The following non-numeric values were converted to NA: 'A', 'B'",
    fixed=TRUE
  )
})
