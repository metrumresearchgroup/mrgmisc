duplicate_df <-
  dplyr::tibble(
    ID = c(1, 1, 2, 2),
    EVID = c(0, 1, 1, 1),
    TIME = c(-0.5, 1, 1, 1)
  )

non_dup_df <-
  dplyr::tibble(
    ID = c(1, 1, 2, 2),
    EVID = c(0, 1, 1, 1),
    TIME = c(-0.5, 1, 1, 2)
  )

test_that("is_distinct catches when there is a duplicate in the data", {
  expect_true(!is_distinct(.df = duplicate_df, .cols = c("EVID", "TIME")))
  expect_true(!is_distinct(.df = duplicate_df, .cols = c("ID", "EVID", "TIME")))
  expect_true(!is_distinct(.df = duplicate_df, .cols = c("ID")))
})

test_that("is_distinct catches when there is a duplicate in the data", {
  expect_true(!is_distinct(.df = non_dup_df, .cols = c("EVID", "TIME")))
  expect_true(is_distinct(.df = non_dup_df, .cols = c("ID", "EVID", "TIME")))
})

test_that("is_distinct gives an error if column name doesn't exist", {
  expect_error(is_distinct(.df = non_dup_df, .cols = c("EVID2", "TIME")), "All .cols not in .df")
})
