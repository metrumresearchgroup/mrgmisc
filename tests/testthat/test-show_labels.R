id <- 1:4
age <- c(83, 29, 64, 40)
bwt <- c(101, 102, 103, 104)
sex <- c(1, 0, 0, 1)
df_no_labels <- tibble::tibble(ID = id, AGE = age, BWT = bwt, SEX = sex)
attr(id, "label") <- "Subject ID"
attr(age, "label") <- "Age (years)"
attr(bwt, "label") <- 3
df <- tibble::tibble(ID = id, AGE = age, BWT = bwt)
df_missing_label <- tibble::tibble(ID = id, AGE = age, BWT = bwt, SEX = sex)

test_that("show_labels gives error when df is not data.frame", {
  expect_error(show_labels(c(1, 2)), "df must be a data frame.")
})

test_that("show_labels gives error when dots are not acceptable", {
  expect_error(show_labels(df, list()), "Invalid column selection in `...`.")
  expect_error(show_labels(df, 0.5), "Invalid column selection in `...`.")
})

test_that("show_labels works on unlabelled data.frame", {
  expect_equal(
    show_labels(df_no_labels),
    tibble::tibble(
      name = c("ID", "AGE", "BWT", "SEX"), 
      label = c(
        ID = NA_character_, 
        AGE = NA_character_, 
        BWT = NA_character_, 
        SEX = NA_character_
      )
    )
  )
})

test_that("show_labels works on partially labelled data.frame", {
  expect_equal(
    show_labels(df_missing_label),
    tibble::tibble(
      name = c("ID", "AGE", "BWT", "SEX"), 
      label = c(
        ID = "Subject ID", 
        AGE = "Age (years)", 
        BWT = "3", 
        SEX = NA_character_
      )
    )
  )
})

test_that("show_labels works on labelled data.frame", {
  expect_equal(
    show_labels(df), 
    tibble::tibble(
      name = c("ID", "AGE", "BWT"), 
      label = c(
        ID = "Subject ID", 
        AGE = "Age (years)", 
        BWT = "3"
      )
    )
  )
})

test_that("show_labels works on labelled data.frame with character dots", {
  expect_equal(
    show_labels(df, "ID", "BWT"), 
    tibble::tibble(
      name = c("ID", "BWT"), 
      label = c(
        ID = "Subject ID", 
        BWT = "3"
      )
    )
  )
})

test_that("show_labels works on labelled data.frame with unquoted dots", {
  expect_equal(
    show_labels(df, ID, BWT), 
    tibble::tibble(
      name = c("ID", "BWT"), 
      label = c(
        ID = "Subject ID", 
        BWT = "3"
      )
    )
  )
})

test_that("show_labels works on labelled data.frame with indexed dots", {
  expect_equal(
    show_labels(df, 1, 3), 
    tibble::tibble(
      name = c("ID", "BWT"), 
      label = c(
        ID = "Subject ID", 
        BWT = "3"
      )
    )
  )
})

test_that("show_labels works on labelled data.frame with char vector dots", {
  expect_equal(
    show_labels(df, c("ID", "BWT")), 
    tibble::tibble(
      name = c("ID", "BWT"), 
      label = c(
        ID = "Subject ID", 
        BWT = "3"
      )
    )
  )
})

test_that("show_labels works on labelled data.frame w/ unquoted vector dots", {
  expect_equal(
    show_labels(df, c(ID, BWT)), 
    tibble::tibble(
      name = c("ID", "BWT"), 
      label = c(
        ID = "Subject ID", 
        BWT = "3"
      )
    )
  )
})

test_that("show_labels works on labelled data.frame with index vector dots", {
  expect_equal(
    show_labels(df, c(1, 3)), 
    tibble::tibble(
      name = c("ID", "BWT"), 
      label = c(
        ID = "Subject ID", 
        BWT = "3"
      )
    )
  )
})
