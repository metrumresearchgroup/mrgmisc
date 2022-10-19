
dat <- dplyr::tibble(
  var1 = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
  var2 = c(1, 1, 4, 3, 5, 1, 2, 3, 3)
)

# chunk testing -----------------------------------------------------------

test_that("chunk expected output: Chunking works with unique elements [MRG-CHNK-001]", {
  uni_chunk <- chunk(1:9, 3) # sorted
  expect_outcome <- c(1, 1, 1, 2, 2, 2, 3, 3, 3)
  expect_equal(uni_chunk, expect_outcome)
  
  uni_chunk2 <- chunk(c(9, 4, 5, 6, 2, 3, 1, 8, 7), 3) # not sorted
  expect_equal(uni_chunk2, expect_outcome)
  
  let_chunk <- chunk(c("a", "b", "c", "d", "e", "f", "g"), 3)
  expect_outcome2 <- c(1, 1, 1, 2, 2, 3, 3)
  expect_equal(let_chunk, expect_outcome2)
  
  tib_chunk <- chunk(dat$var1, 4)
  expect_outcome3 <- c(1, 1, 1, 2, 2, 3, 3, 4, 4)
  expect_equal(tib_chunk, expect_outcome3)
  
  expect_error(chunk(1:9, 10))
})

test_that("chunk expected output: Chunking ignores non-unique elements [MRG-CHNK-001]", {
  nonuni_chunk <- chunk(c(1, 1, 1:7), 3)
  expect_outcome <- c(1, 1, 1, 2, 2, 2, 3, 3, 3)
  expect_equal(nonuni_chunk, expect_outcome)
})

# Group chunking testing --------------------------------------------------

test_that("chunks unique groups as evenly as possible [MRG-CHNK-002]", {
  unigrp_chunk <- chunk_grp(c(1, 1, 1:7), 3)
  expect_outcome <- c(1, 1, 1, 1, 1, 2, 2, 3, 3)
  expect_equal(unigrp_chunk, expect_outcome)
  
  nonsort_chk <- chunk_grp(c(7, 4, 1:7), 3) # check that non-sorted list will still work
  expect_outcome <- c(1, 1, 1, 2, 2, 1, 3, 3, 1)
  expect_equal(nonsort_chk, expect_outcome)
  
  let_chunk <- chunk_grp(c("a", "b", "c", "a", "b", "a", "g"), 3) # string input check
  expect_outcome2 <- c(1, 1, 2, 1, 1, 1, 3)
  expect_equal(let_chunk, expect_outcome2)
})

# List chunking -----------------------------------------------------------

test_that("chunk with groups: Chunks unique elements into a list with their representative values [MRG-CHNK-003]", {
  expect_equal(chunk_list(letters[1:9], 3), 
               list(c("a", "b", "c"), 
                    c("d", "e", "f"), 
                    c("g", "h", "i")))
  
  expect_warning(chunk_list(list(c("a", "b", "c"), 
                                 c("d", "e", "f"), 
                                 c("g", "h", "i")), 3))
})

test_that("chunk with groups: Chunks unique elements into equal list if not grp_list [MRG-CHNK-003]", {
  expect_equal(chunk_list(letters[c(1, 1, 2, 1:7)], 3), 
               list(c("a", "a", "b", "a"), 
                    c("b", "c", "d"), 
                    c("e", "f", "g")))
})

# Group listing testing ---------------------------------------------------

test_that("chunk with groups: Chunks grouped elements into ragged arrays [MRG-CHNK-003]", {
  expect_equal(chunk_grp_list(c(letters[1], letters[1], letters[1:7]), 3),
               list(c("a", "a", "a", "b", "c"),
                    c("d", "e"), 
                    c("f", "g")))
})

# ID per plot -------------------------------------------------------------

test_that("ids_per_plot expected output: Error occurs if input ID's are not vector [MRG-IDPL-001]", {
  expect_error(ids_per_plot(Theoph$Subject[1], id_per_plot = 9), "chunking requires a vector")
})

test_that("ids_per_plot expected output: IDs are sorted properly [MRG-IDPL-001]", {
  expect_equal(ids_per_plot(id = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), id_per_plot = 5), 
               c(1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 3, 3))
})

test_that("ids_per_plot special case: no error if more bins provided than values [MRG-IDPL-002]", {
  expect_equal(ids_per_plot(letters[1:3], 4), c(1, 1, 1))
  expect_equal(ids_per_plot(c(1, 1, 2, 3), 2), c(1, 1, 1, 2))
})
