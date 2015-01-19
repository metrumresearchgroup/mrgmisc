#' check for if the passed name has a file extension
#' @param name. string name to check for extension
#' @param ext. string of extension to check
#' @param match_case. logical whether to match the case when checking extension.
#'    defaults to TRUE
#' @details
#' This is not a particularly robust checker, but serves its purpose
#' @examples
#' \dontrun{
#' has_ext("test.rmd", ".rmd") #TRUE
#' has_ext("test.Rmd", ".rmd", match_case=F) #TRUE
#' has_ext("testrmd", ".rmd") #FALSE
#' }
#' @export
has_ext <- function(name, ext, match_case=TRUE) {
  ext <- gsub("\\.", "", ext)
  grepl(pattern = paste0("\\.", ext, "$"),
        x = name, 
        ignore.case = as.logical(abs(match_case-1)),
        perl=T) 
}

# TESTS TO MOVE INTO PROPER TESTING FOLDER
# test_that("has_ext properly matches extensions for possible scenarios", {
# expect_true(has_ext("test.rmd", ".rmd"))
# expect_true(has_ext("test.Rmd", ".rmd", match_case=F))
# expect_false(has_ext("testrmd", ".rmd"))
# expect_false(has_ext("test.rmd2", ".rmd"))
# })

