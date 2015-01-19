#' a better new rmd template
#' @param file file name and (optionally) path to subdirectory
#' @param title. Title of rmd document
#' @param author. Author of document
#' @param date. Date
#' @param template. template file to use for file creation
#' @examples
#' \dontrun{
#' new_rmd("Report.Rmd")
#' new_rmd("templates/Report.Rmd", "Example Report", "Devin")
#' }
#' @export
new_rmd <- function(file, 
                    title = NULL, 
                    author = NULL, 
                    date = format(Sys.time(), "%b %d, %Y"),
                    template = "basic.Rmd") {
  if(!has_ext(template, ".Rmd")) template <- paste0(template, ".Rmd")
  if(!has_ext(file, ".Rmd", match_case=F)) file <- paste0(file, ".Rmd")
  # TODO: check if directory for file path actually exists
  
  template_path <- paste0(find.package("PKPDmisc"), "/templates/")
  file_path <- paste0(template_path, template)
  
  template <- readChar(file_path, nchars=file.info(file_path)$size)
  
  if(!is.null(title)) template <- gsub("TITLE", title, template)
  if(!is.null(author)) template <- gsub("AUTHOR", author, template)
  template <- gsub("DATE", date, template)
  
  writeChar(template, con = file)
}

#new_rmd("tests/test.rmd") #example failing test if tests/ folder doesn't exist
