#' create view commands that save rds files to where a shiny app is listening for them
#' @param path path to shiny app directory
#' @return a function with the path set
#' @details 
#' will, by default, set up to save RDS files to the shiny app to render them,
#' and can take some optional arguments in the resulting function. Namely, 
#' can rename the file via name, and can determine whether the function should
#' return a dataframe with return = T/F. The value of returning a function is
#' view<x> can be embedded in a data pipeline to output intermediate results
#' as well, while continuing on the pipeline
#' @examples \dontrun{
#' path <- "~/Repos/dataView" # dataView shiny app location
#' view2 <- view_creator(path)
#' View2(Theoph) # will save a file Theoph.rds in dataView
#' View2(Theoph, "theoph1") #will save a file theoph1.rds
#' View2(Theoph, return = F) # will not return Theoph as well
#' }
#' @export
view_creator <- function(path) {
  return(function(.data, name = NULL, return = TRUE) {
    if(is.null(name)) {
      name <- deparse(substitute(.data))
    }
    
    saveRDS(.data, suppressWarnings(normalizePath(file.path(path, paste0(name, ".rds")))))
    
    if(return) {
      return(.data)
    }
  })
}