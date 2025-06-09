#' Return the name of the current script
#' 
#' @return
#' The name of the current script, with no path information.
#' 
#' @seealso [this_file_path()], [this_file_proj()]
#' 
#' @md
#' @export
this_file_name <- function() {
  basename(this_file_path())
}

#' Return the absolute path to the current script
#' 
#' @return
#' The absolute path to the current script.
#' 
#' @seealso [this_file_name()], [this_file_proj()]
#' 
#' @md
#' @export
this_file_path <- function() {
  if(!requireNamespace("this.path", quietly = TRUE)) {
    abort("The package \"this.path\" is required.")  
  }
  envir <- caller_env()
  this.path::this.path(envir = envir, srcfile = TRUE)
}

#' Return the path to current script relative to its project
#' 
#' @return
#' The file name of the current script, relative to the root of the project that
#' contains it, as defined by a call to [this.path::this.proj()].
#' 
#' @seealso [this_file_path()], [this_file_name()]
#' 
#' @md
#' @export
this_file_proj <- function() {
  if(!requireNamespace("this.path", quietly = TRUE)) {
    abort("The package \"this.path\" is required.")  
  }
  if(!requireNamespace("fs", quietly = TRUE)) {
    abort("The package \"fs\" is required.")  
  }
  envir <- caller_env()
  proj <- fs::path_real(this.path::this.proj(envir = envir, srcfile = TRUE))
  path <- fs::path_real(this_file_path())
  as.character(fs::path_rel(path, proj))
}
