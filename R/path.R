#' Return the name of the current script
#' 
#' @return
#' The name of the current script, with no path information.
#' 
#' @seealso [this_file_path()], [this_file_here()]
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
#' @seealso [this_file_name()], [this_file_here()]
#' 
#' @md
#' @export
this_file_path <- function() {
  stopifnot(requireNamespace("this.path", quietly = TRUE))
  envir <- caller_env()
  this.path::this.path(envir = envir, srcfile = TRUE)
}

#' Return the relative path to the current script
#' 
#' @return
#' The name and path to the current script, relative to a root
#' directory defined by a call to [here::here()].
#' 
#' @seealso [this_file_path()], [this_file_name()]
#' 
#' @md
#' @export
this_file_here <- function() {
  stopifnot(requireNamespace("this.path", quietly = TRUE))
  stopifnot(requireNamespace("here", quietly = TRUE))
  stopifnot(requireNamespace("fs", quietly = TRUE))
  envir <- caller_env()
  ans <- fs::path_rel(
    this.path::this.path(envir = envir, srcfile = TRUE), 
    here::here()
  )
  as.character(ans)
}
