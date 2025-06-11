#' Return the current script's file name
#'
#' @description
#'
#' These functions use \pkg{this.path} to determine the file name of the current
#' script.
#'
#'  * `this_file_name()` returns the base name of the current script.
#'
#'  * `this_file_path()` returns the absolute path of the current script.
#'
#'  * `this_file_proj()` returns the path of the current script relative to the
#'     root of the project that contains it, as defined by
#'     [this.path::this.proj()].
#'
#' @return path The file name of the current script.
#' @md
#' @name this_file
NULL

#' @rdname this_file
#' @export
this_file_name <- function() {
  basename(this_file_path())
}

#' @rdname this_file
#' @export
this_file_dir <- function() {
  dirname(this_file_path())  
}

#' @rdname this_file
#' @export
this_file_path <- function() {
  if(!requireNamespace("this.path", quietly = TRUE)) {
    abort("The package \"this.path\" is required.")  
  }
  envir <- caller_env()
  this.path::this.path(envir = envir, srcfile = TRUE)
}

#' @rdname this_file
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
