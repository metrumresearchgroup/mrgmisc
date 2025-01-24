
#' @export
this_file <- function() {
  basename(this_file_path())
}

#' @export
this_file_abs <- function() {
  stopifnot(requireNamespace("this.path", quietly = TRUE))
  this.path::this.path()
}

#' @export
this_file_here <- function() {
  stopifnot(requireNamespace("this.path", quietly = TRUE))
  stopifnot(requireNamespace("here", quietly = TRUE))
  fs::path_rel(this.path::this.path(), here::here())
}
