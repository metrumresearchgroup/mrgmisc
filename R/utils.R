
#' detect if a filepath is for a directory
#' @param x vector of file paths
#' @export
is_dir <- function(x) {
  isTRUE(file.info(x)$isdir)
}

#' get the basename of a filepath, minus any extensions
#' @param .x filepath
#' @export
#' @rdname basename_sans_ext
#' @importFrom tools file_path_sans_ext
basename_sans_ext <- function(.x) {
  basename(file_path_sans_ext(.x))
}
