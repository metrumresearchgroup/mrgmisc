#' Return the current script's file name or directory
#'
#' @description
#'
#' The following functions use \pkg{this.path} to determine the file name of the
#' current script:
#'
#'  * `this_file_name()` returns the base name of the current script.
#'
#'  * `this_file_path()` returns the absolute path of the current script.
#'
#'  * `this_file_proj()` returns the path of the current script relative to the
#'    root of the project that contains it, as defined by
#'    [this.path::this.proj()].
#'
#' The following functions use \pkg{this.path} to determine the parent directory
#' of the current script:
#'
#'  * `this_dir_name()` returns the base name of the directory containing the
#'     current script.
#'
#'  * `this_dir_path()` returns the absolute path of the directory containing
#'    the current script.
#'
#'  * `this_dir_proj()` returns the path of the directory containing the current
#'    script relative to the root of the project that contains it, as defined by
#'    [this.path::this.proj()].
#'
#' @return path The file name of the current script.
#' @md
#' @name this_file
NULL

this_proj <- function() {
  envir <- caller_env()
  proj <- fs::path_real(this.path::this.proj(envir = envir, srcfile = TRUE))
  proj
}

#' @rdname this_file
#' @export
this_file_name <- function() {
  basename(this_file_path())
}

#' @rdname this_file
#' @export
this_dir_name <- function() {
  basename(dirname(this_file_path()))  
}

#' @rdname this_file
#' @export
this_file_path <- function() {
  if(!requireNamespace("this.path", quietly = TRUE)) {
    abort("The package \"this.path\" is required.")  
  }
  envir <- caller_env()
  # Pass NULL for srcfile to prevent this.path from extracting the file name
  # from a source reference. The source reference lookup shouldn't be relevant
  # for users of these wrappers, the tests depend on the lookup being disabled.
  this.path::this.path(envir = envir, srcfile = NULL)
}

#' @rdname this_file
#' @export
this_dir_path <- function() {
  dirname(this_file_path())  
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
  # See comment above about srcfile=NULL.
  proj <- fs::path_real(this.path::this.proj(envir = envir, srcfile = NULL))
  path <- fs::path_real(this_file_path())
  as.character(fs::path_rel(path, proj))
}

#' @rdname this_file
#' @export
this_dir_proj <- function() {
  dirname(this_file_proj())  
}

#' Set options for annotating and saving tables and figures
#' 
#' @description
#' * `mrg_script()` sets the `mrg.script` option using [this_file_proj()].
#' 
#' * `tables_to()` sets the `pmtables.dir` option as the default table output
#'   directory; this function also sets the `pmtables.path.type` option, 
#'   defaulting to `proj`.
#'   
#' * `figures_to()` sets the `mrggsave.dir` option as the default figure output 
#'   directory.
#'   
#' * `tf_options()` prints the `mrg.script`, `pmtables.dir`, and `mrggsave.dir`
#'   options to the console.
#'   
#' @param path the script name or table or figure output path, stated relative 
#' to the project root. 
#'  
#' @return
#' - `tables_to()`, `figures_to()` and `mrg_script()` return the value of the 
#'   option invisibly
#' - `tf_options()` prints messages to the console and returns `NULL` invisibly
#' 
#' @md
#' @rdname tf_options
#' @export
tf_options <- function() {
  not_set <- "<option not set>"
  
  # mrg.script
  script <- options()$mrg.script
  if(!is.character(script)) {
    script <- not_set 
  }
  script <- paste0("script  ", script)
  names(script) <- "*"
  inform(script)
  
  # pmtables.dir
  tables <- options()$pmtables.dir
  if(is.character(tables)) {
    envir <- caller_env()
    proj <- fs::path_real(this.path::this.proj(envir = envir, srcfile = TRUE))
    tables <- fs::path_rel(tables, proj)
  } else {
    tables <- not_set  
  }
  tables <- paste0("tables  ", tables)
  names(tables) <- "*"
  inform(tables)
  
  # mrggsave.dir
  figures <- options()$mrggsave.dir
  if(is.character(figures)) {
    envir <- caller_env()
    proj <- fs::path_real(this.path::this.proj(envir = envir, srcfile = TRUE))
    figures <- fs::path_rel(figures, proj)
  } else {
    figures <- not_set 
  }
  figures <- paste0("figures ", figures)
  names(figures) <- "*"
  inform(figures)
  
  invisible(NULL)
}

#' @rdname tf_options
#' @export
mrg_script <- function(path = NULL) {
  if(is.null(path)) {
    path <- this_file_proj()  
  }
  options(mrg.script = path) 
  return(invisible(options()$mrg.script))
}

#' @rdname tf_options
#' @export
tables_to <- function(path, path.type = "proj") {
  tab_path <- file.path(this_proj(), path)
  if(!dir.exists(tab_path)) {
    warn("The table output path does not exist.") 
  }
  options(
    pmtables.dir = tab_path, 
    pmtables.path.type = "proj"
  )
  invisible(options()$pmtables.dir)
}

#' @rdname tf_options
#' @export
figures_to <- function(path) {
  fig_path <- file.path(this_proj(), path)
  if(!dir.exists(fig_path)) {
    warn("The mrggsave output path does not exist.") 
  }
  options(mrggsave.dir = fig_path)
  invisible(options()$mrggsave.dir)
}
