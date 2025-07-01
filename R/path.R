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
  # Pass NULL for srcfile to prevent this.path from extracting the file name
  # from a source reference. The source reference lookup shouldn't be relevant
  # for users of these wrappers, the tests depend on the lookup being disabled.
  proj <- fs::path_real(this.path::this.proj(envir = emptyenv(), srcfile = NULL))
  proj
}

bad_path <- function(path, type = c("table", "figure", "script"), create = FALSE) {
  type <- match.arg(type)
  if(isTRUE(create) && type != "script") {
    inform(message = "Creating directory: ", body = path)
    new_path <- fs::dir_create(path)
    if(!dir.exists(new_path)) {
      abort("Failed to create directory.")  
    }
    return(invisible(NULL))
  }
  look <- c(table = "tables_to", figure = "figures_to", script = "mrg_script")
  fun <- look[type]
  help <- paste0("See ?", fun, " for help formatting the path.")
  msg <- paste0("The ", type, " output path could not be found:")
  names(path) <- "x"
  names(help) <- "i"
  abort(message = msg, body = c(path, help))
}

#' @rdname this_file
#' @export
this_file_name <- function() {
  check_path_deps()
  basename(this_file_path())
}

#' @rdname this_file
#' @export
this_dir_name <- function() {
  check_path_deps()
  basename(dirname(this_file_path()))  
}

#' @rdname this_file
#' @export
this_file_path <- function() {
  check_path_deps()
  # See comment above about srcfile=NULL.
  this.path::this.path(envir = emptyenv(), srcfile = NULL)
}

#' @rdname this_file
#' @export
this_dir_path <- function() {
  check_path_deps()
  dirname(this_file_path())  
}

#' @rdname this_file
#' @export
this_file_proj <- function() {
  check_path_deps()
  
  proj <- this_proj()
  path <- fs::path_real(this_file_path())
  as.character(fs::path_rel(path, proj))
}

#' @rdname this_file
#' @export
this_dir_proj <- function() {
  check_path_deps()
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
#' * `tf_options_clear()` clears options set by `mrg_script()`, `tables_to()`, 
#'   and `figures_to()`, setting the option value to `NULL`.
#'   
#' @param path the absolute path or path relative to the current working
#' directory.
#' @param create if `TRUE`, the directory will be created with 
#' [fs::dir_create()] in case it does not already exist. 
#' @param path.type indicates how `pmtables` should format the path portion
#' of table annotations; see `pmtables::format_table_path`. 
#' @param set_script if `TRUE` (the default), include a call to `mrg_script()` 
#' to set the `mrg.script` option.  
#' @param quietly if `TRUE`, don't echo `tf_options()` outputs after options
#' are cleared with `tf_options_clear()`.
#'  
#' @return
#' - `tables_to()`, `figures_to()` and `mrg_script()` return the value of the 
#'   option invisibly
#' - `tf_options()` prints messages to the console and returns `NULL` invisibly
#' - `tf_options_clear()` prints `tf_options()` output unless the user requests
#'   quiet reset.
#'  
#' @md
#' @rdname tf_options
#' @export
tf_options <- function() {
  check_path_deps()
  
  not_set <- "<option not set>"
  missing <- "<does not exist>"
  
  proj <- this_proj()
  
  # mrg.script
  script <- options()$mrg.script
  if(!is.character(script)) {
    script <- not_set 
  } else if (!file.exists(file.path(proj, script))) {
    script <- paste(script, missing)
  }
  script <- paste0("script  ", script)
  names(script) <- "*"
  inform(script)
  
  # pmtables.dir
  tables <- options()$pmtables.dir
  if (!is.character(tables)) {
    tables <- not_set
  } else if (dir.exists(tables)) {
    tables <- proj_rel(tables)
  } else {
    tables <- paste(tables, missing)
  }
  tables <- paste0("tables  ", tables)
  names(tables) <- "*"
  inform(tables)
  
  # mrggsave.dir
  figures <- options()$mrggsave.dir
  if (!is.character(figures)) {
    figures <- not_set
  } else if (dir.exists(figures)) {
    figures <- proj_rel(figures)
  } else {
    figures <- paste(figures, missing)
  }
  figures <- paste0("figures ", figures)
  names(figures) <- "*"
  inform(figures)
  
  invisible(NULL)
}

#' @rdname tf_options
#' @export
tf_options_clear <- function(quietly = FALSE) {
  check_path_deps()
  options(
    mrg.script = NULL, 
    pmtables.dir = NULL, 
    pmtables.path.type = NULL,
    mrggsave.dir = NULL
  )
  if(!isTRUE(quietly)) {
    tf_options()
  }
  return(invisible(NULL))
}

#' @rdname tf_options
#' @export
mrg_script <- function(path = NULL) {
  check_path_deps()
  if(is.null(path)) {
    path <- this_file_proj()  
  } else {
    if(!file.exists(path)) {
      bad_path(path, type = "script")  
    }
    path <- proj_rel(path)
  }
  options(mrg.script = path) 
  return(invisible(options()$mrg.script))
}

#' @rdname tf_options
#' @export
tables_to <- function(path, create = FALSE, set_script = TRUE, path.type = "proj") {
  check_path_deps()
  if(isTRUE(set_script)) {
    mrg_script()  
  }
  if(!dir.exists(path)) {
    bad_path(path, type = "table", create = create)
  }
  path <- as.character(fs::path_real(path))
  options(
    pmtables.dir = path, 
    pmtables.path.type = path.type
  )
  invisible(options()$pmtables.dir)
}

#' @rdname tf_options
#' @export
figures_to <- function(path, create = FALSE, set_script = TRUE) {
  check_path_deps()
  if(isTRUE(set_script)) {
    mrg_script()  
  }
  if(!dir.exists(path)) {
    bad_path(path, type = "figure", create = create) 
  }
  path <- as.character(fs::path_real(path))
  options(mrggsave.dir = path)
  invisible(options()$mrggsave.dir)
}

#' Calculate the path relative to the project root
#' 
#' @param path the path to a file or folder.
#' 
#' @export
proj_rel <- function(path) {
  check_path_deps()
  as.character(fs::path_rel(path, this_proj()))
}

check_path_deps <- function() {
  if (!requireNamespace("this.path", quietly = TRUE)) {
    abort("The package \"this.path\" is required.", call = caller_env())
  }
  if (!requireNamespace("fs", quietly = TRUE)) {
    abort("The package \"fs\" is required.", call = caller_env())
  }
}
