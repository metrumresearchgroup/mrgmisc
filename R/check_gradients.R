#' get gradients for nonmem run running via PSN
#' @name check_gradients
#' @param runnum Xpose-style run number
#' @param wd relative directly location where PSN run results are output
#' @param folder_str folder name besides run number
#' @param full_directory can be specified if folder where gradient file stored non-traditional
#' @param print print visual plot of gradients
#' @examples
#' \dontrun{
#' check_gradients(001)
#' check_gradients(001, wd = "~/ProjX/NONMEM")
#' check_gradients(001, folder_str = "modelfit_dir1")
#' check_gradients(001, full_directory = "~/user/ProjX/NONMEM/run001.mod.dir.1/NM_run2/psn.grd")
#' }
#' @details
#' by default, the working directory should be set to the location of .mod file associated with the run
#'
#' the default folder structure anticipates runs were run via psn with nomenclature such as run001.mod -mod
#'
#' has not been checked for side effects if have multiple runs with the same file name
#' due to restarts or other changes
#'
#' default to not print visual of gradients
#' @export
check_gradients <- function(runnum,
                            wd = NULL,
                            folder_str = '.mod.dir.1',
                            full_directory = NULL,
                            print = FALSE) {
  if (is.null(wd))
    wd <- getwd()
  
  ifelse(
    !is.null(full_directory),
    # read custom full directory
    raw_grd <- read_nonmem(full_directory),
    # use default directory
    raw_grd <-
      read_nonmem(
        file.path(wd, 'run', runnum, folder_str, 'NM_run1', 'psn.grd')
      )
  )
  
  grd <- reshape2::melt(raw_grd, id.vars = 'ITERATION')
  
  grd <- within(grd, iszero <- ifelse(value == 0, 1, 0))
  bdry_df <- suppressWarnings(dplyr::group_by(grd, variable)
                              %>% dplyr::summarize(boundary = any(iszero)))
  grd <- suppressMessages(dplyr::left_join(grd, bdry_df))
  
  # for some reason this statement as an ifelse throws an error "replacement has length zero"
  if (any(grd$boundary)) {
    message(paste("Boundaries found for parameters:"),
            bdry_df$variable[bdry_df$boundary == TRUE])
  } else {
    message(paste("No boundaries found"))
  }
  
  
  if (print) {
    out <-
      ggplot2::ggplot(data = grd, ggplot2::aes(x = ITERATION, y = value, group = variable))
    if (any(isTRUE(grd$boundary))) {
      out <-
        out + ggplot2::geom_rect(
          data = dplyr::filter(grd, boundary == TRUE),
          ggplot2::aes(fill = "red"),
          xmin = -Inf,
          xmax = Inf,
          ymin = -Inf,
          ymax = Inf,
          alpha = 0.01
        )
    }
    out <-
      out + ggplot2::geom_line() + ggplot2::geom_point(ggplot2::aes(color = factor(iszero))) +
      ggplot2::facet_wrap( ~ variable, scales = "free") +
      ggplot2::scale_color_manual(
        name = "Gradient Value",
        values = c("black", "red"),
        labels = c("non-zero", "zero")
      ) +
      ggplot2::ggtitle(paste0("run", runnum)) + guides(fill = FALSE)
    return(out)
  }
  invisible()
}