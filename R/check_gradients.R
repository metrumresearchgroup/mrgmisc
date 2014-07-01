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
#' @export 
check_gradients <- function(runnum, 
                            wd = NULL, 
                            folder_str = '.mod.dir.1', 
                            full_directory = NULL, 
                            print = TRUE) {
  
if(is.null(wd)) wd <- getwd()

ifelse(!is.null(full_directory),
     # read custom full directory
     raw_grd <- read.table(full_directory, skip = 1, header = T),
     # use default directory
     raw_grd <- read.table(paste0(wd,'/run', runnum, folder_str, '/NM_run1/psn.grd'), 
                           skip = 1, 
                           header = T)  
)

grd <- reshape2::melt(raw_grd, id.vars='ITERATION')

grd <- within(grd, iszero <- ifelse(value == 0, 1, 0))
bdry_df <- dplyr::group_by(grd, variable) %>% dplyr::summarize(boundary = any(iszero))
grd <- dplyr::left_join(grd, bdry_df)

# for some reason this statement as an ifelse throws an error "replacement has length zero"
if(any(grd$boundary)) {cat(paste("Boundaries found for parameters:"), 
                           bdry_df$variable[bdry_df$boundary == TRUE])
} else cat(paste("No boundaries found"))


  if(print == TRUE) {
  out <- ggplot(data = grd, aes(x= ITERATION, y = value, group = variable)) 
  if (any(grd$boundary == TRUE)) { 
    out <- out + geom_rect(data = subset(grd, boundary == TRUE),
                           aes(fill = "red"), 
                           xmin = -Inf, 
                           xmax = Inf, 
                           ymin = -Inf,
                           ymax = Inf, 
                           alpha = 0.01)} 
  out <- out + geom_line() + geom_point(aes(color = factor(iszero))) +
    facet_wrap(~variable, scales = "free") +
    scale_color_manual(name = "Gradient Value", values = c("black", "red"), 
                       labels = c("non-zero", "zero")) + 
    ggtitle(paste0("run", runnum)) + guides(fill = FALSE)
  print(out)
  }
}