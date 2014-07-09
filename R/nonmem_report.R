#' create basic folder structure for a nonmem report
#' @name nonmem_report
#' @param project name of top level project folder
#' @param secondary_folders list of subfolders
#' @details
#' A placeholder.txt file is created in each subfolder to allow for
#' initial folder structure to be maintained if using a version control
#' system such as git (as empty directories are not kept)
#' 
#' Currently only works if there is no project folder with the same name, and
#' does not do any checking (will just error out)
#' @examples
#' \dontrun{
#' nonmem_report("Drug-x")
#' nonmem_report("Drug-x", list("Rscripts", "nonmem","CTS", "data"))
#' }
#' @export 
nonmem_report <- function(project = NULL
         secondary_folders = list("scripts", 
                                  "modeling", 
                                  "lab-notebook", 
                                  "data")) 
{
if (!is.null(project)) {
  dir.create(project)
  lapply(secondary_folders, function(x) {dir.create(paste0(project ,'/', x))
                                         file.create(paste0(project ,'/', x,"/", "placeholder.txt"))})
         } else {
         lapply(secondary_folders, function(x) {dir.create(paste0(x))
                                         file.create(paste0(x,"/", "placeholder.txt"))})    
         }
}
