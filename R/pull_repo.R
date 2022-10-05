#' function to sparse pull repo
#'
#' @param .repo SVN repo location
#' @param .name optional name to assign folder on local disk
#' @param .dirs directories to import with depth infinity
#' @param .user mc1 username
#' 
#' @export
pull_repo <- function(.repo, .name=NA_character_, .dirs=c("deliv","doc","renv","script"), .user="samc"){
  ## move to proper directory
  current_wd <- getwd()
  setwd("/data")
  
  ## use repo name if not specified
  if(is.na(.name)) .name <- .repo
  
  ## sparse checkout of repo
  system(glue::glue("svn co svn+ssh://{user}@mc1.metrumrg.com/common/repo/{.repo} {.name} --depth immediates"))
  
  ## pull down all folders listed in .dirs argument
  purrr::map(.dirs, ~{
    setwd(file.path("/data",.name,.x))
    system("svn up --set-depth infinity")
  })
  
  ## return to original wd
  setwd(current_wd)
}