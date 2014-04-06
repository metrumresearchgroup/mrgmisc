#' Faster way of reading in nonmem data
#' @param file file to be read in
#' @param sedCmd sed command
#' @param ... dots argument to pass to fread
#' @details
#' uses sed to covert to csv before fread and returns data.table object
#' @name read_nonmem
#' @export
read_nonmem <-function(file, sedCmd=NULL, ...){
  # from SO post 
  #http://stackoverflow.com/questions/22229109/
  #r-data-table-fread-command-how-to-read-large-files-with-irregular-separators
  if(is.null(sedCmd)){
    #default : sed for convert blank separated table to csv. Thanks NeronLevelu 
    sedCmd <- "'s/^[[:blank:]]*//;s/[[:blank:]]\\{1,\\}/,/g'"
  }
  #sed into temp file
  tmpPath<-tempfile(pattern='tmp',fileext='.txt')
  sysCmd<-paste('sed',sedCmd, file, '>',tmpPath)
  try(system(sysCmd))
  DT<-data.table::fread(tmpPath,...)
  try(system(paste('rm',tmpPath)))
  return(DT)
}
