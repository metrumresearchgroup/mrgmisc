#' Better defaults for reading in nonmem data
#' @param file file to be read in
#' @param skip number of lines to skip (default = 1)
#' @param header TRUE/FALSE header
#' @param colClasses numeric default to colClasses argument in read.table
#' @param ... dots argument to pass to read.table
#' @details
#' gives colClasses=numeric + default args to read.table
#' @name read_nonmem
#' @return data frame object
#' @export
read_nonmem <-function(file, skip = 1, 
                       header = TRUE,
                       colClasses = "numeric",
                       ...){
  read.table(file, 
             skip = skip, 
             header = header, 
             colClasses = colClasses,  
             ...)
}


#' Faster way of reading in nonmem data using sed to convert table to csv to be read in by fread
#' @param file file to be read in
#' @param sedCmd sed command
#' @param ... dots argument to pass to fread
#' @details
#' uses sed to covert to csv before fread and returns data.table object
#' only applicable on unix systems (I think)
#' @name read_nonmem_sed
#' @return data.table object
#' @export
read_nonmem_sed <-function(file, sedCmd=NULL, ...){
  # reduce whole package dependency on data.table for now
  require(data.table)
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
