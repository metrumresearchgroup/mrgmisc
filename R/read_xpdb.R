#' read xpose tables to xpose database and save and compress as RDS
#' @param runno run number
#' @param table.names table names for xpose.data to search for
#' @param RDS_name name of RDS save file
#' @param rm_tabs logical value to delete output tables after saving to xpdb
#' @param ... arguments to pass to xpose.data
#' @export
#' @examples
#' \dontrun{
#' read_xpdb("001") # would save a file xpdb001.rds
#' }
#' @return NULL
read_xpdb <- function(runno,
                      table.names=c("sdtab","mutab","patab","catab",
                                          "cotab","mytab","extra","xptab","cwtab"), 
                      RDS_name = paste0("xpdb",runno),
                      rm_tabs = FALSE,
                      ...) {
  
  if(!requireNamespace("xpose4", quietly = TRUE)) {
    stop("need xpose4 to use this function, please install!")
  }
  
  xpdb <- xpose4::xpose.data(runno, table.names = table.names, ...)
  if(is.null(xpdb)) stop("Error creating xpose database, nothing created")
  RDS = paste0(RDS_name,".rds")
  saveRDS(xpdb, file=RDS)
  message("xpose database saved as: ", RDS)
  return(NULL)   
}
