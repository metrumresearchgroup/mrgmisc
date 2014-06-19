#' read xpose tables to xpose database and save and compress as RDS
#' @param runno
#' @param ... arguments to pass to xpose.data
#' @param rm_tabs logical value to delete output tables after saving to xpdb
#' @export
#' @examples
#' \dontrun{
#' read_xpdb("001") # would save a file 
#' }
#' @return NULL
read_xpdb <- function(runno,
                      table.names=c("sdtab","mutab","patab","catab",
                                          "cotab","mytab","extra","xptab","cwtab"),
                      ..., 
                      RDS_name = paste0("xpdb",runno)) {
  require(xpose4)
  xpdb <- xpose.data(runno, table.names = table.names, ...)
  if(is.null(xpdb)) stop("Error creating xpose database, nothing created")
  RDS = paste0(RDS_name,".rds")
  saveRDS(xpdb, file=RDS)
  message("xpose database saved as: ", RDS)
  return(NULL)   
}
