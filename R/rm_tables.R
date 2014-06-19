#' clean tables associated with nonmem output from directory
#' 
#' @param runno nonmem run number to check for tables
#' @param xpose_tables
#' @param table_names additional table names
#' @details
#' defaults to check and clean for xpose-style tables
#' @export
rm_tables <- function(runno, 
                      xpose_tables =c("sdtab","mutab","patab","catab",
                                      "cotab","mytab","extra","xptab","cwtab"),
                      table_names = NULL
                      ) {
  NULL
}