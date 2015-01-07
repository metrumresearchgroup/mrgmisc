#' to more easily read data with a 2nd row with units
#' @param data the name of the csv
#' @param skip rows to skip before header row
#' @param stringsAsFactors whether to include string columns as factors
#' @param header logical value indicating whether the file contains the names of variables
#'  as it's first line
#' @param ... additional arguments to read.csv functions
#' @param has_units has a units row (a la phx data)
#' @details
#' helpful function to handle situations where the second row is a units row
#' as often seen with phoenix-style datasets
#' @return data frame of read-in csv
#' @examples
#' \dontrun{
#' read_csv_wunits("example.csv")
#' read_csv_wunits("example.csv", skip = 1) # will ignore 1st line, good for comment lines
#' }
#' @export
read_csv_wunits <- function(data, 
                            skip = 0, 
                            stringsAsFactors = FALSE, 
                            header = TRUE,
                            ...,
                            has_units = TRUE,
                            fread=FALSE) {
message("read_csv_wunits is depreciated and will be removed in future,
please use read_table with has_units=TRUE")
read_table(data = data,
           skip = skip,
           fread=fread,
           stringsAsFactors=stringsAsFactors,
           has_units = has_units,
           header = header,
           ...)
}