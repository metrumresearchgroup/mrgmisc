#' to more easily read data with a 2nd row with units
#' @param data the name of the csv
#' @param skip_top rows to skip before header row
#' @param stringsAsFactors whether to include string columns as factors
#' @param ... additional arguments to read.csv functions
#' @details
#' helpful function to handle situations where the second row is a units row
#' as often seen with phoenix-style datasets
#' @return data frame of read-in csv
#' @examples
#' \dontrun{
#' read_csv_wunits("example.csv")
#' read_csv_wunits("example.csv", skip_top = 1) # will ignore 1st line, good for comment lines
#' }
#' @export
read_csv_wunits <- function(data, 
                            skip_top = 0, 
                            stringsAsFactors = FALSE, 
                            ...) {
  dat_info <- read.csv(data, 
                       header=F,
                       nrows = 2, 
                       skip = skip_top, 
                       ...)
  dat <- read.csv(data, 
                  header=F, 
                  skip = skip_top + 2, 
                  stringsAsFactors= stringsAsFactors, 
                  ...)
  names(dat) <- unlist(dat_info[1,])
  return(dat)
}