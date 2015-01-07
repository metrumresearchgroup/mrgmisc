#' to more easily read data with a 2nd row with units
#' @param data the name of the csv
#' @param skip rows to skip before header row
#' @param header logical value indicating whether the file contains the names of variables
#'    as it's first line
#'  @param sep field separator character. With fread defaults to 'auto', else defaults
#'    to ","
#' @param stringsAsFactors logical value whether to include string columns as factors
#' @param has_units logical value whether has a units row below the header (eg phx nlme data)
#' @param fread logical value. Use fread from data.table for much faster reading
#' @param data.table logical value. When using fread, whether to return 
#'    data.table (TRUE) or data.frame (FALSE)
#' @param ... additional arguments to read.csv functions
#' @details
#' helpful function to handle situations where the second row is a units row
#' as often seen with phoenix-style datasets
#' @return data frame of read-in csv
#' @examples
#' \dontrun{
#' read_table("example.csv")
#' read_table("example.csv", skip = 1) # will ignore 1st line, good for comment lines
#' }
#' @export
read_table <- function(data, 
                            skip = 0, 
                            header = TRUE,
                            sep = "auto",
                            stringsAsFactors = FALSE, 
                            has_units = FALSE,
                            fread = TRUE,
                            data.table = FALSE,
                            ...) {
  if(!fread && sep == "auto") sep <- ","

  if (isTRUE(has_units)) {
    dat_info <- read.table(data, 
                           header=F,
                           nrows = as.numeric(header + 1), 
                           skip = skip, 
                           stringsAsFactors = stringsAsFactors,
                           sep = sep,
                           ...)
  }

  if(isTRUE(fread)) {
    if(has_units) {
      dat <- data.table::fread(
        input = data,
        stringsAsFactors=stringsAsFactors,
        skip = skip+ as.numeric(header+1),
        header = F,
        data.table = data.table,
        sep = sep,
        ...
      )
      names(dat) <- unlist(dat_info[1,])
      
    } else {
      dat <- data.table::fread(
        input = data,
        stringsAsFactors=stringsAsFactors,
        skip=skip,
        header=header,
        data.table = data.table,
        sep = sep,
        ...
      )

    }

  } else {
    if(isTRUE(has_units)) {
      dat <- read.table(data, 
                        header=F, 
                        skip = skip + as.numeric(header+1), 
                        stringsAsFactors= stringsAsFactors,
                        sep=sep,
                        ...)
      names(dat) <- unlist(dat_info[1,])
    } else {
      dat <- read.table(data, 
                        header=header, 
                        skip = skip, 
                        stringsAsFactors= stringsAsFactors,
                        sep = sep,
                        ...)
    }

  }
return(dat)
}