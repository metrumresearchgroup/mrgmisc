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
  if(!fread && sep == "auto") stop("sep == 'auto' can only be used with fread")

  if(has_units) {
    if (!fread) {
      dat_info <- read.table(data, 
                             header=F,
                             nrows = as.numeric(header + 1), 
                             skip = skip, 
                             stringsAsFactors = stringsAsFactors,
                             sep = sep,
                             ...)
      
      
    } else {
      dat_info <- data.table::fread(data, 
                                    header=F,
                                    nrows = as.numeric(header + 1), 
                                    skip = skip, 
                                    stringsAsFactors = stringsAsFactors,
                                    sep = sep,
                                    ...)
    }
  }
  

  if(fread) {
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
    if(has_units) {
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

capture_colnames <- function(x, strip_flags = c("TABLE", "ID", "DV", "MDV", "EVID")) {
  no_table <- strip_flags[!(strip_flags %in% "TABLE")]
  flags <- paste0(no_table, collapse = "|")
  
  for (i in seq_along(x)) {
    if(isTRUE(as.logical(grep(flags, x[i])))) return(x[i])
  }
  warning("could not find column names")
  return(NULL)
}

#' read nonmem files easily
#' @param path path to file
#' @export
read_nonmem <- function(path) {
  lines <- readr::read_lines(path)
  col_name <- stringr::str_trim(capture_colnames(lines))
    col_name <- stringr::str_replace_all(col_name, "\\s+", ",")
    lines <- clean_nonmem(lines)
  readr::read_csv(text = paste0(col_name,"\n", lines))
}