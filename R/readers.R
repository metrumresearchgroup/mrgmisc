#' to more easily read data with a 2nd row with units common to phx data
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
#' read_phx("example.csv")
#' read_phx("example.csv", skip = 1) # will ignore 1st line, good for comment lines
#' }
#' @export
read_phx <- function(data, 
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

capture_colnames <- function(x, strip_flags = c("TABLE", "ID", "DV", "MDV", "EVID", 
                                                "KA", "CL", "V", "REP", "ETA", "THETA")) {
  no_table <- strip_flags[!(strip_flags %in% "TABLE")]
  flags <- paste0(no_table, collapse = "|")
  
  for (i in seq_along(x)) {
    if(isTRUE(as.logical(grep(flags, x[i])))) return(x[i])
  }
  warning("could not find column names")
  return(NULL)
}

# capture separator
capture_sep <- function(lines) {
  is_comma <- vapply(lines, function(x) {
    grepl(",", x)
  }, logical(1))
  if(any(is_comma)) {
    return(c(","))
  } else {
    return("auto")
  }
}

#' read nonmem files easily
#' @param path path to file
#' @param header whether header with column names exists
#' @param sep automatically detected by default, however can tell by default. 
#' @param example_name name of column to detect which rows contain header(s)
#' @details 
#' This function is designed specifically for handling nonmem's nonstandard output format, and
#' is especially useful for simulation tables output with NSUB as it will appropriately parse out
#' the additional TABLE and column name rows.
#' 
#' HOWEVER, for tables with standard formatting (eg comma separated with FORMAT=,1PE11.4) and
#' no NSUB, then the `read_phx()` function will likely be slightly faster. This should only be an issue
#' for large (at least 20 MB) files, else the difference will be imperceptible. 
#' @export
read_nonmem <- function(path, header = TRUE, sep = "auto", example_name = NULL) {
  if(!requireNamespace("readr",quietly = TRUE)) {
    stop("Need readr, please install with install.packages(\"readr\")")
  }
  lines <- readr::read_lines(path)
  if(sep == "auto") {
    sep <- capture_sep(lines[1:5])
  }
   if(header) {
     col_name <- stringr::str_trim(capture_colnames(lines[1:5]))
    if (sep == "auto") {
      col_name <- stringr::str_replace_all(col_name, "\\s+", ",")
    } else {
      col_name <- stringr::str_replace_all(col_name, " ", "")
    }
   }
  # if no default col_name start with 
  header_name <- ifelse(is.null(example_name), 
                            ifelse(header, 
                                   ## grab first column name from col_name
                                   stringr::str_split(col_name, ",")[[1]][[1]], 
                 # give default of ID if nothing specified in header or col_name
                                   "ID"), 
                        example_name)
  lines <- clean_nonmem(lines, sep =sep, colname = header_name)
  if(header) {
    output <- suppressWarnings(readr::read_csv(file = paste0(col_name,"\n", lines), na = "."))
  } else {
    
    output <- suppressWarnings(readr::read_csv(file =lines, col_names = FALSE, na = "."))
  }
  return(output) 
}