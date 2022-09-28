#' @keywords internal
`contains` <-
  function(pattern,text,...){
    hits <- regexpr(pattern,text,...)
    hits >=0
  }

#' @keywords internal
`%contains%` <- function(x,y)contains(y,x)

#' @keywords internal
`prev` <-
  #function(x)c(NA,x[-length(x)])#last observation
  function(x){
    s <- seq_along(x)
    s <- c(length(s),s[-length(s)])
    x <- x[s]
    if(length(x))x[[1]] <- NA
    x
  }

#' @keywords internal
`runhead` <-
  function(x){#not like last observation
    n <- x != prev(x)
    if(length(n)) n[[1]] <- TRUE
    n
  }

#' Create, Manipulate, Read, and Write NONMEM Control Streams
#'
#' @description 
#' This family of functions implements the class \code{nmctl}: an object
#' model of the NONMEM control stream.  \code{nmctl} models a control stream
#' as a list of records; each record is a character vector. The read and write 
#' functions (not generic) convert \code{nmctl} to and from file format. 
#' The print, format, and \code{as.character} methods display \code{nmctl} 
#' as it normally looks in a text editor.  \code{as.list.nmctl} simply 
#' unclasses its argument. \code{as.nmctl.character} does the heavy work, 
#' breaking up a character vector into records and storing as a list. 
#' If \code{parse} is \code{TRUE}, \code{as.nmctl} attempts to convert 
#' certain records to higher-level objects.
#' 
#' @param x an nmctl object (or analogous character vector)
#' @param \dots extra arguments passed to other functions
#' @param pattern regular expression for first line of a control record
#' @param head regular expression (relative to \code{pattern}) 
#' giving the name of the control record
#' @param tail regular expression (relative to \code{pattern}) giving the
#' balance of the control record
#' @param con a connection or the name of a file to open
#' @param parse whether to create R objects from the character 
#' vectors serving as records
#' @param file passed to \code{write}
#' @param ncolumns passed to \code{write}
#' @param append passed to \code{write}
#' @param sep passed to \code{write}
#' 
#' @details
#' Serendipitously, the record indicator in NONMEM control stream syntax is the 
#' same as the element selector in R list syntax: $.  The convention is that names
#' of elements in \code{nmctl} (lower case) are converted to record types (upper case)
#' in the control stream.  The user is free to add, delete, rearrange, and edit
#' records using standard list manipulation techniques. When printed, records 
#' appear in list order. The write function warns if the 80 character limit is exceeded
#' (not including comments).
#' 
#' @author Tim Bergsma
#' 
#' @export
as.nmctl <-
  function(x,...)UseMethod('as.nmctl')

#' @rdname as.nmctl
#' @export
as.character.nmctl <-
  function(x,...){
    if(length(x)==0) return(character(0))
    x[] <- lapply(x,as.character) # to accommodate novel underlying object types
    order <- sapply(x,length)
    recnums <- 1:length(x)
    record <- rep(recnums,order)
    flag <- runhead(record)
    content <- as.character(unlist(x))
    nms <- toupper(names(x))
    content[flag] <- paste(glue::glue('$',nms),content[flag])
    content[flag] <- sub(' $','',content[flag])
    content
  }

#' @rdname as.nmctl
#' @export
as.list.nmctl <-
  function(x,...)unclass(x)

#' @rdname as.nmctl
#' @export
as.nmctl.character <-
  function(
    x,
    pattern='^ *\\$([^ ]+)( .*)?$',
    head='\\1',
    tail='\\2',
    parse=FALSE,
    ...
  ){
    flag <- contains(pattern,x)
    nms <- sub(pattern,head,x)
    nms <- nms[flag]
    nms <- tolower(nms)
    content <- sub(pattern,tail,x)
    content[flag] <- sub('^ ','',content[flag])
    content <- split(content,cumsum(flag))
    content[['0']] <- NULL	
    names(content) <- nms
    class(content) <- c('nmctl',class(content))
    thetas <- names(content)=='theta'
    if(parse)content[thetas] <- lapply(content[thetas],as.list)
    content
  }

#' @rdname as.nmctl
#' @export
format.nmctl <-
  function(x,...)as.character(x,...)

#' @rdname as.nmctl
#' @export
print.nmctl <-
  function(x,...)print(format(x,...))

#' @rdname as.nmctl
#' @export
read.nmctl <-
  function(con,parse=FALSE,...)as.nmctl(readLines(con,...),parse=parse,...)

#' @rdname as.nmctl
#' @export
write.nmctl <-
  function(x, file='data',ncolumns=1,append=FALSE, sep=" ", ...){
    out <- format(x)
    write(
      out,
      file=file,
      ncolumns=ncolumns,
      append=append, 
      sep=sep,
      ...
    )
  }
