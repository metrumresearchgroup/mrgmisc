as.nmctl <-
  function(x,...)UseMethod('as.nmctl')

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
    content[flag] <- paste(glue('$',nms),content[flag])
    content[flag] <- sub(' $','',content[flag])
    content
  }

as.list.nmctl <-
  function(x,...)unclass(x)

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
    if(parse)content[thetas] <- lapply(content[thetas],as.initList)
    content
  }

format.nmctl <-
  function(x,...)as.character(x,...)

print.nmctl <-
  function(x,...)print(format(x,...))

read.nmctl <-
  function(con,parse=FALSE,...)as.nmctl(readLines(con,...),parse=parse,...)

write.nmctl <-
  function(x, file='data',ncolumns=1,append=FALSE, sep=" ",...){
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

`[.nmctl` <- function (x, ..., drop = TRUE){
  cl <- oldClass(x)
  class(x) <- NULL
  val <- NextMethod("[")
  class(val) <- cl
  val
}

`[[.nmctl` <- function (x, ..., drop = TRUE)NextMethod("[[")