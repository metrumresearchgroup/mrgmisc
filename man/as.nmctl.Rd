% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/as_nmctl.R
\name{as.nmctl}
\alias{as.nmctl}
\alias{as.character.nmctl}
\alias{as.list.nmctl}
\alias{as.nmctl.character}
\alias{format.nmctl}
\alias{print.nmctl}
\alias{read.nmctl}
\alias{write.nmctl}
\title{Create, Manipulate, Read, and Write NONMEM Control Streams}
\usage{
as.nmctl(x, ...)

\method{as.character}{nmctl}(x, ...)

\method{as.list}{nmctl}(x, ...)

\method{as.nmctl}{character}(
  x,
  pattern = "^ *\\\\$([^ ]+)( .*)?$",
  head = "\\\\1",
  tail = "\\\\2",
  parse = FALSE,
  ...
)

\method{format}{nmctl}(x, ...)

\method{print}{nmctl}(x, ...)

read.nmctl(con, parse = FALSE, ...)

write.nmctl(x, file = "data", ncolumns = 1, append = FALSE, sep = " ", ...)
}
\arguments{
\item{x}{an nmctl object (or analogous character vector)}

\item{\dots}{extra arguments passed to other functions}

\item{pattern}{regular expression for first line of a control record}

\item{head}{regular expression (relative to \code{pattern}) 
giving the name of the control record}

\item{tail}{regular expression (relative to \code{pattern}) giving the
balance of the control record}

\item{parse}{whether to create R objects from the character 
vectors serving as records}

\item{con}{a connection or the name of a file to open}

\item{file}{passed to \code{write}}

\item{ncolumns}{passed to \code{write}}

\item{append}{passed to \code{write}}

\item{sep}{passed to \code{write}}
}
\description{
This family of functions implements the class \code{nmctl}: an object
model of the NONMEM control stream.  \code{nmctl} models a control stream
as a list of records; each record is a character vector. The read and write 
functions (not generic) convert \code{nmctl} to and from file format. 
The print, format, and \code{as.character} methods display \code{nmctl} 
as it normally looks in a text editor.  \code{as.list.nmctl} simply 
unclasses its argument. \code{as.nmctl.character} does the heavy work, 
breaking up a character vector into records and storing as a list. 
If \code{parse} is \code{TRUE}, \code{as.nmctl} attempts to convert 
certain records to higher-level objects.
}
\details{
Serendipitously, the record indicator in NONMEM control stream syntax is the 
same as the element selector in R list syntax: $.  The convention is that names
of elements in \code{nmctl} (lower case) are converted to record types (upper case)
in the control stream.  The user is free to add, delete, rearrange, and edit
records using standard list manipulation techniques. When printed, records 
appear in list order. The write function warns if the 80 character limit is exceeded
(not including comments).
}
\author{
Tim Bergsma
}
