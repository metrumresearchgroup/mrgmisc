\name{read_nonmem}
\alias{read_nonmem}
\title{Faster way of reading in nonmem data}
\usage{
read_nonmem(file, sedCmd = NULL, ...)
}
\arguments{
  \item{file}{file to be read in}

  \item{sedCmd}{sed command}

  \item{...}{dots argument to pass to fread}
}
\value{
data.table object
}
\description{
Faster way of reading in nonmem data
}
\details{
uses sed to covert to csv before fread and returns
data.table object only applicable on unix systems (I think)
}

