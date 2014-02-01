\name{check_gradients}
\alias{check_gradients}
\title{get gradients for nonmem run running via PSN}
\usage{
check_gradients(runnum, wd = NULL, folder_str = ".mod.dir.1",
  full_directory = NULL)
}
\arguments{
  \item{runnum}{Xpose-style run number}

  \item{wd}{relative directly location where PSN run
  results are output}

  \item{folder_str}{folder name besides run number}

  \item{full_directory}{can be specified if folder where
  gradient file stored non-traditional}
}
\description{
get gradients for nonmem run running via PSN
}

