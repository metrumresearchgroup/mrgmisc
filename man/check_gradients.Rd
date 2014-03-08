\name{check_gradients}
\alias{check_gradients}
\title{get gradients for nonmem run running via PSN}
\usage{
check_gradients(runnum, wd = NULL, folder_str = ".mod.dir.1",
  full_directory = NULL, print = TRUE)
}
\arguments{
  \item{runnum}{Xpose-style run number}

  \item{wd}{relative directly location where PSN run
  results are output}

  \item{folder_str}{folder name besides run number}

  \item{full_directory}{can be specified if folder where
  gradient file stored non-traditional}

  \item{print}{print visual plot of gradients}
}
\description{
get gradients for nonmem run running via PSN
}
\examples{
\dontrun{
check_gradients(001)
check_gradients(001, wd = "~/ProjX/NONMEM")
check_gradients(001, folder_str = "modelfit_dir1")
check_gradients(001, full_directory = "~/user/ProjX/NONMEM/run001.mod.dir.1/NM_run2/psn.grd")
}
}

