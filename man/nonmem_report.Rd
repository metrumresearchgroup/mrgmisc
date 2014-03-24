\name{nonmem_report}
\alias{nonmem_report}
\title{create basic folder structure for a nonmem report}
\usage{
nonmem_report(project, secondary_folders = list("Rscripts", "nonmem",
  "nonmem_testing", "lab-notebook", "data"))
}
\arguments{
  \item{project}{name of top level project folder}

  \item{secondary_folders}{list of subfolders}
}
\description{
create basic folder structure for a nonmem report
}
\details{
A placeholder.txt file is created in each subfolder to
allow for initial folder structure to be maintained if
using a version control system such as git (as empty
directories are not kept)

Currently only works if there is no project folder with the
same name, and does not do any checking (will just error
out)
}
\examples{
\dontrun{
nonmem_report("Drug-x")
nonmem_report("Drug-x", list("Rscripts", "nonmem","CTS", "data"))
}
}

