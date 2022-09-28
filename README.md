# mrgmisc
Miscellaneous helpers for getting work done on MetrumRG projects. Inheriting much code from PKPDmisc and metrumrg packages.

## Documentation
Public documentation of all functions is hosted at [https://metrumresearchgroup.github.io/mrgmisc/](https://metrumresearchgroup.github.io/mrgmisc/)

## Development

`mrgmisc` uses [pkgr](https://github.com/metrumresearchgroup/pkgr) to manage
development dependencies and [renv](https://rstudio.github.io/renv/) to
provide isolation. To replicate this environment,

1.  clone the repo

2.  install pkgr

3.  open package in an R session and run `renv::init(bare = TRUE)`

    -   install `renv` \> 0.8.3-4 into default `.libPaths()` if not
        already installed

4.  run `pkgr install` in terminal within package directory

5.  restart session

Then, launch R with the repo as the working directory (open the project
in RStudio). renv will activate and find the project library.

