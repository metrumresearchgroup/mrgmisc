
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->

[![Build
Status](https://github.com/metrumresearchgroup/mrgmisc/actions/workflows/main.yaml/badge.svg)](https://github.com/metrumresearchgroup/mrgmisc/actions/workflows/main.yaml)
<!-- badges: end -->

## Overview

A set of helpful functions for formatting, manipulating and summarizing
data in the field of pharmacometrics. Many of the functions were
inherited from previous packages `PKPDmisc` and `metumrg`.

Some helpful utilities include:

-   `auc_inf()` and `auc_partial()` calculates (full and partial) area
    under the curve

-   `nasum()` displays a count of NAs by column in a data.frame

-   `pool_df()` displays overlapping names between data.frames

-   `min_through()` and `max_through()` gives the minimum and maximum
    values up to each index in a vector

-   `chunk()` creates custom sized bins for vectors and data.frames

## Documentation

Public documentation of all functions is hosted at
<https://metrumresearchgroup.github.io/mrgmisc/>

## Development

`mrgmisc` uses [pkgr](https://github.com/metrumresearchgroup/pkgr) to
manage development dependencies and
[renv](https://rstudio.github.io/renv/) to provide isolation. To
replicate this environment,

1.  clone the repo

2.  install pkgr

3.  open package in an R session and run `renv::init(bare = TRUE)`

    -   install `renv` \> 0.8.3-4 into default `.libPaths()` if not
        already installed

4.  run `pkgr install` in terminal within package directory

5.  restart session

Then, launch R with the repo as the working directory (open the project
in RStudio). renv will activate and find the project library.

## Getting help

If you encounter a clear bug, please file an issue with a minimal
reproducible example on [mrgmisc](https://github.com/metrumresearchgroup/mrgmisc/issues).
