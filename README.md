[![wercker status](https://app.wercker.com/status/96e246fc3e13f11190f75df5e7a2786a/m "wercker status")](https://app.wercker.com/project/bykey/96e246fc3e13f11190f75df5e7a2786a)

PKPDmisc
========

miscellaneous functions for use in pharmacometric analyses

To install, make sure devtools and Rtools (under windows) is installed then do:

```
devtools::install_github("dpastoor/PKPDmisc")
# many examples can be replicated with data from the PKPDdatasets package
devtools::install_github("dpastoor/PKPDdatasets")
```

NOTE: the most) recent version of Rcpp `Rcpp (>= 0.11.5)`, so please check to make sure a suitable version is installed.

This can be checked via:
```
packageVersion("Rcpp")
```


The CRAN release should be sufficient, and can be installed with `install.packages("Rcpp")` to make sure the most updated version is installed.

## Optional enhanced nonmem function(s)

If you would like to use the function `read_nonmem()` to quickly parse nonmem simulation tables, you must *also* install `readr` via:

```
install.packages("readr")
```
