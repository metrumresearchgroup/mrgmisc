[![wercker status](https://app.wercker.com/status/96e246fc3e13f11190f75df5e7a2786a/m "wercker status")](https://app.wercker.com/project/bykey/96e246fc3e13f11190f75df5e7a2786a)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/dpastoor/PKPDmisc?branch=master&svg=true)](https://ci.appveyor.com/project/dpastoor/PKPDmisc)
PKPDmisc
========

miscellaneous functions for use in pharmacometric analyses

## Key Functions

### Data Ingestion

* [read_nonmem](http://devinpastoor.com/PKPDmisc/reference/read_nonmem.html)
* [read_phx](http://devinpastoor.com/PKPDmisc/reference/read_phx.html)
* [read_xpdb](http://devinpastoor.com/PKPDmisc/reference/read_xpdb.html)
* [write_nonmem](http://devinpastoor.com/PKPDmisc/reference/write_nonmem.html)

### Data Manipulation

* [as_numeric](http://devinpastoor.com/PKPDmisc/reference/as_numeric.html)
* [fill_backwards](http://devinpastoor.com/PKPDmisc/reference/fill_backwards.html)
* [fill_forward](http://devinpastoor.com/PKPDmisc/reference/fill_forward.html)
* [ids_per_plot](http://devinpastoor.com/PKPDmisc/reference/ids_per_plot.html)
* [max_through](http://devinpastoor.com/PKPDmisc/reference/max_through.html)
* [min_through](http://devinpastoor.com/PKPDmisc/reference/min_through.html)
* [set_bins](http://devinpastoor.com/PKPDmisc/reference/set_bins.html)
* [unique_non_numerics](http://devinpastoor.com/PKPDmisc/reference/unique_non_numerics.html)

### Summaries

* [s_cmax](http://devinpastoor.com/PKPDmisc/reference/s_cmax.html)
* [s_pauc](http://devinpastoor.com/PKPDmisc/reference/s_pauc.html)
* [s_quantiles](http://devinpastoor.com/PKPDmisc/reference/s_quantiles.html)
 

### Formatting

* [capitalize_names](http://devinpastoor.com/PKPDmisc/reference/capitalize_names.html)
* [lowercase_names](http://devinpastoor.com/PKPDmisc/reference/lowercase_names.html)
* [char_to_numeric](http://devinpastoor.com/PKPDmisc/reference/char_to_numeric.html)
* [cols_to_numeric](http://devinpastoor.com/PKPDmisc/reference/cols_to_numeric.html)
* [ordinal_to_binary_](http://devinpastoor.com/PKPDmisc/reference/ordinal_to_binary_.html)
* [replace_dots](http://devinpastoor.com/PKPDmisc/reference/replace_dots.html)
* [pad_left](http://devinpastoor.com/PKPDmisc/reference/pad_left.html)

### Visuals

### Pharmacometrics-specific

* [auc_inf](http://devinpastoor.com/PKPDmisc/reference/auc_inf.html)
* [auc_partial](http://devinpastoor.com/PKPDmisc/reference/auc_partial.html)
* [resample_df](http://devinpastoor.com/PKPDmisc/reference/resample_df.html)
* [strip_curves](http://devinpastoor.com/PKPDmisc/reference/strip_curves.html)
* [wam](http://devinpastoor.com/PKPDmisc/reference/wam.html)


### Other miscellaneous 

* [jprint](http://devinpastoor.com/PKPDmisc/reference/jprint.html)
* [view_creator](http://devinpastoor.com/PKPDmisc/reference/view_creator.html)
* [peek](http://devinpastoor.com/PKPDmisc/reference/peek.html)

## Installation information

To install, make sure devtools and Rtools (under windows) is installed then do:

```
# many examples can be replicated with data from the PKPDdatasets package
devtools::install_github("dpastoor/PKPDdatasets")
devtools::install_github("dpastoor/PKPDmisc")
```