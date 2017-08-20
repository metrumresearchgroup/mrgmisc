[![wercker status](https://app.wercker.com/status/96e246fc3e13f11190f75df5e7a2786a/m "wercker status")](https://app.wercker.com/project/bykey/96e246fc3e13f11190f75df5e7a2786a)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/dpastoor/PKPDmisc?branch=master&svg=true)](https://ci.appveyor.com/project/dpastoor/PKPDmisc)
PKPDmisc
========

miscellaneous functions for use in pharmacometric analyses

## Key Functions

### Data Ingestion

* [read_nonmem](https://dpastoor.github.io/PKPDmisc/reference/read_nonmem.html)
* [read_phx](https://dpastoor.github.io/PKPDmisc/reference/read_phx.html)
* [read_xpdb](https://dpastoor.github.io/PKPDmisc/reference/read_xpdb.html)
* [write_nonmem](https://dpastoor.github.io/PKPDmisc/reference/write_nonmem.html)

### Data Manipulation

* [as_numeric](https://dpastoor.github.io/PKPDmisc/reference/as_numeric.html)
* [fill_backwards](https://dpastoor.github.io/PKPDmisc/reference/fill_backward.html)
* [fill_forward](https://dpastoor.github.io/PKPDmisc/reference/fill_forward.html)
* [ids_per_plot](https://dpastoor.github.io/PKPDmisc/reference/ids_per_plot.html)
* [max_through](https://dpastoor.github.io/PKPDmisc/reference/max_through.html)
* [min_through](https://dpastoor.github.io/PKPDmisc/reference/min_through.html)
* [set_bins](https://dpastoor.github.io/PKPDmisc/reference/set_bins.html)
* [unique_non_numerics](https://dpastoor.github.io/PKPDmisc/reference/unique_non_numerics.html)

### Summaries

* [s_pauc](https://dpastoor.github.io/PKPDmisc/reference/s_pauc.html)
* [s_quantiles](https://dpastoor.github.io/PKPDmisc/reference/s_quantiles.html)
 

### Formatting

* [capitalize_names](https://dpastoor.github.io/PKPDmisc/reference/capitalize_names.html)
* [lowercase_names](https://dpastoor.github.io/PKPDmisc/reference/lowercase_names.html)
* [char_to_numeric](https://dpastoor.github.io/PKPDmisc/reference/char_to_numeric.html)
* [cols_to_numeric](https://dpastoor.github.io/PKPDmisc/reference/cols_to_numeric.html)
* [ordinal_to_binary_](https://dpastoor.github.io/PKPDmisc/reference/ordinal_to_binary.html)
* [replace_dots](https://dpastoor.github.io/PKPDmisc/reference/replace_dots.html)
* [pad_left](https://dpastoor.github.io/PKPDmisc/reference/pad_left.html)

### Visuals

### Pharmacometrics-specific

* [auc_inf](https://dpastoor.github.io/PKPDmisc/reference/auc_inf.html)
* [auc_partial](https://dpastoor.github.io/PKPDmisc/reference/auc_partial.html)
* [resample_df](https://dpastoor.github.io/PKPDmisc/reference/resample_df.html)
* [strip_curves](https://dpastoor.github.io/PKPDmisc/reference/strip_curves.html)
* [wam](https://dpastoor.github.io/PKPDmisc/reference/wam.html)


### Other miscellaneous 

* [jprint](https://dpastoor.github.io/PKPDmisc/reference/jprint.html)
* [view_creator](https://dpastoor.github.io/PKPDmisc/reference/view_creator.html)
* [peek](https://dpastoor.github.io/PKPDmisc/reference/peek.html)

## Installation information

To install, make sure devtools and Rtools (under windows) is installed then do:

```
# many examples can be replicated with data from the PKPDdatasets package
devtools::install_github("dpastoor/PKPDdatasets")
devtools::install_github("dpastoor/PKPDmisc")
```