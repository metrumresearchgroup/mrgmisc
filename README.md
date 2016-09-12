[![wercker status](https://app.wercker.com/status/96e246fc3e13f11190f75df5e7a2786a/m "wercker status")](https://app.wercker.com/project/bykey/96e246fc3e13f11190f75df5e7a2786a)

PKPDmisc
========

miscellaneous functions for use in pharmacometric analyses

## Key Functions

### Data Ingestion

* [read_nonmem](http://devinpastoor.com/PKPDmisc/read_nonmem.html)
* [read_phx](http://devinpastoor.com/PKPDmisc/read_phx.html)
* [read_xpdb](http://devinpastoor.com/PKPDmisc/read_xpdb.html)
* [write_nonmem](http://devinpastoor.com/PKPDmisc/write_nonmem.html)

### Data Manipulation

* [as_numeric](http://devinpastoor.com/PKPDmisc/as_numeric.html)
* [fill_backwards](http://devinpastoor.com/PKPDmisc/fill_backwards.html)
* [fill_forward](http://devinpastoor.com/PKPDmisc/fill_forward.html)
* [ids_per_plot](http://devinpastoor.com/PKPDmisc/ids_per_plot.html)
* [max_through](http://devinpastoor.com/PKPDmisc/max_through.html)
* [min_through](http://devinpastoor.com/PKPDmisc/min_through.html)
* [set_bins](http://devinpastoor.com/PKPDmisc/set_bins.html)
* [unique_non_numerics](http://devinpastoor.com/PKPDmisc/unique_non_numerics.html)

### Summaries

* [s_cmax](http://devinpastoor.com/PKPDmisc/s_cmax.html)
* [s_pauc](http://devinpastoor.com/PKPDmisc/s_pauc.html)
* [s_quantiles](http://devinpastoor.com/PKPDmisc/s_quantiles.html)
 

### Formatting

* [capitalize_names](http://devinpastoor.com/PKPDmisc/capitalize_names.html)
* [lowercase_names](http://devinpastoor.com/PKPDmisc/lowercase_names.html)
* [char_to_numeric](http://devinpastoor.com/PKPDmisc/char_to_numeric.html)
* [cols_to_numeric](http://devinpastoor.com/PKPDmisc/cols_to_numeric.html)
* [ordinal_to_binary_](http://devinpastoor.com/PKPDmisc/ordinal_to_binary_.html)
* [replace_dots](http://devinpastoor.com/PKPDmisc/replace_dots.html)
* [pad_left](http://devinpastoor.com/PKPDmisc/pad_left.html)

### Visuals

### Pharmacometrics-specific

* [auc_inf](http://devinpastoor.com/PKPDmisc/auc_inf.html)
* [auc_partial](http://devinpastoor.com/PKPDmisc/auc_partial.html)
* [resample_df](http://devinpastoor.com/PKPDmisc/resample_df.html)
* [strip_curves](http://devinpastoor.com/PKPDmisc/strip_curves.html)
* [wam](http://devinpastoor.com/PKPDmisc/wam.html)


### Other miscellaneous 

* [jprint](http://devinpastoor.com/PKPDmisc/jprint.html)
* [view_creator](http://devinpastoor.com/PKPDmisc/view_creator.html)
* [peek](http://devinpastoor.com/PKPDmisc/peek.html)

## Installation information

To install, make sure devtools and Rtools (under windows) is installed then do:

```
# many examples can be replicated with data from the PKPDdatasets package
devtools::install_github("dpastoor/PKPDdatasets")
devtools::install_github("dpastoor/PKPDmisc")
```