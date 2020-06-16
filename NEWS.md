# PKPDmisc 2.1.3

* Add `require_conversion` option to `as_numeric()` to allow for providing
  information on values that are not converted, or giving an error with that
  information (implemented by @billdenney)
* Return value from `unique_non_numerics()` is now sorted (implemented by
  @billdenney).

# PKPDmisc 0.4

* change `set_bins` behavior to no longer replace lower and upper bounds unless
  bounds explicity set to NULL
* update basic report template to supress package startup messages and set
  figures to look nice when knitting to word
* add `s_quantiles` and `s_quantiles_` to calculate quantile summaries for a
  column for a data pipeline
* add `view_creator` to create views to pipe data into to dataviewer shiny app
