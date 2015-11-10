# PKPDmisc 0.4

* change `set_bins` behavior to no longer replace lower and upper bounds unless bounds
explicity set to NULL
* update basic report template to supress package startup messages and set figures
to look nice when knitting to word
* add `s_quantiles` and `s_quantiles_` to calculate quantile summaries for a column for a data pipeline
* add `view_creator` to create views to pipe data into to dataviewer shiny app