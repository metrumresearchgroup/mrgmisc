# mrgmisc 0.1.0

## New features and changes

- `mrgmisc` was constructed with select functions from two older packages
  `PKPDmisc` and `metrumrg`. These packages are no longer being maintained, so 
  this functionality will be maintained in `mrgmisc` moving forward.

- Data manipulation functions brought over from `PKPDmisc` include: `chunk()`,
  `chunk_df()`, `ids_per_plot()`, `max_through()`, `min_through()`, `set_bins()`,
  and `set_bins_df()`.
  
- Data manipulation functions brought over from `metrumrg` include: `posmat()`
  and `snap()`.

- Summarizing functions brought over from `PKPDmisc` include: `s_pauc()` and
  `s_quantiles()`.

- Summarizing functions brought over from `metrumrg` include: `pool()` and
  `nasum()`.

- Formatting functions brought over from `PKPDmisc` include: `capitalize_names()`,
  `ordinal_to_binary_()`, `replace_dots()` and `pad_left()`.

- Formatting functions brought over from `metrumrg` include: `parens()`.

- Pharmacometrics-specific functions brought over from `PKPDmisc` include:
  `auc_inf()`, `auc_partial()` and `resample_df()`.

- Pharmacometrics-specific functions brought over from `metrumrg` include:
  `as_nmctl()`

