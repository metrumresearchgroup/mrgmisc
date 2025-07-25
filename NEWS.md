# mrgmisc 0.3.0

- Added functions to render paths for various project needs
  - Returns a directory path
    - `this_dir_name()`
    - `this_dir_path()`
    - `this_dir_proj()`
  - Returns a file path
    - `this_file_name()`
    - `this_file_path()`
    - `this_file_proj()`
  - Format a file or directory path relative to the project root
    - `proj_rel()`
    
- Added functions to set options for table and figure outputs
  - `figures_to()`
  - `tables_to()`
  - `mrg_script()`
  - `tf_options()`
  - `tf_options_clear()`


# mrgmisc 0.2.2

## Bug fixes

 - The latest `resample_df` refactor done in #51 caused unnecessary join messages
 and hurt performance to a notable degree. These changes were reverted, and the
 messages referred to in #51 were addressed by specifying the `relationship` for
 `dplyr >= 1.1.1` (#56).

# mrgmisc 0.2.1

- Added `row_compare()` function to find columns where rows are not identical. (#47)

## Bug fixes

- Updated resampling functions to remove `dplyr::left_join()` warning. (#51)

- Updated `s_quantiles()` to remove `dplyr::summarize_()` warning. (#50)

# mrgmisc 0.2.0

## New features and changes

- Updated `pool` to keep output list names consistent. (#45)

## Bug fixes

- `numeric_time` was udpated to work with negative times. (#43)

# mrgmisc 0.1.5

## New features and changes

- Updates to improve mpn.scorecard score. (#30)

# mrgmisc 0.1.4

## New features and changes

- `nsub` now only takes quoted arguments and will default to ID and USUBJID as subject columns. (#25)

- `is_distinct` added to the package. (#25)

# mrgmisc 0.1.3

## New features and changes

- `pool` and `pool_df` now include data.frame names in comparison. (#19)

- `saerch_df` added to the package to search for all occurrences of a value in a data.frame. (#20)

## Bug fixes

- Fixed incorrect `glue` function using `parens()`. (#21)

# mrgmisc 0.1.2

## Bug fixes

- Fixed incorrect `glue` function used in `read.nmctl()`. (#13)

# mrgmisc 0.1.1

## New features and changes

- Moved `readr` and `purrr` from imports to suggests. (#8)

## Bug fixes

- Replaced `nasum` function from `metrumRG` version to allow it to work with
  grouped data.frames and only output columns with more than 0 `NA` values. (#7)

- `nsub` was returning 1 regardless of the number of ID in the data set. Fixed to
  now return the appropriate number of subject IDs. (#7)

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
