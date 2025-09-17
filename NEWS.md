# NBDCtools 1.0.2

## Changes

- Removed startup check for `NBDCtoolsData` package, as CRAN does not allow
  skipping checks. Now the `check_data_pkg_installed` performs the check when
  other functions are called.
- Data from `NBDCtoolsData` is now loaded into a separate environment as 
  caching for faster access.
- Added `NBDCtoolsData` to `Suggests` in `DESCRIPTION` file.
- Removed `is_on_cran()` internal function.
- Used `@examplesIf` roxygen tag to avoid examples being run on CRAN
  if `NBDCtoolsData` is not installed.

## Bug fixes

- Fixed in `transf_factor` function that caused an error when the data
  has no `session_id` column.

# NBDCtools 1.0.1

## Bug fixes

- Fixed an error in the `transf_value_to_label()` function that led to the order
  of categorical levels being incorrect after transformation to labels.
- Fixed an error in the `join_tabulated()` function that occurred if the first
  table/variable in `tables_add`/`vars_add` was static but further
  tables/variables were dynamic.

## Changes

- all `join_by`s in `join_tabulated()` are now dynamically set by intersecting
  with `id_cols`. Hardcoded `join_by` has been removed.

# NBDCtools 1.0.0

## New Features

The initial release of the NBDCtools package.
