# Changelog

## NBDCtools 1.0.3

### New Features

- Added
  [`create_bids_sidecar_metadata()`](https://software.nbdc-datahub.org/NBDCtools/reference/create_bids_sidecar_metadata.md)
  function to create BIDS sidecar from metadata without using any
  processed datasets. The old `create_bids_sidecar()` function is now
  renamed to
  [`create_bids_sidecar_data()`](https://software.nbdc-datahub.org/NBDCtools/reference/create_bids_sidecar_data.md).
- Added `get_all_releases()` and
  [`get_latest_release()`](https://software.nbdc-datahub.org/NBDCtools/reference/get_releases.md)
  functions to retrieve information about all data releases, the latest
  release, respectively.
- Added a validation step in
  [`join_tabulated()`](https://software.nbdc-datahub.org/NBDCtools/reference/join_tabulated.md)
  that checks if the specified release version is the same as the data
  version of files in the `dir_data` directory. This is done by
  searching for a special file by study that contains the correct
  session IDs for that release. If this file is not found, users will
  get a warning message and futher validation is skipped. If the file is
  found but the session IDs do not match, an error is raised.

### Other Changes

- Updated instructions on how to install install the package from a
  release downloaded tarball file in the README.md.

## NBDCtools 1.0.2

CRAN release: 2025-10-05

### Changes

- Removed startup check for `NBDCtoolsData` package, as CRAN does not
  allow skipping checks. Now the `check_data_pkg_installed` performs the
  check when other functions are called.
- Data from `NBDCtoolsData` is now loaded into a separate environment as
  caching for faster access.
- Added `NBDCtoolsData` to `Suggests` in `DESCRIPTION` file.
- Removed `is_on_cran()` internal function.
- Used `@examplesIf` roxygen tag to avoid examples being run on CRAN if
  `NBDCtoolsData` is not installed.

### Bug fixes

- Fixed in `transf_factor` function that caused an error when the data
  has no `session_id` column.

## NBDCtools 1.0.1

CRAN release: 2025-09-10

### Bug fixes

- Fixed an error in the
  [`transf_value_to_label()`](https://software.nbdc-datahub.org/NBDCtools/reference/transf_value_to_label.md)
  function that led to the order of categorical levels being incorrect
  after transformation to labels.
- Fixed an error in the
  [`join_tabulated()`](https://software.nbdc-datahub.org/NBDCtools/reference/join_tabulated.md)
  function that occurred if the first table/variable in
  `tables_add`/`vars_add` was static but further tables/variables were
  dynamic.

### Changes

- all `join_by`s in
  [`join_tabulated()`](https://software.nbdc-datahub.org/NBDCtools/reference/join_tabulated.md)
  are now dynamically set by intersecting with `id_cols`. Hardcoded
  `join_by` has been removed.

## NBDCtools 1.0.0

### New Features

The initial release of the NBDCtools package.
