# Changelog

## NBDCtools 1.1.2

### New Features

- [`join_tabulated()`](https://software.nbdc-datahub.org/NBDCtools/reference/join_tabulated.md)
  and
  [`create_dataset()`](https://software.nbdc-datahub.org/NBDCtools/reference/create_dataset.md)
  gained a `finer_add` argument, controlling what happens when an
  additional table is more finely grained than everything selected in
  `vars`/`tables` (for example a session-level `vars_add` on top of a
  participant-level selection). Such a table cannot be attached without
  adding rows, which conflicts with `vars_add`/`tables_add` never
  creating rows.

  - `"error"` (default): abort, naming the tables and both ways out.
  - `"first"`: keep one value per row and drop the rest, with a warning
    reporting how many. The value kept is the first by the additional
    table’s own identifier columns, i.e. the earliest *available*
    session or run rather than a fixed baseline. The whole row is taken,
    so all variables from that table come from the same session/run and
    a missing value is not skipped in favor of a later one. The finer
    identifier columns are not added to the result.
  - `"all"`: keep every value and add rows. This is the behavior of
    earlier versions.

  Selecting those variables via `vars`/`tables` instead keeps every
  value without making a row ambiguous.

  **This is a breaking change.** Selections that previously expanded
  rows now raise an error by default; pass `finer_add = "all"` to
  restore the old result, or `"first"` for one row per main-row
  identifier combination.

### Bug fixes

- Fixed
  [`join_tabulated()`](https://software.nbdc-datahub.org/NBDCtools/reference/join_tabulated.md)
  returning missing or wrong values when the selected variables come
  from tables with different identifier columns. Two cases were
  affected, both silent:
  - Participant-level variables requested via `vars_add`/`tables_add`
    (such as demographic information) were missing on most rows as soon
    as a session-level additional variable was requested as well, and
    adding an unrelated additional variable could change the values of
    another one. The additional tables are no longer joined to each
    other before being added to the data; each is now attached on its
    own identifier columns.
  - A selection spanning participant-, session- and run-level tables
    gave different results depending on the order in which the variables
    or tables were listed, in some orders splitting a participant across
    two rows and dropping participant-level values. Tables are now
    always joined from the finest identifier set outwards, so the result
    no longer depends on the order of the arguments.
- [`join_tabulated()`](https://software.nbdc-datahub.org/NBDCtools/reference/join_tabulated.md)
  now fails with an informative error, instead of silently duplicating
  rows, when an additional table has more than one row per identifier
  combination.

### Other Changes

- Added the package citation: Zhang, L., Das, B., Celhay, O., Smith, C.
  J., Ziemer, L. R., Dale, A. M., & Linkersdörfer, J. (2026). *NBDC Data
  Tools: Building Analysis-ready Datasets From ABCD and HBCD Releases in
  R and Python*. bioRxiv. <https://doi.org/10.64898/2026.11.02.763757>.
  It is available via `citation("NBDCtools")` (new `inst/CITATION`), the
  package startup message, the package-level documentation, and the
  README.
- Added a note to the README.md about the Python version of the package,
  [`nbdctools`](https://github.com/nbdc-datahub/nbdctools-py), which
  provides both a wrapper around the R package and a pure Python
  implementation.

## NBDCtools 1.1.1

### Bug fixes

- Fixed double import of `utils` package in `DESCRIPTION` file.
- Fixed arrangement of variable levels in
  [`create_bids_sidecar_metadata()`](https://software.nbdc-datahub.org/NBDCtools/reference/create_bids_sidecar_metadata.md)
  function
- Fixed an error in `check_dd()` function that caused an error when
  using a release version. When trying to get `id_cols`, the release
  number was not used in study-specific way, which caused a search in
  both studies, and caused release not found in the second study. Now
  the search is done in a study-specific way.

## NBDCtools 1.1.0

CRAN release: 2026-03-23

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
- Added
  [`add_custom_metadata()`](https://software.nbdc-datahub.org/NBDCtools/reference/add_custom_metadata.md)
  function to allow users to add their own metadata to the NBDCtools
  environment. This is useful for users who have their own metadata. A
  new vignette “Add custom metadata” has been added to demonstrate how
  to use this function.

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
