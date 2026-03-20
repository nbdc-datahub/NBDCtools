# Join tabulated data

Joins selected variables and/or whole tables from the tabulated
data/shadow files into a single data frame. Expects the data files to be
stored in one directory in `.parquet` or `.tsv` format, with one file
per table following the naming convention of the respective NBDC dataset
(from the ABCD or HBCD studies). Typically, this will be the
`rawdata/phenotype/` directory within a BIDS dataset downloaded from the
NBDC Data Hub.

Variables specified in `vars` and `tables` will be full-joined together,
i.e., all rows will be kept, even if they do not have a value for all
columns. Variables specified in `vars_add` will be left-joined to the
variables selected in `vars` and `tables`, i.e., only the values for
already existing rows will be added and no new rows will be created.
This is useful for adding variables to the dataset that are important
for a given analysis but are not the main variables of interest (e.g.,
design/nesting or demographic information). By left-joining these
variables, one avoids creating new rows that contain only missing values
for the main variables of interest selected using `vars` and `tables`.
If the same variables are specified in `vars`/`tables` and
`vars_add`/`tables_add`, the variables in `vars_add`/`tables_add` will
be ignored.

In addition to the main `join_tabulated()` function, there are two
study-specific variations:

- `join_tabulated_abcd()`: for the ABCD study.

- `join_tabulated_hbcd()`: for the HBCD study.

They have the same arguments as the `join_tabulated()` function, except
that the `study` argument is set to the respective study by default, and
should not be set by the user.

## Usage

``` r
join_tabulated(
  dir_data,
  study,
  vars = NULL,
  tables = NULL,
  vars_add = NULL,
  tables_add = NULL,
  release = "latest",
  format = "parquet",
  shadow = FALSE,
  remove_empty_rows = TRUE,
  bypass_ram_check = FALSE,
  ignore_version_mismatch = FALSE
)

join_tabulated_abcd(...)

join_tabulated_hbcd(...)
```

## Arguments

- dir_data:

  character. Path to the directory with the data files in `.parquet` or
  `.tsv` format.

- study:

  character. NBDC study (One of `"abcd"` or `"hbcd"`).

- vars:

  character (vector). Name(s) of variable(s) to be joined. (Default:
  `NULL`, i.e., no variables are selected; one of `tables` or `vars` has
  to be provided).

- tables:

  character (vector). Name(s) of table(s) to be joined (Default: `NULL`,
  i.e., no tables are selected; one of `tables` or `vars` has to be
  provided).

- vars_add:

  character (vector). Name(s) of additional variable(s) to be
  left-joined to the variables selected in `vars` and `tables` (Default:
  `NULL`, i.e., no additional variables are selected)

- tables_add:

  character (vector). Name(s) of additional table(s) to be left-joined
  to the variables selected in `vars` and `tables` (Default: `NULL`,
  i.e., no additional tables are selected)

- release:

  character. Release version (Default: `"latest"`)

- format:

  character. Data format (One of `"parquet"` or `"tsv"`; default:
  `"parquet"`).

- shadow:

  logical. Whether to join the shadow matrix instead of the data table
  (default: `FALSE`).

- remove_empty_rows:

  logical. Whether to filter out rows that have all values missing in
  the joined variables, except for the ID columns (default: `TRUE`).

- bypass_ram_check:

  logical. If `TRUE`, the function will not abort if the number of
  variables exceeds 10000 and current available RAM is less than 75% of
  the estimated RAM usage. This can prevent the long loading time of the
  data, but failing in the middle due to insufficient RAM. For large
  datasets, it is recommended to save 2 times or more of estimated RAM
  before running this function.

  This argument is only used for the ABCD study, as the HBCD data is
  small enough to be loaded without RAM issues with most personal
  computers. As HBCD data grows in the future, this may change.

- ignore_version_mismatch:

  logical. Whether to ignore version mismatch between data files and
  metadata (dd, levels, etc) and proceed with joining anyway (default:
  `FALSE`).

  The function performs a version check by reading a specific file from
  the `dir_data` directory to determine the latest session in the data
  files. This session is then compared to the expected latest session
  for the specified `release`. If there is a mismatch, a warning or
  error is raised depending on the value of this parameter. If set to
  `FALSE`, the function will abort on version mismatch. If set to
  `TRUE`, a warning will be issued, and the function will proceed with
  joining the data.

- ...:

  Additional arguments passed to the underlying function
  `join_tabulated()`

  Note: Turning this parameter to `FALSE` is useful for shadow matrices
  processing. Some shadow-related functions expect the shadow matrix to
  have the same dimensions as the original data to proceed correctly.
  See
  [`shadow_bind_data()`](https://software.nbdc-datahub.org/NBDCtools/reference/shadow_bind_data.md)
  and
  [`shadow_replace_binding_missing()`](https://software.nbdc-datahub.org/NBDCtools/reference/shadow_replace_binding_missing.md)

## Value

A tibble of data or shadow matrix with the joined variables.

## Examples

``` r
if (FALSE) { # \dontrun{
join_tabulated(
  dir_data = "path/to/data/",
  vars     = c("var_1", "var_2", "var_3"),
  tables   = c("table_1", "table_2"),
  study    = "abcd",
  release  = "6.0"
)
} # }
```
