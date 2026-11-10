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

Tables are joined on their identifier columns, which differ by table:
participant-level tables are identified by `participant_id` alone,
session-level tables by `participant_id` and `session_id`, and run-level
tables also by `run_id`. A variable from a coarser table is repeated on
every finer row it applies to, so a participant-level variable such as
birth data appears on every session and every run of that participant.
The result does not depend on the order in which variables or tables are
listed, and contains one row per distinct combination of identifier
columns.

If an additional table is more finely grained than everything selected
in `vars`/`tables` (for example a session-level `vars_add` on top of a
participant-level selection), its values cannot all be attached without
adding rows, which conflicts with the rule that `vars_add`/`tables_add`
do not create rows. Neither way out is right for every analysis, so
`finer_add` decides; see that argument for the three options. By default
this is an error, since silently dropping values and silently adding
rows are both surprising.

A third option is to not use `vars_add`/`tables_add` for those variables
at all: selecting them via `vars`/`tables` keeps every value and makes
the finer grain part of the row set, so no row is ambiguous.

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
  ignore_version_mismatch = FALSE,
  finer_add = c("error", "first", "all")
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

- finer_add:

  character. How to treat an additional table that is more finely
  grained than everything selected in `vars`/`tables` (one of `"error"`,
  `"first"` or `"all"`; default: `"error"`).

  - `"error"`: abort and ask for an explicit choice. This is the default
    because the alternatives either drop values or add rows, and which
    is appropriate depends on the analysis.

  - `"first"`: keep one value per row of the main selection and drop the
    rest, with a warning reporting how many were dropped. The value kept
    is the first by the additional table's own identifier columns, i.e.
    the earliest available session or the first run – not necessarily
    the baseline, since a participant need not have one. The whole row
    is taken, so all variables from that table come from the same
    session/run, and a missing value is not skipped over in favor of a
    later one. The finer identifier columns are not added to the result.

  - `"all"`: keep every value, which adds rows to the main selection.
    This was the behavior before version 1.1.2.

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
