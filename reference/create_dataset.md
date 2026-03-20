# Create a dataset

This high-level function simplifies the process of creating a dataset
from the ABCD or HBCD Study data by allowing users to create an
analysis-ready dataset in a single step. It executes the lower-level
functions provided in the `NBDCtools` package in sequence to load, join,
and transform the data.

The function expects study data to be stored as one `.parquet` or `.tsv`
file per database table within a specified directory, provided as
`dir_data`. Variables specified in `vars` and `tables` will be
full-joined together, while variables specified in `vars_add` and
`tables_add` will be left-joined to these variables. For more details,
see
[`join_tabulated()`](https://software.nbdc-datahub.org/NBDCtools/reference/join_tabulated.md).

In addition to the main `create_dataset()` function, there are two
study-specific variations:

- `create_dataset_abcd()`: for the ABCD study.

- `create_dataset_hbcd()`: for the HBCD study.

They have the same arguments as the `create_dataset()` function, except
that the `study` argument is set to the respective study by default, and
should not be set by the user.

## Usage

``` r
create_dataset(
  dir_data,
  study,
  vars = NULL,
  tables = NULL,
  vars_add = NULL,
  tables_add = NULL,
  release = "latest",
  format = "parquet",
  bypass_ram_check = FALSE,
  ignore_version_mismatch = FALSE,
  categ_to_factor = TRUE,
  add_labels = TRUE,
  value_to_label = FALSE,
  value_to_na = FALSE,
  time_to_hms = FALSE,
  bind_shadow = FALSE,
  ...
)

create_dataset_abcd(...)

create_dataset_hbcd(...)
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

- categ_to_factor:

  logical. Whether to convert categorical variables to factors class,
  see
  [`transf_factor()`](https://software.nbdc-datahub.org/NBDCtools/reference/transf_factor.md)
  (Default: `TRUE`).

- add_labels:

  logical. Whether to adds variable and value labels to the variables,
  see
  [`transf_label()`](https://software.nbdc-datahub.org/NBDCtools/reference/transf_label.md)
  (Default: `TRUE`).

- value_to_label:

  logical. Whether to convert the categorical variables' numeric values
  to labels, see
  [`transf_value_to_label()`](https://software.nbdc-datahub.org/NBDCtools/reference/transf_value_to_label.md)
  (Default: `FALSE`). To run this process, `categ_to_factor` and
  `add_labels` must be `TRUE`.

- value_to_na:

  logical. Whether to convert categorical missingness/non-response codes
  to `NA`, see
  [`transf_value_to_na()`](https://software.nbdc-datahub.org/NBDCtools/reference/transf_value_to_na.md)
  (Default: `FALSE`). To run this process, `categ_to_factor` and
  `add_labels` must be `TRUE`.

- time_to_hms:

  logical. Whether to convert time variables to `hms` class, see
  [`transf_time_to_hms()`](https://software.nbdc-datahub.org/NBDCtools/reference/transf_time_to_hms.md)
  (Default: `FALSE`).

- bind_shadow:

  logical. Whether to bind the shadow matrix to the dataset (Default:
  `FALSE`). See more in details.

- ...:

  additional arguments passed to downstream functions after the
  [`join_tabulated()`](https://software.nbdc-datahub.org/NBDCtools/reference/join_tabulated.md)
  step. See examples for details.

## Value

A tibble with the analysis-ready dataset.

## Details

### Order

This high-level function executes the different steps in the following
order:

1.  Read the data/shadow matrix using
    [`join_tabulated()`](https://software.nbdc-datahub.org/NBDCtools/reference/join_tabulated.md).

2.  Convert categorical variables to factors using
    [`transf_factor()`](https://software.nbdc-datahub.org/NBDCtools/reference/transf_factor.md).

3.  Add labels to the variables and values using
    [`transf_label()`](https://software.nbdc-datahub.org/NBDCtools/reference/transf_label.md).

4.  Convert categorical variables' numeric values to labels using
    [`transf_value_to_label()`](https://software.nbdc-datahub.org/NBDCtools/reference/transf_value_to_label.md).

5.  Convert categorical missingness/non-response codes to `NA` using
    [`transf_value_to_na()`](https://software.nbdc-datahub.org/NBDCtools/reference/transf_value_to_na.md).

6.  Convert time variables to `hms` class using
    [`transf_time_to_hms()`](https://software.nbdc-datahub.org/NBDCtools/reference/transf_time_to_hms.md).

7.  If `bind_shadow` and the study is `"HBCD"`, replace the missing
    values in the shadow due to joining multiple datasets using
    [`shadow_replace_binding_missing()`](https://software.nbdc-datahub.org/NBDCtools/reference/shadow_replace_binding_missing.md).

8.  Bind the shadow matrix to the data using
    [`shadow_bind_data()`](https://software.nbdc-datahub.org/NBDCtools/reference/shadow_bind_data.md).

Not all steps are executed by default. The above order represents the
maximal order of execution.

### `bind_shadow`

If `bind_shadow` is `TRUE`, the shadow matrix will be added to the data
using
[`shadow_bind_data()`](https://software.nbdc-datahub.org/NBDCtools/reference/shadow_bind_data.md).

- **HBCD study:** For the `HBCD` study, this function uses the shadow
  matrix from the `dir_data` directory by default (the HBCD Study
  releases a `_shadow.parquet`/`_shadow.tsv` file per table that
  accompanies the data). Alternatively, one can set
  `naniar_shadow = TRUE` as part of the `...` arguments to use
  [`naniar::as_shadow()`](https://naniar.njtierney.com/reference/as_shadow.html)
  to create a shadow matrix from the data.

- **ABCD study:** The `ABCD` Study does not currently release shadow
  matrices. If `bind_shadow` is set to `TRUE`, the function will create
  the shadow matrix from the data using
  [`naniar::as_shadow()`](https://naniar.njtierney.com/reference/as_shadow.html);
  no extra `naniar_shadow = TRUE` argument is needed.

## Examples

``` r
if (FALSE) { # \dontrun{
# most common use case
create_dataset(
  dir_data = "6_0/data",
  study = "abcd",
  vars = c("var1", "var2", "var3")
)

# to handle with tagged missingness
create_dataset(
  dir_data = "1_0/data",
  study = "hbcd",
  vars = c("var1", "var2", "var3"),
  value_to_na = TRUE
)

# to bind shadow matrices to the data
create_dataset(
  dir_data = "1_0/data/",
  study = "hbcd",
  vars = c("var1", "var2", "var3"),
  bind_shadow = TRUE
)

# to use the additional arguments
# for example in `value_to_na` option, the underlying function
# `transf_value_to_na()` has 2 more arguments,
# which can be passed to the `create_dataset()` function
create_dataset(
  dir_data = "6_0/data",
  study = "abcd",
  vars = c("var1", "var2", "var3"),
  value_to_na = TRUE,
  missing_codes = c("999", "888", "777", "666", "555", "444", "333", "222"),
  ignore_col_pattern = "__dk$|__dk__l$"
)

# use study specific functions
create_dataset_abcd(
  dir_data = "6_0/data",
  vars = c("var1", "var2", "var3")
)
} # }
```
