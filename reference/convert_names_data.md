# Convert column names in a data frame

This function renames columns in a data frame to another type of column
name specified in the data dictionary.

For example, this can be used to convert the ABCD column names
introduced in the 6.0 release to the previously used column names. If
you instead want to convert the column names in a file, use
[`convert_names_file()`](https://software.nbdc-datahub.org/NBDCtools/reference/convert_names_file.md).

Note: Please use this function with caution and make sure that the data
in the converted column is equivalent to the data in the original
column. Also, please make sure that the names can be mapped one-to-one.
Some variables in the ABCD data dictionary have been collapsed from
previous releases and thus might have multiple names in the `name_to`
column that map to a single name (see `skip_sep_check` argument below).

## Usage

``` r
convert_names_data(
  data,
  dd,
  name_from = "name",
  name_to,
  ignore_cols = union(get_id_cols_abcd(), get_id_cols_hbcd()),
  skip_sep_check = FALSE
)
```

## Arguments

- data:

  tibble. The input data frame with columns to be renamed.

- dd:

  tibble. The data dictionary table. One can use
  [`get_dd()`](https://software.nbdc-datahub.org/NBDCtools/reference/get_dd.md)
  family of functions to get the data dictionary for a given study and
  release or provide a custom data dictionary.

- name_from:

  character. The column name type in the data dictionary that the
  columns in `data` currently use (Default: `"name"`). This column must
  exist in the data dictionary.

- name_to:

  character. The column name type in the data dictionary that the
  columns in `data` should be renamed to. This column must exist in the
  data dictionary.

- ignore_cols:

  character vector. The columns to ignore (Default: identifier columns
  used in ABCD and HBCD).

- skip_sep_check:

  logical. Whether to skip the check for `name_to` column's separators
  validation. In our official data dictionaries, some columns have
  multiple names separated by a `"|"` in the same cell.

  For columns with multiple names, it the recommended to use functions
  like
  [`tidyr::separate_rows()`](https://tidyr.tidyverse.org/reference/separate_rows.html)
  to split the names into multiple rows and decide which name to use for
  the renaming by filtering the rows, so that the `name_from` and
  `name_to` columns are one-to-one mapping.

  If `skip_sep_check = FALSE` (default), the function will check if the
  `name_from` or `name_to` columns have the `"|"` separator and will
  throw an error if the separator is found. If `skip_sep_check = TRUE`,
  it means you understand that character strings with `"|"` inside will
  be used for rename mapping, and the function will not check for the
  separator.

## Value

tibble. The data with renamed column names.

## Examples

``` r
if (FALSE) { # \dontrun{
# rename columns to previous ABCD names used by NDA
convert_names_data(
  data,
  dd = get_dd("abcd"),
  name_from = "name",
  name_to = "name_nda"
)

# rename columns to Stata names
convert_names_data(
  data,
  dd = get_dd("abcd"),
  name_from = "name",
  name_to = "name_stata"
)
} # }
```
