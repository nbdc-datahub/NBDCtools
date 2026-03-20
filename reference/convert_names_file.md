# Convert column names in a file

This function replaces all matched column names in a file with another
type of column name specified in the data dictionary.

For example, this function can be used to convert script files that
specified previously used column names to the the ABCD column names
introduced in the 6.0 release. If you instead want to convert the column
names in a data frame, use
[`convert_names_data()`](https://software.nbdc-datahub.org/NBDCtools/reference/convert_names_data.md).

Note: Please use this function with caution and make sure that the data
in the converted column is equivalent to the data in the original
column. Also, please make sure that the names can be mapped one-to-one.
Some variables in the ABCD data dictionary have been collapsed from
previous releases and thus might have multiple names in the `name_from`
column that map to a single name (see `skip_sep_check` argument below).

## Usage

``` r
convert_names_file(
  file_in,
  file_out = NULL,
  dd,
  name_from,
  name_to,
  skip_sep_check = FALSE
)
```

## Arguments

- file_in:

  character. The input file path.

- file_out:

  character. The output file path. If not provided, defaults to the
  input file path with a "\_converted" suffix.

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

- skip_sep_check:

  logical. Whether to skip the check for `name_from` and `name_to`
  columns' separators validation. In our official data dictionaries,
  some columns have multiple names separated by a `"|"` in the same
  cell.

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

character. The path to the output file with converted names, invisible.

## Details

### Word matching

The function uses word boundaries to match the names in the file. It
Uses regex word boundaries (`\\b`) to ensure exact word matches. This
prevents partial matches within larger words. For example, matching
"age" will not match "cage" or "page".

### Speed

The data dictionary is big from
[`get_dd()`](https://software.nbdc-datahub.org/NBDCtools/reference/get_dd.md),
so the function would loop through all the names in the data dictionary.
If there are only a few names to replace, it is the best to trim the
data dictionary to only those names before using this function.

## Examples

``` r
if (FALSE) { # \dontrun{
convert_names_file(
  file_in = "analysis_script.R",
  dd = get_dd("abcd"),
  name_from = "name_nda",
  name_to = "name"
)

# Specify custom output file
convert_names_file(
  file_in = "analysis_script.py",
  file_out = "analysis_script_new.py",
  dd = get_dd("abcd"),
  name_from = "name_nda",
  name_to = "name"
)
} # }
```
