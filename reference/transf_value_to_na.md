# Convert categorical missingness/non-response codes to `NA`

This function converts the missing codes in the dataset to NA in all
factor columns. Example of missing codes are `999`, `888`, `777`, etc.

## Usage

``` r
transf_value_to_na(
  data,
  missing_codes = c("999", "888", "777", "666", "555", "444", "333", "222"),
  ignore_col_pattern = "__dk$|__dk__l$",
  id_cols = union(get_id_cols_abcd(), get_id_cols_hbcd())
)
```

## Arguments

- data:

  tibble. The labelled dataset and type converted data.

- missing_codes:

  character vector. The missing codes to be converted to NA

- ignore_col_pattern:

  character. A regex pattern to ignore columns that should not be
  converted to NA.

- id_cols:

  character vector. The names of the ID columns to be excluded from the
  conversion (Default: identifier columns used in ABCD and HBCD).

## Value

A tibble of the dataset with missing codes converted to NA

## Details

### Use case

This function works the best with `ABCD` data where the missing codes
are strictly defined. For `HBCD` data, the missing codes are still under
discussion. The function may work, but for some undecided future missing
codes, the function may not work as expected.

In case of `HBCD` data or other aribitrary missing codes that one wishes
to convert to NA, it is recommended to use the
[`sjmisc::set_na_if()`](https://strengejacke.github.io/sjmisc/reference/set_na_if.html)
function instead.

### Input requirements

The data must be type transformed and labelled. See
[`transf_factor()`](https://software.nbdc-datahub.org/NBDCtools/reference/transf_factor.md)
and
[`transf_label()`](https://software.nbdc-datahub.org/NBDCtools/reference/transf_label.md)
for details.

    data <- data |>
      transf_factor() |>
      transf_label()

## Examples

``` r
if (FALSE) { # \dontrun{
data <- data |>
  transf_factor() |>
  transf_label()

transf_value_to_na(data)
} # }
```
