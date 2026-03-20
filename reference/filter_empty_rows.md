# Filter empty rows

This function filters out rows that are empty

## Usage

``` r
filter_empty_rows(
  data,
  id_cols = union(get_id_cols_abcd(), get_id_cols_hbcd())
)
```

## Arguments

- data:

  tibble. The data to be filtered.

- id_cols:

  character (vector). The names of the ID columns to be excluded from
  the filtering (Default: identifier columns used in ABCD and HBCD).

## Value

A tibble with the filtered data.

## Examples

``` r
data <- tibble::tibble(
  participant_id = c("sub-001", "sub-002", "sub-003"),
  session_id = c("ses-001", "ses-001", "ses-002"),
  var1 = c(NA, NA, 1),
  var2 = c(NA, NA, 2),
  var3 = c(NA, NA, 3)
)
filter_empty_rows(data)
#> # A tibble: 1 × 5
#>   participant_id session_id  var1  var2  var3
#>   <chr>          <chr>      <dbl> <dbl> <dbl>
#> 1 sub-003        ses-002        1     2     3
```
