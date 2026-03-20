# Filter empty columns

This function filters out columns that are empty.

## Usage

``` r
filter_empty_cols(
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
  var1 = c(NA, NA, NA),
  var2 = c(NA, NA, 2),
  var3 = c(NA, NA, 3)
)
filter_empty_cols(data)
#> # A tibble: 3 × 4
#>   participant_id session_id  var2  var3
#>   <chr>          <chr>      <dbl> <dbl>
#> 1 sub-001        ses-001       NA    NA
#> 2 sub-002        ses-001       NA    NA
#> 3 sub-003        ses-002        2     3
```
