# Fix binding resulted missingness in shadow matrices

This function replaces the missing values in the shadow matrices. This
is done by checking if the values in shadow matrices are both NA. If
they are, the value in the shadow matrix is replaced with
`Missing due to joining`.

## Usage

``` r
shadow_replace_binding_missing(
  data,
  shadow,
  id_cols = union(get_id_cols_abcd(), get_id_cols_hbcd()),
  replacement = "Missing due to joining"
)
```

## Arguments

- data:

  tibble. The data.

- shadow:

  tibble. The shadow matrix.

- id_cols:

  character (vector). The possible unique identifier columns. The data
  does not need to have all of these columns, but if they are present,
  they will be used to identify unique rows (Default: identifier columns
  used in ABCD and HBCD). For example, the ABCD data usually has only
  `participant_id` and `session_id`, so if `run_id` is provided, it will
  be ignored.

- replacement:

  character. The value to replace the missing values with.

## Value

A tibble of the shadow matrix with missing values replaced.

## Details

Data and shadow requirements: The two dataframes must have the same
columns and the same number of rows. They must have the same column
names, but the order of the columns does not matter. It is recommended
to use the same column order and the same row order (by ID columns) in
both dataframes, which saves some processing time.

## Examples

``` r
shadow <- tibble::tibble(
  participant_id = c("1", "2", "3"),
  session_id = c("1", "2", "3"),
  var1 = c("Unknown", NA, NA),
  var2 = c("Wish not to answer", NA, NA)
)
data <- tibble::tibble(
  participant_id = c("1", "2", "3"),
  session_id = c("1", "2", "3"),
  var1 = c(NA, NA, 1),
  var2 = c(NA, 2, NA)
)
shadow_replace_binding_missing(data, shadow)
#> # A tibble: 3 × 4
#>   participant_id session_id var1                   var2                  
#>   <chr>          <chr>      <chr>                  <chr>                 
#> 1 1              1          Unknown                Wish not to answer    
#> 2 2              2          Missing due to joining NA                    
#> 3 3              3          NA                     Missing due to joining
```
