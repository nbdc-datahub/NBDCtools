# Bind the shadow matrix to the data

This function binds the shadow matrix to the data.

## Usage

``` r
shadow_bind_data(
  data,
  shadow = NULL,
  naniar_shadow = FALSE,
  id_cols = union(get_id_cols_abcd(), get_id_cols_hbcd()),
  suffix = "_shadow"
)
```

## Arguments

- data:

  tibble. The data.

- shadow:

  tibble. The shadow matrix. If `naniar_shadow` is `TRUE`, this argument
  is ignored.

- naniar_shadow:

  logical. Whether to use
  [`naniar::as_shadow()`](https://naniar.njtierney.com/reference/as_shadow.html)
  to create the shadow matrix from data instead of providing it as an
  argument.

- id_cols:

  character. The columns to join by (the identifier column(s)) in the
  data and shadow matrices (Default: identifier columns used in ABCD and
  HBCD). In `naniar_shadow = TRUE`, these columns are not included in
  the shadow matrix.

- suffix:

  character. The suffix to add to the shadow columns. Default is
  `"_shadow"`. For example, if the column name is `"var1"` and the
  suffix is `"_shadow"`, the resulted column name will be
  `"var1_shadow"`.

  If `naniar_shadow = TRUE`, the suffix is `_NA`, as this suffix will
  have the most compatibility with other functions in the `naniar`
  package.

## Value

a dataframe of the data matrix with shadow columns. It will be 2x the
size of the original data matrix.

## Details

### Data requirements

If `naniar_shadow = FASLE` and `shadow` is provided, the two dataframes
must have the same columns, order of the columns does not matter, but ID
columns must be the same in both dataframes. If there are extra rows in
the shadow matrix, they will be ignored.

### ABCD and HBCD data

NBDC releases HBCD data with shadow matrices, which can be used for the
`shadow` argument. To work with ABCD data, the option for now is to use
`naniar_shadow = TRUE`, which will create a shadow matrix from the data
using
[`naniar::as_shadow()`](https://naniar.njtierney.com/reference/as_shadow.html).

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
shadow_bind_data(data, shadow)
#> # A tibble: 3 × 6
#>   participant_id session_id  var1  var2 var1_shadow var2_shadow       
#>   <chr>          <chr>      <dbl> <dbl> <chr>       <chr>             
#> 1 1              1             NA    NA Unknown     Wish not to answer
#> 2 2              2             NA     2 NA          NA                
#> 3 3              3              1    NA NA          NA                
if (requireNamespace("naniar", quietly = TRUE)) {
  shadow_bind_data(data, naniar_shadow = TRUE)
}
#> # A tibble: 3 × 6
#>   participant_id session_id  var1  var2 var1_NA var2_NA
#>   <chr>          <chr>      <dbl> <dbl> <fct>   <fct>  
#> 1 1              1             NA    NA NA      NA     
#> 2 2              2             NA     2 NA      !NA    
#> 3 3              3              1    NA !NA     NA     
```
