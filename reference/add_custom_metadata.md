# Add custom metadata

This function allows users to add custom metadata (data dictionary,
levels table, sessions table) to the package environment. This can be
useful for users who want to use their own metadata instead of the ones
provided by the package, or for testing and development purposes.

## Usage

``` r
add_custom_metadata(dd = NULL, levels = NULL, sessions = NULL)
```

## Arguments

- dd:

  data frame. Custom data dictionary. Should have the same structure as
  the data dictionary provided by the package.

- levels:

  data frame. Custom levels table.

- sessions:

  data frame. Custom sessions table.

## Value

invisible `TRUE`.

## Details

The custom metadata will be stored in the package environment can be
accessed with any function that contains the "release" argument with
`release = "custom"`. For example,
`get_dd(study = "abcd", release = "custom")`.

The default value for `dd`, `levels`, and `sessions` is `NULL`. If any
of them is not `NULL`, it will be added to the package environment. If
the argument is `NULL`, the corresponding metadata will not be added.

## Examples

``` r
add_custom_metadata(
  dd = tibble::tibble(
    name = "var1",
    table_name = "table1",
    identifier_columns = "participant_id"
  )
)
#> ℹ Added custom data dictionary to NBDCtools
get_dd(study = "abcd", release = "custom")
#> # A tibble: 1 × 3
#>   name  table_name identifier_columns
#>   <chr> <chr>      <chr>             
#> 1 var1  table1     participant_id    
```
