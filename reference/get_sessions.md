# Get sessions table

Retrieves the sessions table for a given study and release version.
Wrapper around
[`get_metadata()`](https://software.nbdc-datahub.org/NBDCtools/reference/get_metadata.md).

In addition to the main `get_sessions()` function, there are two
study-specific variations:

- `get_sessions_abcd()`: for the ABCD study.

- `get_sessions_hbcd()`: for the HBCD study.

They have the same arguments as the `get_sessions()` function, except
that the `study` argument is set to the respective study by default, and
should not be set by the user.

## Usage

``` r
get_sessions(study, release = "latest")

get_sessions_abcd(...)

get_sessions_hbcd(...)
```

## Arguments

- study:

  character. The study name. One of "abcd" or "hbcd".

- release:

  character. Release version (Default: `"latest"`).

- ...:

  Additional arguments passed to the underlying `get_sessions()`
  function.

## Value

Data frame with the sessions table.

## Examples

``` r
get_sessions("abcd")
#> # A tibble: 27 × 4
#>    session_id label    order     n
#>    <chr>      <chr>    <dbl> <dbl>
#>  1 ses-00S    Screener     1 11867
#>  2 ses-00A    Baseline     2 11868
#>  3 ses-00M    0.5 Year     3 11388
#>  4 ses-01A    1 Year       4 11219
#>  5 ses-01M    1.5 Year     5 11082
#>  6 ses-02A    2 Year       6 10973
#>  7 ses-02M    2.5 Year     7 10253
#>  8 ses-03A    3 Year       8 10450
#>  9 ses-03M    3.5 Year     9  9573
#> 10 ses-04A    4 Year      10  9739
#> # ℹ 17 more rows

get_sessions("hbcd")
#> # A tibble: 5 × 2
#>   session_id label  
#>   <chr>      <chr>  
#> 1 ses-V01    Visit 1
#> 2 ses-V02    Visit 2
#> 3 ses-V03    Visit 3
#> 4 ses-V04    Visit 4
#> 5 ses-V05    Visit 5

get_sessions_abcd(release = "6.0")
#> # A tibble: 26 × 2
#>    session_id label   
#>    <fct>      <fct>   
#>  1 ses-00S    Screener
#>  2 ses-00A    Baseline
#>  3 ses-00M    0.5 Year
#>  4 ses-01A    1 Year  
#>  5 ses-01M    1.5 Year
#>  6 ses-02A    2 Year  
#>  7 ses-02M    2.5 Year
#>  8 ses-03A    3 Year  
#>  9 ses-03M    3.5 Year
#> 10 ses-04A    4 Year  
#> # ℹ 16 more rows

get_sessions_hbcd(release = "1.0")
#> # A tibble: 3 × 2
#>   session_id label  
#>   <chr>      <chr>  
#> 1 ses-V01    Visit 1
#> 2 ses-V02    Visit 2
#> 3 ses-V03    Visit 3
```
