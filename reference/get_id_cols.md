# Get identifier columns

Retrieves the identifier columns for a given study and release version.

In addition to the main `get_id_cols()` function, there are two
study-specific variations:

- `get_id_cols_abcd()`: for the ABCD study.

- `get_id_cols_hbcd()`: for the HBCD study.

They have the same arguments as the `get_id_cols()` function, except
that the `study` argument is set to the respective study by default, and
should not be set by the user.

## Usage

``` r
get_id_cols(study, release = "latest")

get_id_cols_abcd(...)

get_id_cols_hbcd(...)
```

## Arguments

- study:

  character. The study name. One of "abcd" or "hbcd".

- release:

  character. Release version (Default: `"latest"`).

- ...:

  Additional arguments passed to the underlying `get_id_cols()`
  function.

## Value

character vector with the identifier columns.

## Examples

``` r
get_id_cols("abcd")
#> [1] "participant_id" "session_id"    

get_id_cols("hbcd")
#> [1] "participant_id" "session_id"     "run_id"        

get_id_cols_abcd(release = "6.0")
#> [1] "participant_id" "session_id"    

get_id_cols_hbcd(release = "1.0")
#> [1] "participant_id" "session_id"     "run_id"        
```
