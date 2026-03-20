# Convert time columns to `hms` format

This function converts time columns to `hms` format.

## Usage

``` r
transf_time_to_hms(data, study, release = "latest")
```

## Arguments

- data:

  tibble. The data to be converted.

- study:

  character. NBDC study (One of `"abcd"` or `"hbcd"`).

- release:

  character. Release version (Default: `"latest"`).

## Value

A tibble with time columns converted to `hms` format.

## Details

The input data with time columns are expected to have character format
of `"HH:MM:SS"`. If it is not in this format, the function will return
NA for that row.

## Examples

``` r
if (FALSE) { # \dontrun{
transf_time_to_hms(data)
} # }
```
