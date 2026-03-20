# Convert categorical columns to factor

Based on the specifications in the data dictionary, transforms all
categorical columns to factor.

## Usage

``` r
transf_factor(data, study, release = "latest")
```

## Arguments

- data:

  tibble. The data to be transformed. Columns are expected to be in the
  data dictionary. If not, they will be skipped.

- study:

  character. NBDC study (One of `"abcd"` or `"hbcd"`.

- release:

  character. Release version (Default: `"latest"`).

## Value

A tibble with the transformed data.

## Examples

``` r
if (FALSE) { # \dontrun{
transf_factor(data, study = "abcd")
} # }
```
