# Convert values to labels for categorical variables

Converts the values of categorical/factor columns (e.g., `"1"`, `"2"`)
to their labels (e.g., `"Male"`, `"Female"`). The value labels will be
set to the values.

## Usage

``` r
transf_value_to_label(data, transf_sess_id = FALSE)
```

## Arguments

- data:

  tibble. The labelled dataset

- transf_sess_id:

  logical. Whether to transform the `session_id` column

## Value

A tibble with factor columns transformed to labels.

## Details

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
transf_value_to_label(data)
transf_value_to_label(data, value_to_na = TRUE)
} # }
```
