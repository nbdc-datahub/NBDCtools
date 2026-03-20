# Add variable/value labels

This function can add variable labels and value labels to the data. The
variable labels are descriptive information about the column, and the
value labels are the levels of the factor variables.

## Usage

``` r
transf_label(
  data,
  study,
  release = "latest",
  add_var_label = TRUE,
  add_value_label = TRUE,
  id_cols_labels = c(participant_id = "Participant identifier", session_id =
    "Event identifier", run_id = "Run identifier")
)
```

## Arguments

- data:

  tibble. The data to be transformed.

- study:

  character. NBDC study (One of `"abcd"` or `"hbcd"`.

- release:

  character. Release version (Default: `"latest"`).

- add_var_label:

  logical. Whether to add variable labels (Default: `TRUE`).

- add_value_label:

  logical. Whether to add value labels (Default: `TRUE`).

- id_cols_labels:

  named character vector. A named vector of labels for the identifier
  columns, with the names being the column names and the values being
  the labels.

## Value

A tibble with the labelled data.

## Details

### Two types of labels

At least one of `add_var_label` or `add_value_label` must be set to
`TRUE`. If both are `FALSE`, an error will be raised.

### Text columns

The
[`transf_factor()`](https://software.nbdc-datahub.org/NBDCtools/reference/transf_factor.md)
function has a `convert_text` argument, which will convert text columns
to unordered factors. When one uses a type transformed data to add
labels, the text-factor columns will not have labels at variable level.

## See also

[`transf_factor()`](https://software.nbdc-datahub.org/NBDCtools/reference/transf_factor.md)
for transforming categorical columns to factors.

## Examples

``` r
if (FALSE) { # \dontrun{
transf_label(data)
} # }
```
