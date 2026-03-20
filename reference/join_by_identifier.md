# Join by identifier set

Internal helper for
[`join_tabulated()`](https://software.nbdc-datahub.org/NBDCtools/reference/join_tabulated.md)
that joins requested variables from all tables that have the given (set
of) identifier column(s).

## Usage

``` r
join_by_identifier(
  dir_data,
  dd,
  identifiers,
  format = "parquet",
  shadow = FALSE
)
```

## Arguments

- dir_data:

  character. Path to the directory with the data files in `.parquet` or
  `.tsv` format.

- dd:

  tibble. Data frame with the data dictionary.

- identifiers:

  character (vector). Identifier column(s).

- format:

  character. Data format (One of `"parquet"` or `"tsv"`; default:
  `"parquet"`).

- shadow:

  logical. Whether to join the shadow matrix instead of the data table
  (default: `FALSE`).

## Value

A tibble with the joined variables for the given (set of) identifier
column(s).
