# Read file

Internal helper for
[`join_by_identifier()`](https://software.nbdc-datahub.org/NBDCtools/reference/join_by_identifier.md)
that reads data/shadow matrix for a given file in either `.parquet` or
`.tsv` format.

## Usage

``` r
read_file(file, dd, format)
```

## Arguments

- file:

  character. Path to the file.

- dd:

  tibble. Data frame with the data dictionary used to select columns and
  determine the column types if reading from a `.tsv` file.

- format:

  character. Data format (One of `"parquet"` or `"tsv"`)

## Value

A tibble with the data/shadow matrix from the file.
