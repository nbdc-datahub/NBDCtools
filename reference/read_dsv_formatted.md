# Read delimiter (tab/comma) separated values file correctly formatted

Reads in a `.tsv` or `.csv` file with correctly formatted column types.
Uses
[`readr::read_tsv()`](https://readr.tidyverse.org/reference/read_delim.html)/[`readr::read_csv()`](https://readr.tidyverse.org/reference/read_delim.html)
internally and specifies the column types explicitly using the
`col_types` argument utilizing information from the data dictionary.
Returns only the identifier columns and the columns specified in the
data dictionary, i.e., all columns in the file that are not specified in
the data dictionary are ignored.

## Usage

``` r
read_dsv_formatted(file, dd, action = "warn")
```

## Arguments

- file:

  character. Path to the `.tsv` or `.csv` file.

- dd:

  tibble. Data dictionary specifying the column types. Only columns
  specified in the data dictionary are read.

- action:

  character. What to do if there are columns in the file that are not
  specified in the data dictionary (One of `"warn"`, `"error"`, or
  `"ignore"`; default: `"warn"`).

## Value

A tibble with the data/shadow matrix read from the `.tsv` or `.csv`
file.

## Details

***WHY THIS IS IMPORTANT:***
[`readr::read_tsv()`](https://readr.tidyverse.org/reference/read_delim.html)/[`readr::read_csv()`](https://readr.tidyverse.org/reference/read_delim.html)
(like other commands to load text files in R or other programming
languages) by default infers the column types from the data. This
doesn't always work perfectly. For example, it may interpret a column
with only integers as a double, or a column with only dates as a
character. Sometimes a column may even be read in completely empty
because, by default,
[`readr::read_tsv()`](https://readr.tidyverse.org/reference/read_delim.html)/[`readr::read_csv()`](https://readr.tidyverse.org/reference/read_delim.html)
only considers the first 1000 rows when inferring the data type and
interprets the column as an empty logical vector if those rows are all
empty. The NBDC datasets store categorical data as integers formatted as
character. By default,
[`readr::read_tsv()`](https://readr.tidyverse.org/reference/read_delim.html)/[`readr::read_csv()`](https://readr.tidyverse.org/reference/read_delim.html)
may interpret them as numeric. By specifying the column types explicitly
based on what is defined in the data dictionary, we can avoid these
issues.

***GENERAL RECOMMENDATION:*** Other file formats like `.parquet`
correctly store the column types and don't need to be handled
explicitly. They also offer other advantages like faster reading speed
and smaller file sizes. As such, these formats should generally be
preferred over `.tsv`/`.csv` files. However, if you have to work with
`.tsv`/`.csv` files, this function can help you avoid common pitfalls.

## Examples

``` r
if (FALSE) { # \dontrun{
dd <- NBDCtools::get_dd("abcd", "6.0")
read_tsv_formatted("path/to/file.tsv", dd)
} # }
```
