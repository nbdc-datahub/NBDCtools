# Create BIDS sidecar from labelled data

Creates a Brain Imaging Data Structure (BIDS) JSON sidecar file from the
metadata (data dictionary and levels table). Returns the JSON object or
writes it to a file.

## Usage

``` r
create_bids_sidecar_data(
  data,
  study,
  release = "latest",
  var_coding = "values",
  metadata_description = "Dataset exported using NBDCtools",
  path_out = NULL,
  pretty = TRUE
)
```

## Arguments

- data:

  tibble. The raw data or data with labels, see
  [`transf_label()`](https://software.nbdc-datahub.org/NBDCtools/reference/transf_label.md).
  If the data is not labelled, the function will try to label the data
  first.

- study:

  character. NBDC study (One of `"abcd"` or `"hbcd"`)

- release:

  character. Release version (Default: `"latest"`).

- var_coding:

  character. the variable coding, one of "values", "labels". If the data
  is processed with
  [`transf_value_to_label()`](https://software.nbdc-datahub.org/NBDCtools/reference/transf_value_to_label.md),
  please use "labels".

- metadata_description:

  string, the description of the metadata

- path_out:

  character. the path to the output file. If `NULL`, the function will
  return the json object.

- pretty:

  logical. Whether to pretty print the json.

## Value

the json object or the path to the json file

## Details

- If you have a labelled dataset, and want to create data specific BIDS
  sidecar with variable levels from the data, please use
  `create_bids_sidecar_data()`.

- If you want to create a BIDS sidecar without the underlying data,
  please use
  [`create_bids_sidecar_metadata()`](https://software.nbdc-datahub.org/NBDCtools/reference/create_bids_sidecar_metadata.md).

## See also

[`create_bids_sidecar_metadata()`](https://software.nbdc-datahub.org/NBDCtools/reference/create_bids_sidecar_metadata.md)

## Examples

``` r
if (FALSE) { # \dontrun{
data |> create_bids_sidecar_data()
data |> create_bids_sidecar_data(path_out = "data.json")
} # }
```
