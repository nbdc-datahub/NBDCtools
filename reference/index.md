# Package index

## Create dataset

High-level functions to create datasets

- [`create_dataset()`](https://software.nbdc-datahub.org/NBDCtools/reference/create_dataset.md)
  [`create_dataset_abcd()`](https://software.nbdc-datahub.org/NBDCtools/reference/create_dataset.md)
  [`create_dataset_hbcd()`](https://software.nbdc-datahub.org/NBDCtools/reference/create_dataset.md)
  : Create a dataset

## Read and join data

Read and join data released in the NBDC Data Hub.

- [`join_tabulated()`](https://software.nbdc-datahub.org/NBDCtools/reference/join_tabulated.md)
  [`join_tabulated_abcd()`](https://software.nbdc-datahub.org/NBDCtools/reference/join_tabulated.md)
  [`join_tabulated_hbcd()`](https://software.nbdc-datahub.org/NBDCtools/reference/join_tabulated.md)
  : Join tabulated data
- [`read_dsv_formatted()`](https://software.nbdc-datahub.org/NBDCtools/reference/read_dsv_formatted.md)
  : Read delimiter (tab/comma) separated values file correctly formatted

## Transform data

Transform/format data released in the NBDC Data Hub.

- [`transf_factor()`](https://software.nbdc-datahub.org/NBDCtools/reference/transf_factor.md)
  : Convert categorical columns to factor

- [`transf_label()`](https://software.nbdc-datahub.org/NBDCtools/reference/transf_label.md)
  : Add variable/value labels

- [`transf_time_to_hms()`](https://software.nbdc-datahub.org/NBDCtools/reference/transf_time_to_hms.md)
  :

  Convert time columns to `hms` format

- [`transf_value_to_label()`](https://software.nbdc-datahub.org/NBDCtools/reference/transf_value_to_label.md)
  : Convert values to labels for categorical variables

- [`transf_value_to_na()`](https://software.nbdc-datahub.org/NBDCtools/reference/transf_value_to_na.md)
  :

  Convert categorical missingness/non-response codes to `NA`

## Filter data

Filter/subset data released in the NBDC Data Hub.

- [`filter_empty_cols()`](https://software.nbdc-datahub.org/NBDCtools/reference/filter_empty_cols.md)
  : Filter empty columns
- [`filter_empty_rows()`](https://software.nbdc-datahub.org/NBDCtools/reference/filter_empty_rows.md)
  : Filter empty rows
- [`filter_events_abcd()`](https://software.nbdc-datahub.org/NBDCtools/reference/filter_events_abcd.md)
  : Filter ABCD events
- [`filter_id_events()`](https://software.nbdc-datahub.org/NBDCtools/reference/filter_id_events.md)
  : Filter ID/events

## Shadow matrices

Work with shadow matrices for data in the NBDC Data Hub.

- [`shadow_bind_data()`](https://software.nbdc-datahub.org/NBDCtools/reference/shadow_bind_data.md)
  : Bind the shadow matrix to the data
- [`shadow_replace_binding_missing()`](https://software.nbdc-datahub.org/NBDCtools/reference/shadow_replace_binding_missing.md)
  : Fix binding resulted missingness in shadow matrices

## Metadata

Retrieve metadata (data dictionary, levels table, sessions table,
identifier columns, release numbers) for studies released in the NBDC
Data Hub.

- [`get_metadata()`](https://software.nbdc-datahub.org/NBDCtools/reference/get_metadata.md)
  : Get metadata
- [`get_dd()`](https://software.nbdc-datahub.org/NBDCtools/reference/get_dd.md)
  [`get_dd_abcd()`](https://software.nbdc-datahub.org/NBDCtools/reference/get_dd.md)
  [`get_dd_hbcd()`](https://software.nbdc-datahub.org/NBDCtools/reference/get_dd.md)
  : Get data dictionary
- [`get_levels()`](https://software.nbdc-datahub.org/NBDCtools/reference/get_levels.md)
  [`get_levels_abcd()`](https://software.nbdc-datahub.org/NBDCtools/reference/get_levels.md)
  [`get_levels_hbcd()`](https://software.nbdc-datahub.org/NBDCtools/reference/get_levels.md)
  : Get levels table
- [`get_releases()`](https://software.nbdc-datahub.org/NBDCtools/reference/get_releases.md)
  [`get_releases_abcd()`](https://software.nbdc-datahub.org/NBDCtools/reference/get_releases.md)
  [`get_releases_hbcd()`](https://software.nbdc-datahub.org/NBDCtools/reference/get_releases.md)
  [`get_latest_release()`](https://software.nbdc-datahub.org/NBDCtools/reference/get_releases.md)
  [`get_latest_release_abcd()`](https://software.nbdc-datahub.org/NBDCtools/reference/get_releases.md)
  [`get_latest_release_hbcd()`](https://software.nbdc-datahub.org/NBDCtools/reference/get_releases.md)
  : Get release version number(s)
- [`get_sessions()`](https://software.nbdc-datahub.org/NBDCtools/reference/get_sessions.md)
  [`get_sessions_abcd()`](https://software.nbdc-datahub.org/NBDCtools/reference/get_sessions.md)
  [`get_sessions_hbcd()`](https://software.nbdc-datahub.org/NBDCtools/reference/get_sessions.md)
  : Get sessions table
- [`get_id_cols()`](https://software.nbdc-datahub.org/NBDCtools/reference/get_id_cols.md)
  [`get_id_cols_abcd()`](https://software.nbdc-datahub.org/NBDCtools/reference/get_id_cols.md)
  [`get_id_cols_hbcd()`](https://software.nbdc-datahub.org/NBDCtools/reference/get_id_cols.md)
  : Get identifier columns
- [`add_custom_metadata()`](https://software.nbdc-datahub.org/NBDCtools/reference/add_custom_metadata.md)
  : Add custom metadata

## Utility functions

Utility functions for working with data in the NBDC Data Hub.

- [`create_bids_sidecar_data()`](https://software.nbdc-datahub.org/NBDCtools/reference/create_bids_sidecar_data.md)
  : Create BIDS sidecar from labelled data
- [`create_bids_sidecar_metadata()`](https://software.nbdc-datahub.org/NBDCtools/reference/create_bids_sidecar_metadata.md)
  : Create BIDS sidecar from metadata
- [`convert_names_data()`](https://software.nbdc-datahub.org/NBDCtools/reference/convert_names_data.md)
  : Convert column names in a data frame
- [`convert_names_file()`](https://software.nbdc-datahub.org/NBDCtools/reference/convert_names_file.md)
  : Convert column names in a file
