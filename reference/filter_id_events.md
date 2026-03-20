# Filter ID/events

Given a vector of ID/events (concatenated like
`"{participant_id}_{session_id}"`), or a dataframe with `participant_id`
and `session_id` columns, this function filters the data to keep or
alternatively remove the rows for the given *ID/events*.

## Usage

``` r
filter_id_events(data, id_events, revert = FALSE)
```

## Arguments

- data:

  tibble. The data to be filtered.

- id_events:

  character (vector) or dataframe. *(Vector of) ID/event(s)* *or a
  dataframe with `participant_id` and `session_id` columns*.

- revert:

  logical. Whether to revert the filter, i.e., to keep only rows NOT
  matching the `id_events` (Default: `FALSE`, i.e., keep only the rows
  matching the `id_events`).

## Value

A tibble with the filtered data.

## Examples

``` r
data <- tibble::tribble(
  ~participant_id, ~session_id,
  "sub-001",       "ses-001",
  "sub-001",       "ses-002",
  "sub-002",       "ses-001",
  "sub-002",       "ses-002",
  "sub-003",       "ses-001",
  "sub-003",       "ses-002"
)

# filter using a vector of ID/events
filter_id_events(
  data,
  id_events = c("sub-001_ses-001", "sub-003_ses-002")
)
#> # A tibble: 2 × 2
#>   participant_id session_id
#>   <chr>          <chr>     
#> 1 sub-001        ses-001   
#> 2 sub-003        ses-002   

# filter using a dataframe with participant_id and session_id
data_filter <- tibble::tibble(
  participant_id = c("sub-001", "sub-003"),
  session_id = c("ses-001", "ses-002")
)
filter_id_events(
  data,
  id_events = data_filter
)
#> # A tibble: 2 × 2
#>   participant_id session_id
#>   <chr>          <chr>     
#> 1 sub-001        ses-001   
#> 2 sub-003        ses-002   

# revert filter
filter_id_events(
  data,
  id_events = c("sub-001_ses-001", "sub-003_ses-002"),
  revert = TRUE
)
#> # A tibble: 4 × 2
#>   participant_id session_id
#>   <chr>          <chr>     
#> 1 sub-001        ses-002   
#> 2 sub-002        ses-001   
#> 3 sub-002        ses-002   
#> 4 sub-003        ses-001   
```
