# Filter data

In this vignette, we will demonstrate how to filter a loaded dataset
using different conditions, such as by participant ID & session ID,
event types, excluding empty rows/columns, or combination of these.

## Prepare data

> **NOTE:** Before running the code in this vignette, we assume that you
> have already read the [Get
> started](https://software.nbdc-datahub.org/NBDCtools/articles/NBDCtools.md)
> page or the [Join
> data](https://software.nbdc-datahub.org/NBDCtools/articles/join.md)
> vignette, and know how to load data for a given NBDC study release.

For demonstration purposes, we load a simulated ABCD dataset that is
included with the package.

``` r
library(NBDCtools)
#> Welcome to the `NBDCtools` package! For more information, visit: https://software.nbdc-datahub.org/NBDCtools/
#> This package is developed by the ABCD Data Analysis, Informatics & Resource Center (DAIRC) at the J. Craig Venter Institute (JCVI)

data <- readRDS(
  system.file("extdata", "simulated_data_abcd.rds", package = "NBDCtools")
)

dplyr::glimpse(data)
#> Rows: 10
#> Columns: 10
#> $ participant_id                    <chr> "sub-0000000006", "sub-0000000007", …
#> $ session_id                        <chr> "ses-04A", "ses-05A", "ses-02A", "se…
#> $ ab_g_dyn__visit_type              <chr> "3", "1", "1", "1", "3", "3", "2", "…
#> $ ab_g_dyn__cohort_grade            <chr> "9", "6", NA, "7", "8", NA, "8", "8"…
#> $ ab_g_dyn__visit__day1_dt          <date> 2020-10-19, 2022-05-19, 2022-09-12,…
#> $ ab_g_stc__gen_pc__01              <dbl> -0.022995395, 0.006506271, 0.0039703…
#> $ ab_g_dyn__visit_age               <dbl> 11.63836, 13.18630, 10.11475, 10.114…
#> $ ab_g_dyn__visit_days              <int> 1, 1, 2, 1, 1, 1, 2, 2, 1, 2
#> $ mr_y_qc__raw__dmri__r01__series_t <chr> NA, NA, NA, "11:05:35", "15:47:35", …
#> $ ab_g_dyn__visit_dtt               <dttm> 2022-05-19 13:01:00, 2019-02-17 09:2…
```

## Filter by participant ID and session ID

The
[`filter_id_events()`](https://software.nbdc-datahub.org/NBDCtools/reference/filter_id_events.md)
function accepts a vector of concatenated participant and session IDs,
separated by an underscore `_`, or a data frame with `participant_id`
and `session_id` columns. The function will filter the data by the
specified participant/events and return a data frame with the filtered
data.

``` r
# filter using a vector of concatenated participant and session IDs
vec_id_events = c(
  "sub-0000000006_ses-04A", 
  "sub-0000000007_ses-05A",
  "sub-0000000002_ses-03A"
)
data |> 
  filter_id_events(vec_id_events)
#> # A tibble: 3 × 10
#>   participant_id session_id ab_g_dyn__visit_type ab_g_dyn__cohort_grade
#>   <chr>          <chr>      <chr>                <chr>                 
#> 1 sub-0000000006 ses-04A    3                    9                     
#> 2 sub-0000000007 ses-05A    1                    6                     
#> 3 sub-0000000002 ses-03A    1                    NA                    
#> # ℹ 6 more variables: ab_g_dyn__visit__day1_dt <date>,
#> #   ab_g_stc__gen_pc__01 <dbl>, ab_g_dyn__visit_age <dbl>,
#> #   ab_g_dyn__visit_days <int>, mr_y_qc__raw__dmri__r01__series_t <chr>,
#> #   ab_g_dyn__visit_dtt <dttm>

# filter using a data frame with participant_id and session_id columns
data_filter <- dplyr::tribble(
  ~participant_id,  ~session_id,
  "sub-0000000006", "ses-04A", 
  "sub-0000000007", "ses-05A", 
  "sub-0000000002", "ses-03A"
)
data |>
  filter_id_events(data_filter)
#> # A tibble: 3 × 10
#>   participant_id session_id ab_g_dyn__visit_type ab_g_dyn__cohort_grade
#>   <chr>          <chr>      <chr>                <chr>                 
#> 1 sub-0000000006 ses-04A    3                    9                     
#> 2 sub-0000000007 ses-05A    1                    6                     
#> 3 sub-0000000002 ses-03A    1                    NA                    
#> # ℹ 6 more variables: ab_g_dyn__visit__day1_dt <date>,
#> #   ab_g_stc__gen_pc__01 <dbl>, ab_g_dyn__visit_age <dbl>,
#> #   ab_g_dyn__visit_days <int>, mr_y_qc__raw__dmri__r01__series_t <chr>,
#> #   ab_g_dyn__visit_dtt <dttm>
```

Alternatively, if we only want to keep the rows that *do not match* the
specified participant and session IDs, we can set the `revert = TRUE`
argument.

``` r
data |> 
  filter_id_events(vec_id_events, revert = TRUE)
#> # A tibble: 7 × 10
#>   participant_id session_id ab_g_dyn__visit_type ab_g_dyn__cohort_grade
#>   <chr>          <chr>      <chr>                <chr>                 
#> 1 sub-0000000001 ses-02A    1                    NA                    
#> 2 sub-0000000003 ses-01A    1                    7                     
#> 3 sub-0000000010 ses-04A    3                    8                     
#> 4 sub-0000000008 ses-03A    3                    NA                    
#> 5 sub-0000000004 ses-04A    2                    8                     
#> 6 sub-0000000009 ses-00S    3                    8                     
#> 7 sub-0000000005 ses-01A    2                    8                     
#> # ℹ 6 more variables: ab_g_dyn__visit__day1_dt <date>,
#> #   ab_g_stc__gen_pc__01 <dbl>, ab_g_dyn__visit_age <dbl>,
#> #   ab_g_dyn__visit_days <int>, mr_y_qc__raw__dmri__r01__series_t <chr>,
#> #   ab_g_dyn__visit_dtt <dttm>
```

## Filter ABCD events

The
[`filter_events_abcd()`](https://software.nbdc-datahub.org/NBDCtools/reference/filter_events_abcd.md)
function provides a convenient way to specify event-based filters for
ABCD datasets using shorthands. These shorthands can be a single
condition, such as `"annual"`, `"substudy"`, or `"odd"`, or a
combination of conditions provided as a vector of strings, like
`c("annual", "even")`.

The following conditions are available:

- `"core"`: events for the ABCD core study
- `"annual"`: annual events for the ABCD core study
- `"mid_year"`: mid-year events for the ABCD core study
- `"substudy"`: events for ABCD substudies
- `"covid"`: events for the COVID substudy
- `"sdev"`: events for the Social Development substudy
- `"even"`: even-numbered events
- `"odd"`: odd-numbered events
- numerical expressions like `>2` or `<=5` to filter events by number
- any other string to be used as filter for the `session_id` column

``` r
# retain only annual events
data |> 
  filter_events_abcd("annual")
#> # A tibble: 9 × 10
#>   participant_id session_id ab_g_dyn__visit_type ab_g_dyn__cohort_grade
#>   <chr>          <chr>      <chr>                <chr>                 
#> 1 sub-0000000006 ses-04A    3                    9                     
#> 2 sub-0000000007 ses-05A    1                    6                     
#> 3 sub-0000000001 ses-02A    1                    NA                    
#> 4 sub-0000000003 ses-01A    1                    7                     
#> 5 sub-0000000010 ses-04A    3                    8                     
#> 6 sub-0000000008 ses-03A    3                    NA                    
#> 7 sub-0000000004 ses-04A    2                    8                     
#> 8 sub-0000000005 ses-01A    2                    8                     
#> 9 sub-0000000002 ses-03A    1                    NA                    
#> # ℹ 6 more variables: ab_g_dyn__visit__day1_dt <date>,
#> #   ab_g_stc__gen_pc__01 <dbl>, ab_g_dyn__visit_age <dbl>,
#> #   ab_g_dyn__visit_days <int>, mr_y_qc__raw__dmri__r01__series_t <chr>,
#> #   ab_g_dyn__visit_dtt <dttm>

# retain only even annual events ("imaging events")
data |> 
  filter_events_abcd(c("annual", "even"))
#> # A tibble: 4 × 10
#>   participant_id session_id ab_g_dyn__visit_type ab_g_dyn__cohort_grade
#>   <chr>          <chr>      <chr>                <chr>                 
#> 1 sub-0000000006 ses-04A    3                    9                     
#> 2 sub-0000000001 ses-02A    1                    NA                    
#> 3 sub-0000000010 ses-04A    3                    8                     
#> 4 sub-0000000004 ses-04A    2                    8                     
#> # ℹ 6 more variables: ab_g_dyn__visit__day1_dt <date>,
#> #   ab_g_stc__gen_pc__01 <dbl>, ab_g_dyn__visit_age <dbl>,
#> #   ab_g_dyn__visit_days <int>, mr_y_qc__raw__dmri__r01__series_t <chr>,
#> #   ab_g_dyn__visit_dtt <dttm>

# retain only odd annual events before the 4-year follow-up
data |> 
  filter_events_abcd(c("annual", "odd", "<4"))
#> # A tibble: 4 × 10
#>   participant_id session_id ab_g_dyn__visit_type ab_g_dyn__cohort_grade
#>   <chr>          <chr>      <chr>                <chr>                 
#> 1 sub-0000000003 ses-01A    1                    7                     
#> 2 sub-0000000008 ses-03A    3                    NA                    
#> 3 sub-0000000005 ses-01A    2                    8                     
#> 4 sub-0000000002 ses-03A    1                    NA                    
#> # ℹ 6 more variables: ab_g_dyn__visit__day1_dt <date>,
#> #   ab_g_stc__gen_pc__01 <dbl>, ab_g_dyn__visit_age <dbl>,
#> #   ab_g_dyn__visit_days <int>, mr_y_qc__raw__dmri__r01__series_t <chr>,
#> #   ab_g_dyn__visit_dtt <dttm>

# retain only screener events (using a string condition to filter events)
data |> 
  filter_events_abcd(c("00S"))
#> # A tibble: 1 × 10
#>   participant_id session_id ab_g_dyn__visit_type ab_g_dyn__cohort_grade
#>   <chr>          <chr>      <chr>                <chr>                 
#> 1 sub-0000000009 ses-00S    3                    8                     
#> # ℹ 6 more variables: ab_g_dyn__visit__day1_dt <date>,
#> #   ab_g_stc__gen_pc__01 <dbl>, ab_g_dyn__visit_age <dbl>,
#> #   ab_g_dyn__visit_days <int>, mr_y_qc__raw__dmri__r01__series_t <chr>,
#> #   ab_g_dyn__visit_dtt <dttm>
```

## Filter empty rows

When combining data from different tables, we can end up with rows that
do not have values for any of the variables (besides the identifier
columns). We typically do not want to keep those rows as they don’t
contain any useful information. The package provides a function to
remove such
rows—[`filter_empty_rows()`](https://software.nbdc-datahub.org/NBDCtools/reference/filter_empty_rows.md).

The simulated dataset does not have empty rows. To demonstrate the
function, we can manually set some rows to `NA` values and apply the
[`filter_empty_rows()`](https://software.nbdc-datahub.org/NBDCtools/reference/filter_empty_rows.md)
function afterwards.

``` r
data_empty <- data[1:5, ] |>
  dplyr::mutate(dplyr::across(-c(participant_id, session_id), ~ NA)) |> 
  dplyr::bind_rows(data[-(1:5), ])
data_empty
#> # A tibble: 10 × 10
#>    participant_id session_id ab_g_dyn__visit_type ab_g_dyn__cohort_grade
#>    <chr>          <chr>      <chr>                <chr>                 
#>  1 sub-0000000006 ses-04A    NA                   NA                    
#>  2 sub-0000000007 ses-05A    NA                   NA                    
#>  3 sub-0000000001 ses-02A    NA                   NA                    
#>  4 sub-0000000003 ses-01A    NA                   NA                    
#>  5 sub-0000000010 ses-04A    NA                   NA                    
#>  6 sub-0000000008 ses-03A    3                    NA                    
#>  7 sub-0000000004 ses-04A    2                    8                     
#>  8 sub-0000000009 ses-00S    3                    8                     
#>  9 sub-0000000005 ses-01A    2                    8                     
#> 10 sub-0000000002 ses-03A    1                    NA                    
#> # ℹ 6 more variables: ab_g_dyn__visit__day1_dt <date>,
#> #   ab_g_stc__gen_pc__01 <dbl>, ab_g_dyn__visit_age <dbl>,
#> #   ab_g_dyn__visit_days <int>, mr_y_qc__raw__dmri__r01__series_t <chr>,
#> #   ab_g_dyn__visit_dtt <dttm>

# filter empty rows
data_empty |> 
  filter_empty_rows()
#> # A tibble: 5 × 10
#>   participant_id session_id ab_g_dyn__visit_type ab_g_dyn__cohort_grade
#>   <chr>          <chr>      <chr>                <chr>                 
#> 1 sub-0000000008 ses-03A    3                    NA                    
#> 2 sub-0000000004 ses-04A    2                    8                     
#> 3 sub-0000000009 ses-00S    3                    8                     
#> 4 sub-0000000005 ses-01A    2                    8                     
#> 5 sub-0000000002 ses-03A    1                    NA                    
#> # ℹ 6 more variables: ab_g_dyn__visit__day1_dt <date>,
#> #   ab_g_stc__gen_pc__01 <dbl>, ab_g_dyn__visit_age <dbl>,
#> #   ab_g_dyn__visit_days <int>, mr_y_qc__raw__dmri__r01__series_t <chr>,
#> #   ab_g_dyn__visit_dtt <dttm>
```

As you can see, the first 5 rows that had missing values for all
variables were removed.

> **NOTE:** The
> [`filter_empty_rows()`](https://software.nbdc-datahub.org/NBDCtools/reference/filter_empty_rows.md)
> function is automatically called by the
> [`join_tabulated()`](https://software.nbdc-datahub.org/NBDCtools/reference/join_tabulated.md)
> or
> [`create_dataset()`](https://software.nbdc-datahub.org/NBDCtools/reference/create_dataset.md)
> functions when joining data from NBDC study datasets, so you do not
> need to call it explicitly. If you want to keep the empty rows, you
> can set the `remove_empty_rows = FALSE` argument in these two
> functions.

## Filter empty columns

Similarly, datasets can also contain completely empty columns. We can
remove them from the data using the
[`filter_empty_cols()`](https://software.nbdc-datahub.org/NBDCtools/reference/filter_empty_cols.md)
function.

Here, we modify the simulated data to create two empty columns and
execute
[`filter_empty_cols()`](https://software.nbdc-datahub.org/NBDCtools/reference/filter_empty_cols.md)
to remove them.

``` r
data_empty_cols <- data |> 
  dplyr::mutate(
    ab_g_dyn__visit_type = NA,
    ab_g_dyn__cohort_grade = NA
  ) 
data_empty_cols
#> # A tibble: 10 × 10
#>    participant_id session_id ab_g_dyn__visit_type ab_g_dyn__cohort_grade
#>    <chr>          <chr>      <lgl>                <lgl>                 
#>  1 sub-0000000006 ses-04A    NA                   NA                    
#>  2 sub-0000000007 ses-05A    NA                   NA                    
#>  3 sub-0000000001 ses-02A    NA                   NA                    
#>  4 sub-0000000003 ses-01A    NA                   NA                    
#>  5 sub-0000000010 ses-04A    NA                   NA                    
#>  6 sub-0000000008 ses-03A    NA                   NA                    
#>  7 sub-0000000004 ses-04A    NA                   NA                    
#>  8 sub-0000000009 ses-00S    NA                   NA                    
#>  9 sub-0000000005 ses-01A    NA                   NA                    
#> 10 sub-0000000002 ses-03A    NA                   NA                    
#> # ℹ 6 more variables: ab_g_dyn__visit__day1_dt <date>,
#> #   ab_g_stc__gen_pc__01 <dbl>, ab_g_dyn__visit_age <dbl>,
#> #   ab_g_dyn__visit_days <int>, mr_y_qc__raw__dmri__r01__series_t <chr>,
#> #   ab_g_dyn__visit_dtt <dttm>

# filter empty columns
data_empty_cols |> 
  filter_empty_cols()
#> # A tibble: 10 × 8
#>    participant_id session_id ab_g_dyn__visit__day1_dt ab_g_stc__gen_pc__01
#>    <chr>          <chr>      <date>                                  <dbl>
#>  1 sub-0000000006 ses-04A    2020-10-19                           -0.0230 
#>  2 sub-0000000007 ses-05A    2022-05-19                            0.00651
#>  3 sub-0000000001 ses-02A    2022-09-12                            0.00397
#>  4 sub-0000000003 ses-01A    2021-06-09                            0.00360
#>  5 sub-0000000010 ses-04A    2021-06-11                            0.00397
#>  6 sub-0000000008 ses-03A    2022-09-12                            0.00575
#>  7 sub-0000000004 ses-04A    2019-07-08                            0.00575
#>  8 sub-0000000009 ses-00S    2021-05-12                           -0.0230 
#>  9 sub-0000000005 ses-01A    2021-05-12                            0.00651
#> 10 sub-0000000002 ses-03A    2020-10-19                            0.00660
#> # ℹ 4 more variables: ab_g_dyn__visit_age <dbl>, ab_g_dyn__visit_days <int>,
#> #   mr_y_qc__raw__dmri__r01__series_t <chr>, ab_g_dyn__visit_dtt <dttm>
```

## Combining filters

We can combine several of the above filters by chaining the functions
using the pipe operator. For example, we can first exclude certain
participant/events, then filter by events, and finally remove empty rows
using the following code:

``` r
data_empty |> 
  filter_id_events(
    id_events = c("sub-0000000002_ses-03A"),
    revert = TRUE
  ) |>
  filter_events_abcd("odd") |>
  filter_empty_rows()
#> # A tibble: 2 × 10
#>   participant_id session_id ab_g_dyn__visit_type ab_g_dyn__cohort_grade
#>   <chr>          <chr>      <chr>                <chr>                 
#> 1 sub-0000000008 ses-03A    3                    NA                    
#> 2 sub-0000000005 ses-01A    2                    8                     
#> # ℹ 6 more variables: ab_g_dyn__visit__day1_dt <date>,
#> #   ab_g_stc__gen_pc__01 <dbl>, ab_g_dyn__visit_age <dbl>,
#> #   ab_g_dyn__visit_days <int>, mr_y_qc__raw__dmri__r01__series_t <chr>,
#> #   ab_g_dyn__visit_dtt <dttm>
```
