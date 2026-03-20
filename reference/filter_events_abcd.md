# Filter ABCD events

Given a (set of) condition(s), filters the events included in an ABCD
dataset. Conditions can be specified as a vector of strings, where each
string can be one of the following conditions:

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

The conditions can be combined with logical `"and"` or `"or"`.

## Usage

``` r
filter_events_abcd(data, conditions, connect = "and")
```

## Arguments

- data:

  tibble. The data to be filtered.

- conditions:

  character (vector). The events to keep.

- connect:

  character. Whether to connect the conditions with `"and"` (an event is
  retained if *all* conditions are met; the default) or `"or"` (an event
  is retained if *any* condition is met).

## Value

A tibble with the filtered data.

## Examples

``` r
data <- tibble::tribble(
  ~session_id,     ~study,     ~type,
  "ses-00S",       "core",     "screener",
  "ses-00M",       "core",     "mid-year",
  "ses-00A",       "core",     "even",
  "ses-01M",       "core",     "mid-year",
  "ses-01A",       "core",     "odd",
  "ses-02M",       "core",     "mid-year",
  "ses-02A",       "core",     "even",
  "ses-03M",       "core",     "mid-year",
  "ses-03A",       "core",     "odd",
  "ses-04M",       "core",     "mid-year",
  "ses-04A",       "core",     "even",
  "ses-05M",       "core",     "mid-year",
  "ses-05A",       "core",     "odd",
  "ses-06M",       "core",     "mid-year",
  "ses-06A",       "core",     "even",
  "ses-C01",       "substudy", "covid",
  "ses-C02",       "substudy", "covid",
  "ses-C03",       "substudy", "covid",
  "ses-C04",       "substudy", "covid",
  "ses-C05",       "substudy", "covid",
  "ses-C06",       "substudy", "covid",
  "ses-C07",       "substudy", "covid",
  "ses-S01",       "substudy", "sdev",
  "ses-S02",       "substudy", "sdev",
  "ses-S03",       "substudy", "sdev",
  "ses-S04",       "substudy", "sdev",
  "ses-S05",       "substudy", "sdev"
)

# ABCD core study events
filter_events_abcd(data, c("core"))
#> # A tibble: 15 × 3
#>    session_id study type    
#>    <chr>      <chr> <chr>   
#>  1 ses-00S    core  screener
#>  2 ses-00M    core  mid-year
#>  3 ses-00A    core  even    
#>  4 ses-01M    core  mid-year
#>  5 ses-01A    core  odd     
#>  6 ses-02M    core  mid-year
#>  7 ses-02A    core  even    
#>  8 ses-03M    core  mid-year
#>  9 ses-03A    core  odd     
#> 10 ses-04M    core  mid-year
#> 11 ses-04A    core  even    
#> 12 ses-05M    core  mid-year
#> 13 ses-05A    core  odd     
#> 14 ses-06M    core  mid-year
#> 15 ses-06A    core  even    

# COVID substudy events
filter_events_abcd(data, c("covid"))
#> # A tibble: 7 × 3
#>   session_id study    type 
#>   <chr>      <chr>    <chr>
#> 1 ses-C01    substudy covid
#> 2 ses-C02    substudy covid
#> 3 ses-C03    substudy covid
#> 4 ses-C04    substudy covid
#> 5 ses-C05    substudy covid
#> 6 ses-C06    substudy covid
#> 7 ses-C07    substudy covid

# imaging events
filter_events_abcd(data, c("annual", "even"))
#> # A tibble: 4 × 3
#>   session_id study type 
#>   <chr>      <chr> <chr>
#> 1 ses-00A    core  even 
#> 2 ses-02A    core  even 
#> 3 ses-04A    core  even 
#> 4 ses-06A    core  even 

# mid-years before year 5
filter_events_abcd(data, c("mid_year", "<5"))
#> # A tibble: 5 × 3
#>   session_id study type    
#>   <chr>      <chr> <chr>   
#> 1 ses-00M    core  mid-year
#> 2 ses-01M    core  mid-year
#> 3 ses-02M    core  mid-year
#> 4 ses-03M    core  mid-year
#> 5 ses-04M    core  mid-year

# COVID or Social Development substudy events
filter_events_abcd(data, c("covid", "sdev"), connect = "or")
#> # A tibble: 12 × 3
#>    session_id study    type 
#>    <chr>      <chr>    <chr>
#>  1 ses-C01    substudy covid
#>  2 ses-C02    substudy covid
#>  3 ses-C03    substudy covid
#>  4 ses-C04    substudy covid
#>  5 ses-C05    substudy covid
#>  6 ses-C06    substudy covid
#>  7 ses-C07    substudy covid
#>  8 ses-S01    substudy sdev 
#>  9 ses-S02    substudy sdev 
#> 10 ses-S03    substudy sdev 
#> 11 ses-S04    substudy sdev 
#> 12 ses-S05    substudy sdev 
```
