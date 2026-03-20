# Transform data

In this vignette, we will demonstrate how to apply different
transformations to a joined dataset such as converting categorical
columns to factors, adding variable and value labels, converting
categorical values to labels, and more.

## Prepare data

> **NOTE:** Before running the code in this vignette, we assume that you
> have already read the [Get
> started](https://software.nbdc-datahub.org/NBDCtools/articles/NBDCtools.md)
> vignette or the [Join
> data](https://software.nbdc-datahub.org/NBDCtools/articles/join.md)
> vignette, and know how to load the data for a given NBDC study
> release.

To demonstrate the transformation functions, we load a simulated ABCD
dataset that is included with the package.

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

## Convert categorical columns to factors

From looking at the simulated data, we can see that categorical columns
like `ab_g_dyn__visit_type` are of type “character”. We can use
[`transf_factor()`](https://software.nbdc-datahub.org/NBDCtools/reference/transf_factor.md)
function to convert these columns to type “factor” which is the correct
type for categorical variables in R.

``` r
data_transf <- data |>
  transf_factor(study = "abcd")
dplyr::glimpse(data_transf)
#> Rows: 10
#> Columns: 10
#> $ participant_id                    <chr> "sub-0000000006", "sub-0000000007", …
#> $ session_id                        <fct> ses-04A, ses-05A, ses-02A, ses-01A, …
#> $ ab_g_dyn__visit_type              <fct> 3, 1, 1, 1, 3, 3, 2, 3, 2, 1
#> $ ab_g_dyn__cohort_grade            <ord> 9, 6, NA, 7, 8, NA, 8, 8, 8, NA
#> $ ab_g_dyn__visit__day1_dt          <date> 2020-10-19, 2022-05-19, 2022-09-12, …
#> $ ab_g_stc__gen_pc__01              <dbl> -0.022995395, 0.006506271, 0.0039703…
#> $ ab_g_dyn__visit_age               <dbl> 11.63836, 13.18630, 10.11475, 10.11…
#> $ ab_g_dyn__visit_days              <int> 1, 1, 2, 1, 1, 1, 2, 2, 1, 2
#> $ mr_y_qc__raw__dmri__r01__series_t <chr> NA, NA, NA, "11:05:35", "15:47:35", …
#> $ ab_g_dyn__visit_dtt               <dttm> 2022-05-19 13:01:00, 2019-02-17 09:2…
```

The
[`transf_factor()`](https://software.nbdc-datahub.org/NBDCtools/reference/transf_factor.md)
function automatically detects categorical columns and converts them to
ordered or unordered factors based on the specification in the data
dictionary and levels table for a given study.

## Apply variable and value labels

Next, we can add variable and value labels to the dataset.

``` r
data_transf <- data_transf |> 
  transf_label(study = "abcd")
```

To inspect the *variable labels*, we can use
[`sjlabelled::get_label()`](https://strengejacke.github.io/sjlabelled/reference/get_label.html)
function:

``` r
sjlabelled::get_label(data_transf)
#>                                                                    participant_id 
#>                                                          "Participant identifier" 
#>                                                                        session_id 
#>                                                                "Event identifier" 
#>                                                              ab_g_dyn__visit_type 
#>                    "Visit information: Type of event (In-person, remote, hybrid)" 
#>                                                            ab_g_dyn__cohort_grade 
#> "Cohort description: Current school grade [Cross-listed: ab_p_demo__ed__yth_001]" 
#>                                                          ab_g_dyn__visit__day1_dt 
#>                                           "Visit information (day 1): Visit date" 
#>                                                              ab_g_stc__gen_pc__01 
#>                         "Genetics: First principal component of genetic ancestry" 
#>                                                               ab_g_dyn__visit_age 
#>                        "Visit information: Youth's age at the start of the event" 
#>                                                              ab_g_dyn__visit_days 
#>                                         "Visit information: Number of visit days" 
#>                                                 mr_y_qc__raw__dmri__r01__series_t 
#>                                              "Diffusion MRI (run 1): Series time" 
#>                                                               ab_g_dyn__visit_dtt 
#>                      "Visit information: Date and time at the start of the event"
```

To inspect the *value labels*, we can use
[`sjlabelled::get_labels()`](https://strengejacke.github.io/sjlabelled/reference/get_labels.html)
function:

``` r
sjlabelled::get_labels(data_transf, attr.only = TRUE, values = "n")
#> $participant_id
#> NULL
#> 
#> $session_id
#>    ses-00S    ses-01A    ses-02A    ses-03A    ses-04A    ses-05A 
#> "Screener"   "1 Year"   "2 Year"   "3 Year"   "4 Year"   "5 Year" 
#> 
#> $ab_g_dyn__visit_type
#>         1         2         3 
#> "On-site"  "Remote"  "Hybrid" 
#> 
#> $ab_g_dyn__cohort_grade
#>                        0                        1                       10 
#>           "Kindergarten"              "1st grade"             "10th grade" 
#>                       11                       12                       13 
#>             "11th grade"             "12th grade"                "College" 
#>                       14                        2                        3 
#> "Not enrolled in school"              "2nd grade"              "3rd grade" 
#>                        4                        5                        6 
#>              "4th grade"              "5th grade"              "6th grade" 
#>                        7                        8                        9 
#>              "7th grade"              "8th grade"              "9th grade" 
#> 
#> $ab_g_dyn__visit__day1_dt
#> NULL
#> 
#> $ab_g_stc__gen_pc__01
#> NULL
#> 
#> $ab_g_dyn__visit_age
#> NULL
#> 
#> $ab_g_dyn__visit_days
#> NULL
#> 
#> $mr_y_qc__raw__dmri__r01__series_t
#> NULL
#> 
#> $ab_g_dyn__visit_dtt
#> NULL
```

If the labeling is done incorrectly, we can simply rerun the
[`transf_label()`](https://software.nbdc-datahub.org/NBDCtools/reference/transf_label.md)
function to fix it. If we want to remove all labels, we can use

``` r
data_labels_removed <- sjlabelled::remove_all_labels(data_transf) 
sjlabelled::get_label(data_labels_removed)
#>                    participant_id                        session_id 
#>                                ""                                "" 
#>              ab_g_dyn__visit_type            ab_g_dyn__cohort_grade 
#>                                ""                                "" 
#>          ab_g_dyn__visit__day1_dt              ab_g_stc__gen_pc__01 
#>                                ""                                "" 
#>               ab_g_dyn__visit_age              ab_g_dyn__visit_days 
#>                                ""                                "" 
#> mr_y_qc__raw__dmri__r01__series_t               ab_g_dyn__visit_dtt 
#>                                ""                                ""
sjlabelled::get_labels(data_labels_removed, attr.only = TRUE, values = "n")
#> $participant_id
#> NULL
#> 
#> $session_id
#> NULL
#> 
#> $ab_g_dyn__visit_type
#> NULL
#> 
#> $ab_g_dyn__cohort_grade
#> NULL
#> 
#> $ab_g_dyn__visit__day1_dt
#> NULL
#> 
#> $ab_g_stc__gen_pc__01
#> NULL
#> 
#> $ab_g_dyn__visit_age
#> NULL
#> 
#> $ab_g_dyn__visit_days
#> NULL
#> 
#> $mr_y_qc__raw__dmri__r01__series_t
#> NULL
#> 
#> $ab_g_dyn__visit_dtt
#> NULL
```

## Convert time columns to hms format

Time columns in the dataset (e.g. `mr_y_qc__raw__dmri__r01__series_t`)
are formatted as character strings `"HH:MM:SS"` by default. If we want
to convert these columns into
[`hms`](https://hms.tidyverse.org/reference/hms-package.html) format, we
can use the
[`transf_time_to_hms()`](https://software.nbdc-datahub.org/NBDCtools/reference/transf_time_to_hms.md)
function:

``` r
data_transf <- data_transf |> 
  transf_time_to_hms(study = "abcd")
dplyr::glimpse(data_transf)
#> Rows: 10
#> Columns: 10
#> $ participant_id                    <chr> "sub-0000000006", "sub-0000000007", …
#> $ session_id                        <fct> ses-04A, ses-05A, ses-02A, ses-01A, …
#> $ ab_g_dyn__visit_type              <fct> 3, 1, 1, 1, 3, 3, 2, 3, 2, 1
#> $ ab_g_dyn__cohort_grade            <ord> 9, 6, NA, 7, 8, NA, 8, 8, 8, NA
#> $ ab_g_dyn__visit__day1_dt          <date> 2020-10-19, 2022-05-19, 2022-09-12, …
#> $ ab_g_stc__gen_pc__01              <dbl> -0.022995395, 0.006506271, 0.0039703…
#> $ ab_g_dyn__visit_age               <dbl> 11.63836, 13.18630, 10.11475, 10.11…
#> $ ab_g_dyn__visit_days              <int> 1, 1, 2, 1, 1, 1, 2, 2, 1, 2
#> $ mr_y_qc__raw__dmri__r01__series_t <time>       NA,       NA,       NA, 11:05:…
#> $ ab_g_dyn__visit_dtt               <dttm> 2022-05-19 13:01:00, 2019-02-17 09:2…
```

As we can see, the column type is converted from `character` to `time`
(`hms`) class.

## Convert categorical column levels to labels

In some cases, such as for creating plots, it is useful to convert
categorical values to labels. We can use the
[`transf_value_to_label()`](https://software.nbdc-datahub.org/NBDCtools/reference/transf_value_to_label.md)
function to do so:

``` r
data_transf |> 
  transf_value_to_label()
#> # A tibble: 10 × 10
#>    participant_id session_id ab_g_dyn__visit_type ab_g_dyn__cohort_grade
#>    <chr>          <fct>      <fct>                <ord>                 
#>  1 sub-0000000006 ses-04A    Hybrid               9th grade             
#>  2 sub-0000000007 ses-05A    On-site              6th grade             
#>  3 sub-0000000001 ses-02A    On-site              NA                    
#>  4 sub-0000000003 ses-01A    On-site              7th grade             
#>  5 sub-0000000010 ses-04A    Hybrid               8th grade             
#>  6 sub-0000000008 ses-03A    Hybrid               NA                    
#>  7 sub-0000000004 ses-04A    Remote               8th grade             
#>  8 sub-0000000009 ses-00S    Hybrid               8th grade             
#>  9 sub-0000000005 ses-01A    Remote               8th grade             
#> 10 sub-0000000002 ses-03A    On-site              NA                    
#> # ℹ 6 more variables: ab_g_dyn__visit__day1_dt <date>,
#> #   ab_g_stc__gen_pc__01 <dbl>, ab_g_dyn__visit_age <dbl>,
#> #   ab_g_dyn__visit_days <int>, mr_y_qc__raw__dmri__r01__series_t <time>,
#> #   ab_g_dyn__visit_dtt <dttm>
```

> **NOTE:** Before running this function, make sure that the data has
> been transformed with the
> [`transf_factor()`](https://software.nbdc-datahub.org/NBDCtools/reference/transf_factor.md)
> and
> [`transf_label()`](https://software.nbdc-datahub.org/NBDCtools/reference/transf_label.md)
> functions, so that the variable and value labels are available.

## Convert missing codes to `NA`

In ABCD and HBCD Study datasets, some of the categorical columns use
specific codes to denote missingness/non-responses (e.g., `"999"` for
“Don’t know” or `"777"` for “Decline to answer”). If we want to remove
these values before analysis, we can use the
[`transf_value_to_na()`](https://software.nbdc-datahub.org/NBDCtools/reference/transf_value_to_na.md)
function to convert these codes to `NA`.

> **NOTE:** By default, this function converts all standard categorical
> missingness codes (`"222"` through `"999"`) to `NA`. In the ABCD
> Study, these codes are consistenly used throughout the whole dataset;
> in the HBCD Study, however, columns may use different codes for
> non-responses or missing values. Please refer to the data dictionary
> and levels table for the specific study to see which codes to convert.

``` r
data_transf |> 
  transf_value_to_na()
#> # A tibble: 10 × 10
#>    participant_id session_id ab_g_dyn__visit_type ab_g_dyn__cohort_grade
#>    <chr>          <fct>      <fct>                <ord>                 
#>  1 sub-0000000006 ses-04A    3                    9                     
#>  2 sub-0000000007 ses-05A    1                    6                     
#>  3 sub-0000000001 ses-02A    1                    NA                    
#>  4 sub-0000000003 ses-01A    1                    7                     
#>  5 sub-0000000010 ses-04A    3                    8                     
#>  6 sub-0000000008 ses-03A    3                    NA                    
#>  7 sub-0000000004 ses-04A    2                    8                     
#>  8 sub-0000000009 ses-00S    3                    8                     
#>  9 sub-0000000005 ses-01A    2                    8                     
#> 10 sub-0000000002 ses-03A    1                    NA                    
#> # ℹ 6 more variables: ab_g_dyn__visit__day1_dt <date>,
#> #   ab_g_stc__gen_pc__01 <dbl>, ab_g_dyn__visit_age <dbl>,
#> #   ab_g_dyn__visit_days <int>, mr_y_qc__raw__dmri__r01__series_t <time>,
#> #   ab_g_dyn__visit_dtt <dttm>
```

The simulated dataset does not contain categorical missingness codes.
However, this function provides a custom parameter `missing_codes` to
specify the codes that should be converted to `NA`. For example, if we
want to convert the categorical values `"1"` and `"2"` to `NA`, we can
use:

``` r
data_transf |> 
  transf_value_to_na(missing_codes = c("1", "2"))
#> # A tibble: 10 × 10
#>    participant_id session_id ab_g_dyn__visit_type ab_g_dyn__cohort_grade
#>    <chr>          <fct>      <fct>                <ord>                 
#>  1 sub-0000000006 ses-04A    3                    9                     
#>  2 sub-0000000007 ses-05A    NA                   6                     
#>  3 sub-0000000001 ses-02A    NA                   NA                    
#>  4 sub-0000000003 ses-01A    NA                   7                     
#>  5 sub-0000000010 ses-04A    3                    8                     
#>  6 sub-0000000008 ses-03A    3                    NA                    
#>  7 sub-0000000004 ses-04A    NA                   8                     
#>  8 sub-0000000009 ses-00S    3                    8                     
#>  9 sub-0000000005 ses-01A    NA                   8                     
#> 10 sub-0000000002 ses-03A    NA                   NA                    
#> # ℹ 6 more variables: ab_g_dyn__visit__day1_dt <date>,
#> #   ab_g_stc__gen_pc__01 <dbl>, ab_g_dyn__visit_age <dbl>,
#> #   ab_g_dyn__visit_days <int>, mr_y_qc__raw__dmri__r01__series_t <time>,
#> #   ab_g_dyn__visit_dtt <dttm>
```

In the `ab_g_dyn__visit_type` column, we can see that the values `"1"`
and `"2"` have been converted to `NA`.

The function has another parameter, `ignore_col_pattern`, that can be
used to ignore specific columns, so that they are exempt from the
conversion. This parameter accepts a regular expression pattern, meaning
all columns that match the pattern will be ignored. For example, we can
ignore all columns that start with `ab_g_dyn__visit` by using:

``` r
data_transf |> 
  transf_value_to_na(ignore_col_pattern = "^ab_g_dyn__visit")
#> # A tibble: 10 × 10
#>    participant_id session_id ab_g_dyn__visit_type ab_g_dyn__cohort_grade
#>    <chr>          <fct>      <fct>                <ord>                 
#>  1 sub-0000000006 ses-04A    3                    9                     
#>  2 sub-0000000007 ses-05A    1                    6                     
#>  3 sub-0000000001 ses-02A    1                    NA                    
#>  4 sub-0000000003 ses-01A    1                    7                     
#>  5 sub-0000000010 ses-04A    3                    8                     
#>  6 sub-0000000008 ses-03A    3                    NA                    
#>  7 sub-0000000004 ses-04A    2                    8                     
#>  8 sub-0000000009 ses-00S    3                    8                     
#>  9 sub-0000000005 ses-01A    2                    8                     
#> 10 sub-0000000002 ses-03A    1                    NA                    
#> # ℹ 6 more variables: ab_g_dyn__visit__day1_dt <date>,
#> #   ab_g_stc__gen_pc__01 <dbl>, ab_g_dyn__visit_age <dbl>,
#> #   ab_g_dyn__visit_days <int>, mr_y_qc__raw__dmri__r01__series_t <time>,
#> #   ab_g_dyn__visit_dtt <dttm>
```
