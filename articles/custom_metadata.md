# Custom metadata

In this vignette, we will demonstrate some advanced use cases of adding
custom metadata to the NBDCtools enviornment, and use custom metadata in
various functions.

## Prepare data

> **NOTE:** Before running the code in this vignette, we assume that you
> have already read the [Get
> started](https://software.nbdc-datahub.org/NBDCtools/articles/NBDCtools.md)
> vignette or the [Join
> data](https://software.nbdc-datahub.org/NBDCtools/articles/join.md)
> vignette, and know how to load the data for a given NBDC study
> release.

To load `NBDCtools`, use the following command:

``` r
library(NBDCtools)
#> Welcome to the `NBDCtools` package! For more information, visit: https://software.nbdc-datahub.org/NBDCtools/
#> This package is developed by the ABCD Data Analysis, Informatics & Resource Center (DAIRC) at the J. Craig Venter Institute (JCVI)
suppressPackageStartupMessages({
  library(dplyr)
})
```

To demonstrate the custom metadata utilities, we load some trimmed ABCD
metadata files and simulated ABCD files that are included with the
package.

metadata files:

``` r
# assign the metadata objects to global and we will use later
purrr::walk(
  c("lst_dds", "lst_levels", "lst_sessions"),
  ~ {
    value <- readRDS(system.file(
      "extdata", "meta_internal", glue::glue("{.x}.rds"),
      package = "NBDCtools"
    ))[[1]][[1]]
    assign(gsub("lst_", "", .x), value = value, envir = sys.frame(which = 0))
  }
)
dds[1:5, 1:5]
#> # A tibble: 5 × 5
#>   study domain         sub_domain         source  metric
#>   <chr> <chr>          <chr>              <chr>   <chr> 
#> 1 Core  ABCD (General) Standard Variables General NA    
#> 2 Core  ABCD (General) Standard Variables General NA    
#> 3 Core  ABCD (General) Standard Variables General NA    
#> 4 Core  ABCD (General) Standard Variables General NA    
#> 5 Core  ABCD (General) Standard Variables General NA
levels[1:5, ]
#> # A tibble: 5 × 5
#>   name                      value order_level label                     label_es
#>   <glue>                    <chr>       <dbl> <chr>                     <chr>   
#> 1 ab_g_dyn__cohort_edu__cgs 1               1 Up to high school (No di… NA      
#> 2 ab_g_dyn__cohort_edu__cgs 2               2 High school diploma/GED   NA      
#> 3 ab_g_dyn__cohort_edu__cgs 3               3 Some college              NA      
#> 4 ab_g_dyn__cohort_edu__cgs 4               4 Bachelor’s degree         NA      
#> 5 ab_g_dyn__cohort_edu__cgs 5               5 Graduate school or profe… NA
sessions[1:5, ]
#> # A tibble: 5 × 2
#>   session_id label   
#>   <fct>      <fct>   
#> 1 ses-00S    Screener
#> 2 ses-00A    Baseline
#> 3 ses-00M    0.5 Year
#> 4 ses-01A    1 Year  
#> 5 ses-01M    1.5 Year
```

Simulated data files:

``` r
dir_abcd <- system.file("extdata", "phenotype", package = "NBDCtools")
list.files(dir_abcd)
#> [1] "ab_g_dyn.parquet"           "ab_g_stc.parquet"          
#> [3] "mr_y_qc__raw__dmri.parquet"
```

## Add custom metadata

For users who have their own metadata, they can add the metadata to the
NBDCtools by using the
[`add_custom_metadata()`](https://software.nbdc-datahub.org/NBDCtools/reference/add_custom_metadata.md)
function. This function will add the metadata to the NBDCtools
environment, and make it available for use in various functions. The
metadata can be used on `dd`, `levels`, and `sessions` tables, and
should have the same format as the original metadata tables. Users are
not expected to add all three tables, they can choose to add only one or
two tables, and leave the other tables empty, and the function will only
update the tables that are provided.

Now we can add previously loaded metadata to the NBDCtools environment:

``` r
add_custom_metadata(
  dd = dds,
  levels = levels,
  sessions = sessions
)
#> ℹ Added custom data dictionary to NBDCtools
#> ℹ Added custom levels table to NBDCtools
#> ℹ Added custom sessions table to NBDCtools
```

## get custom metadata

To test if the custom metadata is added successfully, we can use the
`get_<x>` function to retrieve the metadata from the NBDCtools
environment. Please read
[Metadata](https://software.nbdc-datahub.org/NBDCtools/articles/metadata.md)
vignette for more details on how to use the `get_<x>` functions.

The key of using custom metadata is to specify the **release** to be
`"custom"`.

``` r
get_dd_abcd(release = "custom")[1:5, 1:5]
#> # A tibble: 5 × 5
#>   study domain         sub_domain         source  metric
#>   <chr> <chr>          <chr>              <chr>   <chr> 
#> 1 Core  ABCD (General) Standard Variables General NA    
#> 2 Core  ABCD (General) Standard Variables General NA    
#> 3 Core  ABCD (General) Standard Variables General NA    
#> 4 Core  ABCD (General) Standard Variables General NA    
#> 5 Core  ABCD (General) Standard Variables General NA
```

When using the custom metadata, `study` argument is still needed but it
can be either `"abcd"` or `"hbcd"`.

``` r
get_dd_hbcd(release = "custom")[1:5, 1:5]
#> # A tibble: 5 × 5
#>   study domain         sub_domain         source  metric
#>   <chr> <chr>          <chr>              <chr>   <chr> 
#> 1 Core  ABCD (General) Standard Variables General NA    
#> 2 Core  ABCD (General) Standard Variables General NA    
#> 3 Core  ABCD (General) Standard Variables General NA    
#> 4 Core  ABCD (General) Standard Variables General NA    
#> 5 Core  ABCD (General) Standard Variables General NA
```

We can see they give the same `dd` table.

## Use custom metadata in data preparation

The `release = "custom"` can be used in most functions that accept
`release` argument. Custom metadata can be very useful when preparing
the data, especially for internal use when the pre-release metadata is
created but not in the `NBDCtoolsData` package yet.

``` r
mydata <- create_dataset(
  study = "abcd",
  dir = dir_abcd,
  release = "custom",
  vars = c("mr_y_qc__raw__dmri__r01__series_t")
)
#> ℹ Loading the data from the "/home/runner/.cache/R/renv/library/NBDCtools-2ca88…
#> ℹ Using metadata "abcd" version "custom" to join data
#> ℹ Custom release specified. Skipping data and metadata version check.
#> ✔ Using metadata "abcd" version "custom" to join data [70ms]
#> 
#> 
#> 
#> ℹ Loading the data from the "/home/runner/.cache/R/renv/library/NBDCtools-2ca88…
#> ℹ Joining 1 variable from 1 table...
#> 
#> ✔ Joining 1 variable from 1 table... [328ms]
#> 
#> 
#> 
#> ℹ Loading the data from the "/home/runner/.cache/R/renv/library/NBDCtools-2ca88…
#> ✔ Loading the data from the "/home/runner/.cache/R/renv/library/NBDCtools-2ca88…
#> 
#> 
#> 
#> ℹ Converting categorical variables to factors.
#> 
#> ✔ Converting categorical variables to factors. [118ms]
#> 
#> 
#> 
#> ℹ Adding variable and value labels.
#> 
#> ✔ Adding variable and value labels. [135ms]
#> 
#> 
#> 
#> ✔ A dataset with 7 rows and 3 columns has been created. Time used: 0.01
#>   minutes.
mydata
#> # A tibble: 7 × 3
#>   participant_id session_id mr_y_qc__raw__dmri__r01__series_t
#>   <chr>          <fct>      <chr>                            
#> 1 sub-0000000002 ses-03A    16:45:46                         
#> 2 sub-0000000003 ses-01A    11:05:35                         
#> 3 sub-0000000004 ses-04A    11:05:35                         
#> 4 sub-0000000005 ses-01A    16:45:46                         
#> 5 sub-0000000008 ses-03A    10:14:34                         
#> 6 sub-0000000009 ses-00S    15:47:35                         
#> 7 sub-0000000010 ses-04A    15:47:35
```

## Use case

One use case could be when one uses the release metadata,
e.g. `get_get_dd_abcd()`, and finds some errors in the metadata, they
can fix the errors and add the fixed metadata as custom metadata, and
use the custom metadata to prepare the data.

For example, the label of the dataset we just created for
`mr_y_qc__raw__dmri__r01__series_t` is
`Diffusion MRI (run 1): Series time`.

``` r
attr(mydata$mr_y_qc__raw__dmri__r01__series_t, "label")
#> Diffusion MRI (run 1): Series time
```

It is stored in the `dd`

``` r
get_dd_abcd(release = "custom") |> 
  filter(name == "mr_y_qc__raw__dmri__r01__series_t") |>
  select(name, label)
#> # A tibble: 1 × 2
#>   name                              label                             
#>   <glue>                            <glue>                            
#> 1 mr_y_qc__raw__dmri__r01__series_t Diffusion MRI (run 1): Series time
```

If we find the label is not correct, we can fix it in the `dd` table,
and add the fixed `dd` as custom metadata, and use the custom metadata
to create the dataset again.

``` r
# fix the label in the dd
dd_fixed <- dds |>
  mutate(
    label = if_else(
      name == "mr_y_qc__raw__dmri__r01__series_t",
      "my custom label",
      label
    )
  )
# add the fixed dd as custom metadata
# we can leave levels and sessions empty so they will not be changed
add_custom_metadata(dd = dd_fixed)
#> ℹ Added custom data dictionary to NBDCtools
# create the dataset again
mydata_fixed <- create_dataset(
  study = "abcd",
  dir = dir_abcd,
  release = "custom",
  vars = c("mr_y_qc__raw__dmri__r01__series_t")
)
#> ℹ Loading the data from the "/home/runner/.cache/R/renv/library/NBDCtools-2ca88…
#> ℹ Using metadata "abcd" version "custom" to join data
#> ℹ Custom release specified. Skipping data and metadata version check.✔ Using metadata "abcd" version "custom" to join data [27ms]
#> 
#> ℹ Loading the data from the "/home/runner/.cache/R/renv/library/NBDCtools-2ca88…ℹ Joining 1 variable from 1 table...
#> ✔ Joining 1 variable from 1 table... [127ms]
#> 
#> ℹ Loading the data from the "/home/runner/.cache/R/renv/library/NBDCtools-2ca88…✔ Loading the data from the "/home/runner/.cache/R/renv/library/NBDCtools-2ca88…
#> 
#> ℹ Converting categorical variables to factors.
#> ✔ Converting categorical variables to factors. [115ms]
#> 
#> ℹ Adding variable and value labels.
#> ✔ Adding variable and value labels. [125ms]
#> 
#> ✔ A dataset with 7 rows and 3 columns has been created. Time used: 0.01
#>   minutes.
attr(mydata_fixed$mr_y_qc__raw__dmri__r01__series_t, "label")
#> my custom label
```
