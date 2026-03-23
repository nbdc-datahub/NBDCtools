# Join data

In this vignette, we will demonstrate how to load/join selected
variables and/or tables from data files downloaded from the NBDC Data
Hub into a single data frame in memory.

``` r
library(NBDCtools)
#> Welcome to the `NBDCtools` package! For more information, visit: https://software.nbdc-datahub.org/NBDCtools/
#> This package is developed by the ABCD Data Analysis, Informatics & Resource Center (DAIRC) at the J. Craig Venter Institute (JCVI)
```

To download data from the ABCD or HBCD studies through the NBDC Data
Hub, users need to have a valid Data Use Certification (DUC) (see
[here](https://www.nbdc-datahub.org/data-access-process) for more
details). In this documentation, we will use simulated ABCD data files
to demonstrate the package’s functionality.

You can use the following command to inspect the simulated data files:

``` r
dir_abcd <- system.file("extdata", "phenotype", package = "NBDCtools")
list.files(dir_abcd)
#> [1] "ab_g_dyn.parquet"           "ab_g_stc.parquet"          
#> [3] "mr_y_qc__raw__dmri.parquet"
```

We can see that the `phenotype/` directory contains several files. Each
file contains the data from a database table in the ABCD study. For
example, the `ab_g_dyn.parquet` file contains variables from the [ABCD
Dynamic Variables
\[General\]](https://abcd.deapscience.com/?hierarchyOrder=%5B%22study%22%2C%22domain%22%2C%22subDomain%22%2C%22source%22%2C%22metric%22%2C%22atlas%22%5D&hierarchy=%5B%5B%22General%22%2C%22ABCD%20Dynamic%20Variables%20%5BGeneral%5D%22%5D%5D#/my-datasets/create-dataset)
table.

In a more realistic scenario, once you downloaded the ABCD data files to
a local directory, there will be a lot more files in the `phenotype/`
directory; it should contain one file per database table for a given
study. To see the tables available in the ABCD tabulated data resource,
you can use `NBDCtools`’
[`get_dd_abcd()`](https://software.nbdc-datahub.org/NBDCtools/reference/get_dd.md)
function:

``` r
dd_abcd <- get_dd_abcd()
dd_abcd |>
  dplyr::distinct(table_name, table_label)
#> # A tibble: 718 × 2
#>    table_name  table_label                                 
#>    <glue>      <glue>                                      
#>  1 ab_g_dyn    ABCD Dynamic Variables [General]            
#>  2 ab_g_stc    ABCD Static Variables [General]             
#>  3 ab_p_demo   Demographics [Parent]                       
#>  4 ab_p_ocp    Occupation Survey [Parent]                  
#>  5 ab_p_screen Screener (Study Eligibility) [Parent]       
#>  6 fc_p_aclt   Acculturation [Parent]                      
#>  7 fc_p_drv    Youth Driving [Parent]                      
#>  8 fc_p_fes    Family Environment Scale [Parent]           
#>  9 fc_p_hsf    Home Short Form [Parent]                    
#> 10 fc_p_meim   Multi-Group Ethnic Identity Measure [Parent]
#> # ℹ 708 more rows
```

## Join data

We can use the
[`join_tabulated()`](https://software.nbdc-datahub.org/NBDCtools/reference/join_tabulated.md)
function to load the data from the files in the `phenotype/` directory.
The function will read and join the data from different tables based on
the variables and/or tables specified in the these four arguments:

- `vars`: individual variables of interest
- `tables`: full tables of interest
- `vars_add`: additional individual variables
- `tables_add`: additional full tables

The `vars` and `tables` arguments are used to specify the main variables
and/or tables of interest. They will be extracted from the respective
files on disk and will be joined by the identifier columns into a single
data frame in memory. The used join operation is a
[`full_join()`](https://dplyr.tidyverse.org/reference/mutate-joins.html),
i.e., the resulting data frame retains all rows that have at least one
non-missing value in the chosen variables/tables.

The `vars_add` and `tables_add` arguments are used to specify additional
variables and/or tables to be joined with the main variables/tables of
interest. The used join operation for adding these variables is a
[`left_join()`](https://dplyr.tidyverse.org/reference/mutate-joins.html),
i.e., only values for already existing rows will be added and no new
rows will be created. This is useful for adding variables to the dataset
that are important for a given analysis but are not the main variables
of interest (e.g., design/nesting or demographic information). By
left-joining these variables, one avoids creating new rows that contain
only missing values for the main variables of interest selected using
`vars` and `tables`.

``` r
join_tabulated(
  dir_data = dir_abcd,
  study = "abcd",
  vars = c(
    "mr_y_qc__raw__dmri__r01__series_t",
    "ab_g_dyn__visit_type",
    "ab_g_dyn__cohort_grade", 
    "ab_g_dyn__visit__day1_dt",
    "ab_g_stc__gen_pc__01", 
    "ab_g_dyn__visit_age",
    "ab_g_dyn__visit_days", 
    "ab_g_dyn__visit_dtt"
  )
)
#> ℹ Using metadata "abcd" version "6.0" to join data
#> ✔ Using metadata "abcd" version "6.0" to join data [109ms]
#> 
#> ℹ Joining 8 variables from 3 tables...
#> ✔ Joining 8 variables from 3 tables... [307ms]
#> 
#> # A tibble: 10 × 10
#>    participant_id session_id mr_y_qc__raw__dmri__r01__ser…¹ ab_g_dyn__visit_type
#>    <chr>          <chr>      <chr>                          <chr>               
#>  1 sub-0000000001 ses-02A    NA                             1                   
#>  2 sub-0000000002 ses-03A    16:45:46                       1                   
#>  3 sub-0000000003 ses-01A    11:05:35                       1                   
#>  4 sub-0000000004 ses-04A    11:05:35                       2                   
#>  5 sub-0000000005 ses-01A    16:45:46                       2                   
#>  6 sub-0000000006 ses-04A    NA                             3                   
#>  7 sub-0000000007 ses-05A    NA                             1                   
#>  8 sub-0000000008 ses-03A    10:14:34                       3                   
#>  9 sub-0000000009 ses-00S    15:47:35                       3                   
#> 10 sub-0000000010 ses-04A    15:47:35                       3                   
#> # ℹ abbreviated name: ¹​mr_y_qc__raw__dmri__r01__series_t
#> # ℹ 6 more variables: ab_g_dyn__cohort_grade <chr>,
#> #   ab_g_dyn__visit__day1_dt <date>, ab_g_stc__gen_pc__01 <dbl>,
#> #   ab_g_dyn__visit_age <dbl>, ab_g_dyn__visit_days <int>,
#> #   ab_g_dyn__visit_dtt <dttm>
```

> **NOTE:** This is a simulated dataset, so the number of rows are
> significantly smaller than the real data.

***`remove_empty_rows` argument***

Joining selected columns from different tables using
[`join_tabulated()`](https://software.nbdc-datahub.org/NBDCtools/reference/join_tabulated.md)
may result in rows with only missing values. This can occur when some
participant/events lack data for the selected variables but have data
for other, unselected variables in the joined tables. Since rows without
any data are not useful for analysis, the
[`join_tabulated()`](https://software.nbdc-datahub.org/NBDCtools/reference/join_tabulated.md)
function includes the `remove_empty_rows` argument to eliminate these
rows from the resulting dataset. By default, this argument is set to
`TRUE`, meaning rows with all missing data will be removed. To keep
these rows, set the argument to `FALSE`.

### Shadow matrices

For HBCD data, the
[`join_tabulated()`](https://software.nbdc-datahub.org/NBDCtools/reference/join_tabulated.md)
function can also join shadow matrices, which store the reasons for
missing values. Shadow matrix files are located in the same directory as
the data files, with one `{table_name}_shadow.parquet` file accompanying
each `{table_name}.parquet` file. To load the shadow matrices, use the
`shadow = TRUE` argument in the
[`join_tabulated()`](https://software.nbdc-datahub.org/NBDCtools/reference/join_tabulated.md)
function. This will load selected columns or tables from the shadow
matrix files and combine them into a single table, just like the main
data. For more information about shadow matrices, see the [Work with
shadow
matrices](https://software.nbdc-datahub.org/NBDCtools/articles/shadow.md)
vignette.

``` r
join_tabulated(
  dir_data = dir_abcd,
  study = "hbcd",
  vars = ...,
  tables = ...,
  ...,
  shadow = TRUE
)
```
