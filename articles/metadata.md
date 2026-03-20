# Work with metadata

In this vignette, we will demonstrate how to retrieve and work with
metadata for NBDC studies, including data dictionaries, levels tables,
session/event information, and identifier columns.

## Overview

The `NBDCtools` package provides functions to access the following
metadata elements for ABCD and HBCD studies:

- **Data dictionary**: Variable and table definitions, data types, and
  other information.
- **Levels table**: Value, labels, and order for categorical variables.
- **Sessions table**: Information about study sessions/events.
- **Identifier columns**: Variables used to identify unique
  observations.

## Setup

``` r
library(NBDCtools)
#> Welcome to the `NBDCtools` package! For more information, visit: https://software.nbdc-datahub.org/NBDCtools/
#> This package is developed by the ABCD Data Analysis, Informatics & Resource Center (DAIRC) at the J. Craig Venter Institute (JCVI)
```

## Release information

Before working with metadata, it is important to know which data
releases are available for each study. You can use the
[`get_releases()`](https://software.nbdc-datahub.org/NBDCtools/reference/get_releases.md)
function to retrieve the list of available releases:

``` r
get_releases("abcd")
#> [1] "6.0" "6.1"
# study specific releases
get_releases_abcd()
#> [1] "6.0" "6.1"
get_releases_hbcd()
#> [1] "1.0" "1.1" "2.0"
```

We can also check what is the latest release for each study:

``` r
get_latest_release("abcd")
#> [1] "6.1"
# study specific latest release
get_latest_release_abcd()
#> [1] "6.1"
get_latest_release_hbcd()
#> [1] "2.0"
```

## Data dictionary

The data dictionary contains information about all variables in the
tabulated data for a given study, including their names, labels, data
types, which tables they belong to, etc. To read more about the
structure of the data dictionary for NBDC studies, see
[here](https://docs.abcdstudy.org/latest/documentation/curation/metadata.html#dd_levels_tables).

### Basic usage

``` r
# Get data dictionary for the ABCD Study (latest release)
get_dd("abcd")
#> # A tibble: 83,223 × 44
#>    study domain      sub_domain source metric atlas table_name table_label name 
#>    <chr> <chr>       <chr>      <chr>  <chr>  <chr> <chr>      <chr>       <chr>
#>  1 Core  ABCD (Gene… Standard … Gener… NA     NA    ab_g_dyn   ABCD Dynam… ab_g…
#>  2 Core  ABCD (Gene… Standard … Gener… NA     NA    ab_g_dyn   ABCD Dynam… ab_g…
#>  3 Core  ABCD (Gene… Standard … Gener… NA     NA    ab_g_dyn   ABCD Dynam… ab_g…
#>  4 Core  ABCD (Gene… Standard … Gener… NA     NA    ab_g_dyn   ABCD Dynam… ab_g…
#>  5 Core  ABCD (Gene… Standard … Gener… NA     NA    ab_g_dyn   ABCD Dynam… ab_g…
#>  6 Core  ABCD (Gene… Standard … Gener… NA     NA    ab_g_dyn   ABCD Dynam… ab_g…
#>  7 Core  ABCD (Gene… Standard … Gener… NA     NA    ab_g_dyn   ABCD Dynam… ab_g…
#>  8 Core  ABCD (Gene… Standard … Gener… NA     NA    ab_g_dyn   ABCD Dynam… ab_g…
#>  9 Core  ABCD (Gene… Standard … Gener… NA     NA    ab_g_dyn   ABCD Dynam… ab_g…
#> 10 Core  ABCD (Gene… Standard … Gener… NA     NA    ab_g_dyn   ABCD Dynam… ab_g…
#> # ℹ 83,213 more rows
#> # ℹ 35 more variables: label <chr>, instruction <chr>, header <chr>,
#> #   note <chr>, unit <chr>, type_var <chr>, type_data <chr>, type_level <chr>,
#> #   type_field <chr>, order_display <chr>, branching_logic <chr>,
#> #   label_es <chr>, instruction_es <chr>, header_es <chr>, note_es <chr>,
#> #   table_nda <chr>, table_nda_5_0 <chr>, table_redcap <chr>, name_nda <chr>,
#> #   name_deap <chr>, name_redcap <chr>, name_redcap_exp <chr>, …

# Get data dictionary for the HBCD Study (latest release)
get_dd("hbcd")
#> # A tibble: 96,227 × 33
#>    study domain      sub_domain source metric atlas table_name table_label name 
#>    <chr> <chr>       <chr>      <chr>  <chr>  <chr> <chr>      <chr>       <chr>
#>  1 Core  BioSpecime… NA         Biolo… NA     NA    bio_bm_bi… USDTL Nail… bio_…
#>  2 Core  BioSpecime… NA         Biolo… NA     NA    bio_bm_bi… USDTL Nail… bio_…
#>  3 Core  BioSpecime… NA         Biolo… NA     NA    bio_bm_bi… USDTL Nail… bio_…
#>  4 Core  BioSpecime… NA         Biolo… NA     NA    bio_bm_bi… USDTL Nail… bio_…
#>  5 Core  BioSpecime… NA         Biolo… NA     NA    bio_bm_bi… USDTL Nail… bio_…
#>  6 Core  BioSpecime… NA         Biolo… NA     NA    bio_bm_bi… USDTL Nail… bio_…
#>  7 Core  BioSpecime… NA         Biolo… NA     NA    bio_bm_bi… USDTL Nail… bio_…
#>  8 Core  BioSpecime… NA         Biolo… NA     NA    bio_bm_bi… USDTL Nail… bio_…
#>  9 Core  BioSpecime… NA         Biolo… NA     NA    bio_bm_bi… USDTL Nail… bio_…
#> 10 Core  BioSpecime… NA         Biolo… NA     NA    bio_bm_bi… USDTL Nail… bio_…
#> # ℹ 96,217 more rows
#> # ℹ 24 more variables: label <chr>, instruction <chr>, header <chr>,
#> #   note <chr>, unit <chr>, type_var <chr>, type_data <chr>, type_level <chr>,
#> #   type_field <chr>, order_display <chr>, branching_logic <chr>,
#> #   label_es <chr>, instruction_es <chr>, header_es <chr>, note_es <chr>,
#> #   name_short <chr>, name_stata <chr>, url_table <chr>, url_warn_use <lgl>,
#> #   url_warn_data <chr>, url_table_warn_use <chr>, url_table_warn_data <chr>, …

# Get data dictionary for a specific release
get_dd("abcd", release = "6.0")
#> # A tibble: 83,206 × 44
#>    study domain      sub_domain source metric atlas table_name table_label name 
#>    <chr> <chr>       <chr>      <chr>  <chr>  <chr> <chr>      <chr>       <chr>
#>  1 Core  ABCD (Gene… Standard … Gener… NA     NA    ab_g_dyn   ABCD Dynam… ab_g…
#>  2 Core  ABCD (Gene… Standard … Gener… NA     NA    ab_g_dyn   ABCD Dynam… ab_g…
#>  3 Core  ABCD (Gene… Standard … Gener… NA     NA    ab_g_dyn   ABCD Dynam… ab_g…
#>  4 Core  ABCD (Gene… Standard … Gener… NA     NA    ab_g_dyn   ABCD Dynam… ab_g…
#>  5 Core  ABCD (Gene… Standard … Gener… NA     NA    ab_g_dyn   ABCD Dynam… ab_g…
#>  6 Core  ABCD (Gene… Standard … Gener… NA     NA    ab_g_dyn   ABCD Dynam… ab_g…
#>  7 Core  ABCD (Gene… Standard … Gener… NA     NA    ab_g_dyn   ABCD Dynam… ab_g…
#>  8 Core  ABCD (Gene… Standard … Gener… NA     NA    ab_g_dyn   ABCD Dynam… ab_g…
#>  9 Core  ABCD (Gene… Standard … Gener… NA     NA    ab_g_dyn   ABCD Dynam… ab_g…
#> 10 Core  ABCD (Gene… Standard … Gener… NA     NA    ab_g_dyn   ABCD Dynam… ab_g…
#> # ℹ 83,196 more rows
#> # ℹ 35 more variables: label <chr>, instruction <chr>, header <chr>,
#> #   note <chr>, unit <chr>, type_var <chr>, type_data <chr>, type_level <chr>,
#> #   type_field <chr>, order_display <chr>, branching_logic <chr>,
#> #   label_es <chr>, instruction_es <chr>, header_es <chr>, note_es <chr>,
#> #   table_nda <chr>, table_nda_5_0 <chr>, table_redcap <chr>, name_nda <chr>,
#> #   name_deap <chr>, name_redcap <chr>, name_redcap_exp <chr>, …

# If you are not sure what releases are available, just use a random number
# and the function will return an error message presenting the available
# releases
get_dd("abcd", release = "999.0")
#> Error in `get_metadata()`:
#> ! Invalid release '999.0'. Valid releases are: 6.0, 6.1
#> If you believe this version should exist, your metadata might be outdated.
#> Please update the `NBDCtoolsData` package to get the latest metadata.
```

### Study-specific functions

For convenience, you can use study-specific functions that do not
require specifying the study parameter:

``` r
# ABCD-specific function
get_dd_abcd()
#> # A tibble: 83,223 × 44
#>    study domain      sub_domain source metric atlas table_name table_label name 
#>    <chr> <chr>       <chr>      <chr>  <chr>  <chr> <chr>      <chr>       <chr>
#>  1 Core  ABCD (Gene… Standard … Gener… NA     NA    ab_g_dyn   ABCD Dynam… ab_g…
#>  2 Core  ABCD (Gene… Standard … Gener… NA     NA    ab_g_dyn   ABCD Dynam… ab_g…
#>  3 Core  ABCD (Gene… Standard … Gener… NA     NA    ab_g_dyn   ABCD Dynam… ab_g…
#>  4 Core  ABCD (Gene… Standard … Gener… NA     NA    ab_g_dyn   ABCD Dynam… ab_g…
#>  5 Core  ABCD (Gene… Standard … Gener… NA     NA    ab_g_dyn   ABCD Dynam… ab_g…
#>  6 Core  ABCD (Gene… Standard … Gener… NA     NA    ab_g_dyn   ABCD Dynam… ab_g…
#>  7 Core  ABCD (Gene… Standard … Gener… NA     NA    ab_g_dyn   ABCD Dynam… ab_g…
#>  8 Core  ABCD (Gene… Standard … Gener… NA     NA    ab_g_dyn   ABCD Dynam… ab_g…
#>  9 Core  ABCD (Gene… Standard … Gener… NA     NA    ab_g_dyn   ABCD Dynam… ab_g…
#> 10 Core  ABCD (Gene… Standard … Gener… NA     NA    ab_g_dyn   ABCD Dynam… ab_g…
#> # ℹ 83,213 more rows
#> # ℹ 35 more variables: label <chr>, instruction <chr>, header <chr>,
#> #   note <chr>, unit <chr>, type_var <chr>, type_data <chr>, type_level <chr>,
#> #   type_field <chr>, order_display <chr>, branching_logic <chr>,
#> #   label_es <chr>, instruction_es <chr>, header_es <chr>, note_es <chr>,
#> #   table_nda <chr>, table_nda_5_0 <chr>, table_redcap <chr>, name_nda <chr>,
#> #   name_deap <chr>, name_redcap <chr>, name_redcap_exp <chr>, …

# HBCD-specific function  
get_dd_hbcd(release = "1.0")
#> # A tibble: 48,699 × 30
#>    study domain     source table_name table_label name  label instruction header
#>    <chr> <chr>      <chr>  <chr>      <chr>       <chr> <chr> <chr>       <chr> 
#>  1 Core  BioSpecim… Biolo… bio_bm_bi… USDTL Nail… bio_… Spec… NA          NA    
#>  2 Core  BioSpecim… Biolo… bio_bm_bi… USDTL Nail… bio_… Nail… NA          NA    
#>  3 Core  BioSpecim… Biolo… bio_bm_bi… USDTL Nail… bio_… Numb… NA          NA    
#>  4 Core  BioSpecim… Biolo… bio_bm_bi… USDTL Nail… bio_… Spec… NA          NA    
#>  5 Core  BioSpecim… Biolo… bio_bm_bi… USDTL Nail… bio_… Any … NA          NA    
#>  6 Core  BioSpecim… Biolo… bio_bm_bi… USDTL Nail… bio_… Scre… NA          NA    
#>  7 Core  BioSpecim… Biolo… bio_bm_bi… USDTL Nail… bio_… Scre… NA          NA    
#>  8 Core  BioSpecim… Biolo… bio_bm_bi… USDTL Nail… bio_… Conf… NA          NA    
#>  9 Core  BioSpecim… Biolo… bio_bm_bi… USDTL Nail… bio_… Conf… NA          NA    
#> 10 Core  BioSpecim… Biolo… bio_bm_bi… USDTL Nail… bio_… Conf… NA          NA    
#> # ℹ 48,689 more rows
#> # ℹ 21 more variables: note <chr>, unit <chr>, type_var <chr>, type_data <chr>,
#> #   type_level <chr>, type_field <chr>, order_display <chr>,
#> #   branching_logic <chr>, label_es <chr>, instruction_es <chr>,
#> #   header_es <chr>, note_es <chr>, name_short <chr>, name_stata <chr>,
#> #   url_table <chr>, url_warn_use <chr>, url_warn_data <chr>,
#> #   url_table_warn_use <chr>, url_table_warn_data <chr>, …
```

### Filtering by variables

You can retrieve a subset of the data dictionary for specific variables:

``` r
# Get data dictionary for specific variables
vars_of_interest <- c(
  "ab_g_dyn__visit_dtt", 
  "ab_g_dyn__visit_age", 
  "ab_g_stc__design_id__fam"
)
get_dd("abcd", vars = vars_of_interest)
#> # A tibble: 3 × 44
#>   study domain sub_domain source metric atlas table_name table_label name  label
#>   <chr> <chr>  <chr>      <chr>  <chr>  <chr> <chr>      <chr>       <chr> <chr>
#> 1 Core  ABCD … Standard … Gener… NA     NA    ab_g_dyn   ABCD Dynam… ab_g… Visi…
#> 2 Core  ABCD … Standard … Gener… NA     NA    ab_g_dyn   ABCD Dynam… ab_g… Visi…
#> 3 Core  ABCD … Standard … Gener… NA     NA    ab_g_stc   ABCD Stati… ab_g… Desi…
#> # ℹ 34 more variables: instruction <chr>, header <chr>, note <chr>, unit <chr>,
#> #   type_var <chr>, type_data <chr>, type_level <chr>, type_field <chr>,
#> #   order_display <chr>, branching_logic <chr>, label_es <chr>,
#> #   instruction_es <chr>, header_es <chr>, note_es <chr>, table_nda <chr>,
#> #   table_nda_5_0 <chr>, table_redcap <chr>, name_nda <chr>, name_deap <chr>,
#> #   name_redcap <chr>, name_redcap_exp <chr>, url_table <chr>,
#> #   url_warn_use <chr>, url_warn_data <chr>, url_table_warn_use <chr>, …
```

### Filtering by tables

You can also retrieve a subset of the data dictionary for specific
tables:

``` r
# Get data dictionary for specific tables
tables_of_interest <- c(
  "ab_g_dyn",
  "ab_g_stc"
)
get_dd_abcd(tables = tables_of_interest)
#> # A tibble: 73 × 44
#>    study domain      sub_domain source metric atlas table_name table_label name 
#>    <chr> <chr>       <chr>      <chr>  <chr>  <chr> <chr>      <chr>       <chr>
#>  1 Core  ABCD (Gene… Standard … Gener… NA     NA    ab_g_dyn   ABCD Dynam… ab_g…
#>  2 Core  ABCD (Gene… Standard … Gener… NA     NA    ab_g_dyn   ABCD Dynam… ab_g…
#>  3 Core  ABCD (Gene… Standard … Gener… NA     NA    ab_g_dyn   ABCD Dynam… ab_g…
#>  4 Core  ABCD (Gene… Standard … Gener… NA     NA    ab_g_dyn   ABCD Dynam… ab_g…
#>  5 Core  ABCD (Gene… Standard … Gener… NA     NA    ab_g_dyn   ABCD Dynam… ab_g…
#>  6 Core  ABCD (Gene… Standard … Gener… NA     NA    ab_g_dyn   ABCD Dynam… ab_g…
#>  7 Core  ABCD (Gene… Standard … Gener… NA     NA    ab_g_dyn   ABCD Dynam… ab_g…
#>  8 Core  ABCD (Gene… Standard … Gener… NA     NA    ab_g_dyn   ABCD Dynam… ab_g…
#>  9 Core  ABCD (Gene… Standard … Gener… NA     NA    ab_g_dyn   ABCD Dynam… ab_g…
#> 10 Core  ABCD (Gene… Standard … Gener… NA     NA    ab_g_dyn   ABCD Dynam… ab_g…
#> # ℹ 63 more rows
#> # ℹ 35 more variables: label <chr>, instruction <chr>, header <chr>,
#> #   note <chr>, unit <chr>, type_var <chr>, type_data <chr>, type_level <chr>,
#> #   type_field <chr>, order_display <chr>, branching_logic <chr>,
#> #   label_es <chr>, instruction_es <chr>, header_es <chr>, note_es <chr>,
#> #   table_nda <chr>, table_nda_5_0 <chr>, table_redcap <chr>, name_nda <chr>,
#> #   name_deap <chr>, name_redcap <chr>, name_redcap_exp <chr>, …
```

## Levels table

The levels table provides value labels for categorical variables,
showing what each numeric value in the data corresponds to, as well as
the order of levels. To read more about the structure of the levels
table for NBDC studies, see
[here](https://docs.abcdstudy.org/latest/documentation/curation/metadata.html#dd_levels_tables).

### Basic usage

``` r
# Get levels table for ABCD Study (latest release)
get_levels("abcd")
#> # A tibble: 63,498 × 5
#>    name                      value order_level label                    label_es
#>    <chr>                     <chr> <chr>       <chr>                    <chr>   
#>  1 ab_g_dyn__cohort_edu__cgs 1     1           Up to high school (No d… NA      
#>  2 ab_g_dyn__cohort_edu__cgs 2     2           High school diploma/GED  NA      
#>  3 ab_g_dyn__cohort_edu__cgs 3     3           Some college             NA      
#>  4 ab_g_dyn__cohort_edu__cgs 4     4           Bachelor’s degree        NA      
#>  5 ab_g_dyn__cohort_edu__cgs 5     5           Graduate school or prof… NA      
#>  6 ab_g_dyn__cohort_grade    0     1           Kindergarten             NA      
#>  7 ab_g_dyn__cohort_grade    1     2           1st grade                NA      
#>  8 ab_g_dyn__cohort_grade    2     3           2nd grade                NA      
#>  9 ab_g_dyn__cohort_grade    3     4           3rd grade                NA      
#> 10 ab_g_dyn__cohort_grade    4     5           4th grade                NA      
#> # ℹ 63,488 more rows

# Get levels table for HBCD Study (latest release)
get_levels("hbcd")
#> # A tibble: 90,666 × 5
#>    name                                         value order_level label label_es
#>    <chr>                                        <chr>       <int> <chr> <lgl>   
#>  1 bio_bm_biosample_nails_results_Nail_type     1               1 "\"F… NA      
#>  2 bio_bm_biosample_nails_results_Nail_type     2               2 "\"T… NA      
#>  3 bio_bm_biosample_nails_results_Nail_type     3               3 "\"B… NA      
#>  4 bio_bm_biosample_nails_results_Nail_type     4               4 "\"U… NA      
#>  5 bio_bm_biosample_nails_results_test_ordered… 1               1 "\"C… NA      
#>  6 bio_bm_biosample_nails_results_test_ordered… 2               2 "\"O… NA      
#>  7 bio_bm_biosample_nails_results_test_ordered… 3               3 "\"C… NA      
#>  8 bio_bm_biosample_nails_results_c_any_specim… 1               1 "\"P… NA      
#>  9 bio_bm_biosample_nails_results_c_any_specim… 0               2 "\"N… NA      
#> 10 bio_bm_biosample_nails_results_c_any_specim… 3               3 "\"Q… NA      
#> # ℹ 90,656 more rows

# Get levels table for a specific release
get_levels("abcd", release = "6.0")
#> # A tibble: 63,502 × 5
#>    name                      value order_level label                    label_es
#>    <chr>                     <chr>       <int> <chr>                    <chr>   
#>  1 ab_g_dyn__cohort_edu__cgs 1               1 Up to high school (No d… NA      
#>  2 ab_g_dyn__cohort_edu__cgs 2               2 High school diploma/GED  NA      
#>  3 ab_g_dyn__cohort_edu__cgs 3               3 Some college             NA      
#>  4 ab_g_dyn__cohort_edu__cgs 4               4 Bachelor’s degree        NA      
#>  5 ab_g_dyn__cohort_edu__cgs 5               5 Graduate school or prof… NA      
#>  6 ab_g_dyn__cohort_grade    0               1 Kindergarten             NA      
#>  7 ab_g_dyn__cohort_grade    1               2 1st grade                NA      
#>  8 ab_g_dyn__cohort_grade    2               3 2nd grade                NA      
#>  9 ab_g_dyn__cohort_grade    3               4 3rd grade                NA      
#> 10 ab_g_dyn__cohort_grade    4               5 4th grade                NA      
#> # ℹ 63,492 more rows
```

### Study-specific functions

``` r
# ABCD-specific function
get_levels_abcd(release = "6.0")
#> # A tibble: 63,502 × 5
#>    name                      value order_level label                    label_es
#>    <chr>                     <chr>       <int> <chr>                    <chr>   
#>  1 ab_g_dyn__cohort_edu__cgs 1               1 Up to high school (No d… NA      
#>  2 ab_g_dyn__cohort_edu__cgs 2               2 High school diploma/GED  NA      
#>  3 ab_g_dyn__cohort_edu__cgs 3               3 Some college             NA      
#>  4 ab_g_dyn__cohort_edu__cgs 4               4 Bachelor’s degree        NA      
#>  5 ab_g_dyn__cohort_edu__cgs 5               5 Graduate school or prof… NA      
#>  6 ab_g_dyn__cohort_grade    0               1 Kindergarten             NA      
#>  7 ab_g_dyn__cohort_grade    1               2 1st grade                NA      
#>  8 ab_g_dyn__cohort_grade    2               3 2nd grade                NA      
#>  9 ab_g_dyn__cohort_grade    3               4 3rd grade                NA      
#> 10 ab_g_dyn__cohort_grade    4               5 4th grade                NA      
#> # ℹ 63,492 more rows

# HBCD-specific function
get_levels_hbcd()
#> # A tibble: 90,666 × 5
#>    name                                         value order_level label label_es
#>    <chr>                                        <chr>       <int> <chr> <lgl>   
#>  1 bio_bm_biosample_nails_results_Nail_type     1               1 "\"F… NA      
#>  2 bio_bm_biosample_nails_results_Nail_type     2               2 "\"T… NA      
#>  3 bio_bm_biosample_nails_results_Nail_type     3               3 "\"B… NA      
#>  4 bio_bm_biosample_nails_results_Nail_type     4               4 "\"U… NA      
#>  5 bio_bm_biosample_nails_results_test_ordered… 1               1 "\"C… NA      
#>  6 bio_bm_biosample_nails_results_test_ordered… 2               2 "\"O… NA      
#>  7 bio_bm_biosample_nails_results_test_ordered… 3               3 "\"C… NA      
#>  8 bio_bm_biosample_nails_results_c_any_specim… 1               1 "\"P… NA      
#>  9 bio_bm_biosample_nails_results_c_any_specim… 0               2 "\"N… NA      
#> 10 bio_bm_biosample_nails_results_c_any_specim… 3               3 "\"Q… NA      
#> # ℹ 90,656 more rows
```

### Filtering by variables and/or tables

As for the data dictionary, you can also retrieve a subset of the levels
table for specific variables or tables:

``` r
# Get levels for specific categorical variables
get_levels("abcd", vars = c("ab_g_dyn__visit_type"))
#> # A tibble: 3 × 5
#>   name                 value order_level label   label_es
#>   <chr>                <chr> <chr>       <chr>   <chr>   
#> 1 ab_g_dyn__visit_type 1     1           On-site NA      
#> 2 ab_g_dyn__visit_type 2     2           Remote  NA      
#> 3 ab_g_dyn__visit_type 3     3           Hybrid  NA

# Get levels for all categorical variables in specific tables
get_levels("abcd", tables = "ab_g_dyn")
#> # A tibble: 123 × 5
#>    name                         value order_level label             label_es
#>    <chr>                        <chr> <chr>       <chr>             <chr>   
#>  1 ab_g_dyn__visit_type         1     1           On-site           NA      
#>  2 ab_g_dyn__visit_type         2     2           Remote            NA      
#>  3 ab_g_dyn__visit_type         3     3           Hybrid            NA      
#>  4 ab_g_dyn__visit__day1_inform 1     1           Biological mother NA      
#>  5 ab_g_dyn__visit__day1_inform 2     2           Biological father NA      
#>  6 ab_g_dyn__visit__day1_inform 3     3           Adoptive mother   NA      
#>  7 ab_g_dyn__visit__day1_inform 4     4           Adoptive father   NA      
#>  8 ab_g_dyn__visit__day1_inform 5     5           Custodial mother  NA      
#>  9 ab_g_dyn__visit__day1_inform 6     6           Custodial father  NA      
#> 10 ab_g_dyn__visit__day1_inform 7     7           Grandmother       NA      
#> # ℹ 113 more rows

# Get levels for a combination of specific variables and tables
get_levels_abcd(vars = "ab_g_dyn__visit_type", tables = "ab_g_stc")
#> # A tibble: 54 × 5
#>    name                    value order_level label    label_es
#>    <chr>                   <chr> <chr>       <chr>    <chr>   
#>  1 ab_g_dyn__visit_type    1     1           On-site  NA      
#>  2 ab_g_dyn__visit_type    2     2           Remote   NA      
#>  3 ab_g_dyn__visit_type    3     3           Hybrid   NA      
#>  4 ab_g_stc__design_famrel 0     1           Single   NA      
#>  5 ab_g_stc__design_famrel 1     2           Sibling  NA      
#>  6 ab_g_stc__design_famrel 2     3           Twin     NA      
#>  7 ab_g_stc__design_famrel 3     4           Triplet  NA      
#>  8 ab_g_stc__design_sstwin 0     1           No       NA      
#>  9 ab_g_stc__design_sstwin 1     2           Yes      NA      
#> 10 ab_g_stc__cohort_ethn   1     1           Hispanic NA      
#> # ℹ 44 more rows
```

## Sessions table

The sessions table contains information about the events/session IDs
that are part of a given release as well as their labels.

### Basic usage

``` r
# Get sessions information for ABCD Study (latest release)
get_sessions("abcd")
#> # A tibble: 27 × 4
#>    session_id label    order     n
#>    <chr>      <chr>    <dbl> <dbl>
#>  1 ses-00S    Screener     1 11867
#>  2 ses-00A    Baseline     2 11868
#>  3 ses-00M    0.5 Year     3 11388
#>  4 ses-01A    1 Year       4 11219
#>  5 ses-01M    1.5 Year     5 11082
#>  6 ses-02A    2 Year       6 10973
#>  7 ses-02M    2.5 Year     7 10253
#>  8 ses-03A    3 Year       8 10450
#>  9 ses-03M    3.5 Year     9  9573
#> 10 ses-04A    4 Year      10  9739
#> # ℹ 17 more rows

# Get sessions information for HBCD Study (latest release)
get_sessions("hbcd")
#> # A tibble: 5 × 2
#>   session_id label  
#>   <chr>      <chr>  
#> 1 ses-V01    Visit 1
#> 2 ses-V02    Visit 2
#> 3 ses-V03    Visit 3
#> 4 ses-V04    Visit 4
#> 5 ses-V05    Visit 5
```

### Study-specific functions

``` r
# ABCD-specific function (for a specified release)
get_sessions_abcd(release = "6.0")
#> # A tibble: 26 × 2
#>    session_id label   
#>    <fct>      <fct>   
#>  1 ses-00S    Screener
#>  2 ses-00A    Baseline
#>  3 ses-00M    0.5 Year
#>  4 ses-01A    1 Year  
#>  5 ses-01M    1.5 Year
#>  6 ses-02A    2 Year  
#>  7 ses-02M    2.5 Year
#>  8 ses-03A    3 Year  
#>  9 ses-03M    3.5 Year
#> 10 ses-04A    4 Year  
#> # ℹ 16 more rows

# HBCD-specific function (for a specified release)
get_sessions_hbcd(release = "1.0")
#> # A tibble: 3 × 2
#>   session_id label  
#>   <chr>      <chr>  
#> 1 ses-V01    Visit 1
#> 2 ses-V02    Visit 2
#> 3 ses-V03    Visit 3
```

## Identifier columns

Identifier columns are the variables used to uniquely identify
observations in the dataset. These columns are essential for joining
data from different tables. The
[`get_id_cols()`](https://software.nbdc-datahub.org/NBDCtools/reference/get_id_cols.md)
function retrieves the identifier columns for a given study.

### Basic usage

``` r
# Get identifier columns for ABCD Study (latest release)
get_id_cols("abcd")
#> [1] "participant_id" "session_id"

# Get identifier columns for HBCD Study (latest release)
get_id_cols("hbcd")
#> [1] "participant_id" "session_id"     "run_id"
```

### Study-specific functions

``` r
# ABCD-specific function (for a specified release)
get_id_cols_abcd(release = "6.0")
#> [1] "participant_id" "session_id"

# HBCD-specific function (for a specified release)
get_id_cols_hbcd(release = "1.0")
#> [1] "participant_id" "session_id"     "run_id"
```

## General metadata function

The
[`get_metadata()`](https://software.nbdc-datahub.org/NBDCtools/reference/get_metadata.md)
function is the low-level function that is used by all specific metadata
functions. You can use it directly to retrieve any type of metadata:

``` r
# Get data dictionary (same as get_dd)
get_metadata("abcd", type = "dd", release = "6.0")
#> # A tibble: 83,206 × 44
#>    study domain      sub_domain source metric atlas table_name table_label name 
#>    <chr> <chr>       <chr>      <chr>  <chr>  <chr> <chr>      <chr>       <chr>
#>  1 Core  ABCD (Gene… Standard … Gener… NA     NA    ab_g_dyn   ABCD Dynam… ab_g…
#>  2 Core  ABCD (Gene… Standard … Gener… NA     NA    ab_g_dyn   ABCD Dynam… ab_g…
#>  3 Core  ABCD (Gene… Standard … Gener… NA     NA    ab_g_dyn   ABCD Dynam… ab_g…
#>  4 Core  ABCD (Gene… Standard … Gener… NA     NA    ab_g_dyn   ABCD Dynam… ab_g…
#>  5 Core  ABCD (Gene… Standard … Gener… NA     NA    ab_g_dyn   ABCD Dynam… ab_g…
#>  6 Core  ABCD (Gene… Standard … Gener… NA     NA    ab_g_dyn   ABCD Dynam… ab_g…
#>  7 Core  ABCD (Gene… Standard … Gener… NA     NA    ab_g_dyn   ABCD Dynam… ab_g…
#>  8 Core  ABCD (Gene… Standard … Gener… NA     NA    ab_g_dyn   ABCD Dynam… ab_g…
#>  9 Core  ABCD (Gene… Standard … Gener… NA     NA    ab_g_dyn   ABCD Dynam… ab_g…
#> 10 Core  ABCD (Gene… Standard … Gener… NA     NA    ab_g_dyn   ABCD Dynam… ab_g…
#> # ℹ 83,196 more rows
#> # ℹ 35 more variables: label <chr>, instruction <chr>, header <chr>,
#> #   note <chr>, unit <chr>, type_var <chr>, type_data <chr>, type_level <chr>,
#> #   type_field <chr>, order_display <chr>, branching_logic <chr>,
#> #   label_es <chr>, instruction_es <chr>, header_es <chr>, note_es <chr>,
#> #   table_nda <chr>, table_nda_5_0 <chr>, table_redcap <chr>, name_nda <chr>,
#> #   name_deap <chr>, name_redcap <chr>, name_redcap_exp <chr>, …

# Get levels table (same as get_levels)  
get_metadata("abcd", type = "levels", vars = "ab_g_dyn__visit_type")
#> # A tibble: 3 × 5
#>   name                 value order_level label   label_es
#>   <chr>                <chr> <chr>       <chr>   <chr>   
#> 1 ab_g_dyn__visit_type 1     1           On-site NA      
#> 2 ab_g_dyn__visit_type 2     2           Remote  NA      
#> 3 ab_g_dyn__visit_type 3     3           Hybrid  NA

# Get sessions table (same as get_sessions)
get_metadata("abcd", type = "sessions")
#> # A tibble: 27 × 4
#>    session_id label    order     n
#>    <chr>      <chr>    <dbl> <dbl>
#>  1 ses-00S    Screener     1 11867
#>  2 ses-00A    Baseline     2 11868
#>  3 ses-00M    0.5 Year     3 11388
#>  4 ses-01A    1 Year       4 11219
#>  5 ses-01M    1.5 Year     5 11082
#>  6 ses-02A    2 Year       6 10973
#>  7 ses-02M    2.5 Year     7 10253
#>  8 ses-03A    3 Year       8 10450
#>  9 ses-03M    3.5 Year     9  9573
#> 10 ses-04A    4 Year      10  9739
#> # ℹ 17 more rows
```
