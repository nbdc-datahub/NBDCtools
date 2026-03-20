# Get levels table

Retrieves levels table for a given study and release version. Allows for
filtering by variables and tables. Wrapper around
[`get_metadata()`](https://software.nbdc-datahub.org/NBDCtools/reference/get_metadata.md).

In addition to the main `get_levels()` function, there are two
study-specific variations:

- `get_levels_abcd()`: for the ABCD study.

- `get_levels_hbcd()`: for the HBCD study.

They have the same arguments as the `get_levels()` function, except that
the `study` argument is set to the respective study by default, and
should not be set by the user.

## Usage

``` r
get_levels(study, release = "latest", vars = NULL, tables = NULL)

get_levels_abcd(...)

get_levels_hbcd(...)
```

## Arguments

- study:

  character. The study name. One of "abcd" or "hbcd".

- release:

  character. Release version (Default: `"latest"`).

- vars:

  character (vector). Vector with the names of variables to be included.

- tables:

  character (vector). Vector with the names of tables to be included.

- ...:

  Additional arguments passed to the underlying `get_levels()` function.

## Value

Data frame with the levels table.

## Examples

``` r
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

get_levels("hbcd", release = "1.0")
#> # A tibble: 17,051 × 5
#>    name                                         value order_level label label_es
#>    <chr>                                        <chr>       <int> <chr> <chr>   
#>  1 bio_bm_biosample_nails_results_bio_test_ord… 1               1 Cust… NA      
#>  2 bio_bm_biosample_nails_results_bio_test_ord… 2               2 Only… NA      
#>  3 bio_bm_biosample_nails_results_bio_test_ord… 3               3 Canc… NA      
#>  4 bio_bm_biosample_nails_results_bio_test_ord… 4               4 no r… NA      
#>  5 bio_bm_biosample_nails_results_bio_c_any_sp… 1               1 posi… NA      
#>  6 bio_bm_biosample_nails_results_bio_c_any_sp… 0               2 nega… NA      
#>  7 bio_bm_biosample_nails_results_bio_c_any_sp… 3               3 QNS   NA      
#>  8 bio_bm_biosample_nails_results_bio_c_any_st… 1               1 posi… NA      
#>  9 bio_bm_biosample_nails_results_bio_c_any_st… 0               2 nega… NA      
#> 10 bio_bm_biosample_nails_results_bio_c_any_st… 3               3 QNS   NA      
#> # ℹ 17,041 more rows

get_levels("abcd", vars = c("ab_g_dyn__visit_type"))
#> # A tibble: 3 × 5
#>   name                 value order_level label   label_es
#>   <chr>                <chr> <chr>       <chr>   <chr>   
#> 1 ab_g_dyn__visit_type 1     1           On-site NA      
#> 2 ab_g_dyn__visit_type 2     2           Remote  NA      
#> 3 ab_g_dyn__visit_type 3     3           Hybrid  NA      

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
