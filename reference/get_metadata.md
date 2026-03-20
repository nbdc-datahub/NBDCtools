# Get metadata

Retrieves metadata (data dictionary, levels table, event map) for a
given study and release version. Allows for filtering by variables and
tables.

## Usage

``` r
get_metadata(
  study,
  release = "latest",
  vars = NULL,
  tables = NULL,
  type = "dd"
)
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

- type:

  character. Type of metadata to retrieve. One of `"dd"`, `"levels"`,
  `"sessions"` (Default: `"dd"`).

## Value

Data frame with the metadata.

## Examples

``` r
get_metadata("abcd", type = "levels")
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

get_metadata("hbcd", release = "1.0")
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

get_metadata("abcd", vars = c("ab_g_dyn__visit_dtt", "ab_g_dyn__visit_age"))
#> # A tibble: 2 × 44
#>   study domain sub_domain source metric atlas table_name table_label name  label
#>   <chr> <chr>  <chr>      <chr>  <chr>  <chr> <chr>      <chr>       <chr> <chr>
#> 1 Core  ABCD … Standard … Gener… NA     NA    ab_g_dyn   ABCD Dynam… ab_g… Visi…
#> 2 Core  ABCD … Standard … Gener… NA     NA    ab_g_dyn   ABCD Dynam… ab_g… Visi…
#> # ℹ 34 more variables: instruction <chr>, header <chr>, note <chr>, unit <chr>,
#> #   type_var <chr>, type_data <chr>, type_level <chr>, type_field <chr>,
#> #   order_display <chr>, branching_logic <chr>, label_es <chr>,
#> #   instruction_es <chr>, header_es <chr>, note_es <chr>, table_nda <chr>,
#> #   table_nda_5_0 <chr>, table_redcap <chr>, name_nda <chr>, name_deap <chr>,
#> #   name_redcap <chr>, name_redcap_exp <chr>, url_table <chr>,
#> #   url_warn_use <chr>, url_warn_data <chr>, url_table_warn_use <chr>, …

get_metadata("abcd", tables = "ab_g_dyn")
#> # A tibble: 24 × 44
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
#> # ℹ 14 more rows
#> # ℹ 35 more variables: label <chr>, instruction <chr>, header <chr>,
#> #   note <chr>, unit <chr>, type_var <chr>, type_data <chr>, type_level <chr>,
#> #   type_field <chr>, order_display <chr>, branching_logic <chr>,
#> #   label_es <chr>, instruction_es <chr>, header_es <chr>, note_es <chr>,
#> #   table_nda <chr>, table_nda_5_0 <chr>, table_redcap <chr>, name_nda <chr>,
#> #   name_deap <chr>, name_redcap <chr>, name_redcap_exp <chr>, …

get_metadata("abcd", tables = "ab_g_dyn")
#> # A tibble: 24 × 44
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
#> # ℹ 14 more rows
#> # ℹ 35 more variables: label <chr>, instruction <chr>, header <chr>,
#> #   note <chr>, unit <chr>, type_var <chr>, type_data <chr>, type_level <chr>,
#> #   type_field <chr>, order_display <chr>, branching_logic <chr>,
#> #   label_es <chr>, instruction_es <chr>, header_es <chr>, note_es <chr>,
#> #   table_nda <chr>, table_nda_5_0 <chr>, table_redcap <chr>, name_nda <chr>,
#> #   name_deap <chr>, name_redcap <chr>, name_redcap_exp <chr>, …

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
