# Get data dictionary

Retrieves data dictionary for a given study and release version. Allows
for filtering by variables and tables. Wrapper around
[`get_metadata()`](https://software.nbdc-datahub.org/NBDCtools/reference/get_metadata.md).

In addition to the main `get_dd()` function, there are two
study-specific variations:

- `get_dd_abcd()`: for the ABCD study.

- `get_dd_hbcd()`: for the HBCD study.

They have the same arguments as the `get_dd()` function, except that the
`study` argument is set to the respective study by default, and should
not be set by the user.

## Usage

``` r
get_dd(study, release = "latest", vars = NULL, tables = NULL)

get_dd_abcd(...)

get_dd_hbcd(...)
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

  Additional arguments passed to the underlying `get_dd()` function.

## Value

Data frame with the data dictionary.

## Examples

``` r
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

get_dd("hbcd", release = "1.0")
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

get_dd("abcd", vars = c("ab_g_dyn__visit_dtt", "ab_g_dyn__visit_age"))
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

get_dd("abcd", tables = "ab_g_dyn")
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
