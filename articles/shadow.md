# Work with shadow matrices

In this vignette, we will demonstrate how to work with shadow matrices
for NBDC studies. Shadow matrices are used to store information about
the reasons for missing values in the main data (see
[here](https://docs.hbcdstudy.org/latest/datacuration/phenotypes/#shadow-matrices)
for more details). Currently, the HBCD Study releases shadow matrices,
while the ABCD Study does not (see below for how to use the R package
`naniar` to create a basic shadow matrix for ABCD data).

## Prepare data

> **NOTE:** Before running the code in this vignette, we assume that you
> have already read the [Get
> started](https://software.nbdc-datahub.org/NBDCtools/articles/NBDCtools.md)
> page or the [Join
> data](https://software.nbdc-datahub.org/NBDCtools/articles/join.md)
> vignette, and know how to load the data/shadow from a given NBDC study
> release.

For demonstration purposes, we load a simulated HBCD dataset and shadow
matrix that are included with the package.

``` r
library(NBDCtools)
#> Welcome to the `NBDCtools` package! For more information, visit: https://software.nbdc-datahub.org/NBDCtools/
#> This package is developed by the ABCD Data Analysis, Informatics & Resource Center (DAIRC) at the J. Craig Venter Institute (JCVI)
data <- readRDS(
  system.file("extdata", "simulated_data_hbcd.rds", package = "NBDCtools")
)
dplyr::glimpse(data)
#> Rows: 20
#> Columns: 10
#> $ participant_id                                         <chr> "sub-0000000003…
#> $ session_id                                             <chr> "ses-V01", "ses…
#> $ run_id                                                 <chr> NA, NA, NA, NA,…
#> $ bio_bm_biosample_nails_results_bio_test_ordered_n      <chr> "1", "1", "2", …
#> $ mh_cg_ibqr_surg_001                                    <chr> NA, NA, NA, NA,…
#> $ mh_cg_ibqr_date_taken                                  <dttm> NA, NA, NA, NA…
#> $ `bio_bm_biosample_nails_results_bio_c_delta-9-thc_n`   <dbl> NA, NA, NA, NA,…
#> $ bio_bm_biosample_nails_results_bio_nail_weight         <int> 76, 100, 49, NA…
#> $ bio_bm_biosample_nails_results_gestational_age         <dbl> 26, 22, 37, NA,…
#> $ `img_brainswipes_xcpd-T2w_AnatOnAtlasBrainSwipes_nrev` <int> NA, NA, NA, NA,…

shadow <- readRDS(
  system.file("extdata", "simulated_data_hbcd_shadow.rds", package = "NBDCtools")
)
dplyr::glimpse(shadow)
#> Rows: 20
#> Columns: 10
#> $ participant_id                                         <chr> "sub-0000000003…
#> $ session_id                                             <chr> "ses-V01", "ses…
#> $ run_id                                                 <chr> NA, NA, NA, NA,…
#> $ bio_bm_biosample_nails_results_bio_test_ordered_n      <chr> NA, NA, NA, NA,…
#> $ mh_cg_ibqr_surg_001                                    <chr> "Reason 1", "Re…
#> $ mh_cg_ibqr_date_taken                                  <chr> "Reason 2", "Re…
#> $ `bio_bm_biosample_nails_results_bio_c_delta-9-thc_n`   <chr> "Reason 1", "Re…
#> $ bio_bm_biosample_nails_results_bio_nail_weight         <chr> NA, NA, NA, "Re…
#> $ bio_bm_biosample_nails_results_gestational_age         <chr> NA, NA, NA, "Re…
#> $ `img_brainswipes_xcpd-T2w_AnatOnAtlasBrainSwipes_nrev` <chr> "Reason 3", "Re…
```

> **IMPORTANT:** Before running any shadow-related functions, it is very
> important to ensure that the *dimensions of the main data and shadow
> matrix match*, i.e., that they have the same number of rows and
> columns, all the columns in the main data are also present in the
> shadow matrix, and all the identifier pairs (`participant_id`,
> `session_id`, `run_id`) exist in both data frames.

The
[`join_tabulated()`](https://software.nbdc-datahub.org/NBDCtools/reference/join_tabulated.md)
function has a parameter `remove_empty_rows` that removes rows that only
have missing values by default. This can lead to a situation where the
main data and shadow matrix have different dimensions, which will cause
problems in the subsequent shadow-related functions. If you know that
downstream processing involves shadow matrices, make sure to set this
parameter to `remove_empty_rows = FALSE` when calling
[`join_tabulated()`](https://software.nbdc-datahub.org/NBDCtools/reference/join_tabulated.md).

This is not a problem if you are using
[`create_dataset()`](https://software.nbdc-datahub.org/NBDCtools/reference/create_dataset.md)
function, as it automatically sets `remove_empty_rows = FALSE` when
creating a dataset with shadow matrix.

## Bind shadow matrix to main data

### Using a provided shadow matrix

If you have a data frame with a shadow matrix, you can bind it to the
main data using the
[`shadow_bind_data()`](https://software.nbdc-datahub.org/NBDCtools/reference/shadow_bind_data.md)
function. This function will bind the shadow matrix to the data using
the identifier columns (`participant_id`, `session_id`, and `run_id` by
default). The appended shadow columns will have the same column names as
the corresponding columns in the main data, but with a suffix (`_shadow`
by default).

- If there are rows with identifier pairs in the main data that don’t
  exist in the shadow matrix, the appended shadow matrix rows will be
  filled with `NA` values.
- If there are rows with identifier pairs in the shadow matrix that
  don’t exist in the main data, these rows will be dropped from the
  resulting data frame.

``` r
shadow_bind_data(
  data = data,
  shadow = shadow
) |> 
  dplyr::glimpse()
#> Rows: 20
#> Columns: 17
#> $ participant_id                                                <chr> "sub-000…
#> $ session_id                                                    <chr> "ses-V01…
#> $ run_id                                                        <chr> NA, NA, …
#> $ bio_bm_biosample_nails_results_bio_test_ordered_n             <chr> "1", "1"…
#> $ mh_cg_ibqr_surg_001                                           <chr> NA, NA, …
#> $ mh_cg_ibqr_date_taken                                         <dttm> NA, NA,…
#> $ `bio_bm_biosample_nails_results_bio_c_delta-9-thc_n`          <dbl> NA, NA, …
#> $ bio_bm_biosample_nails_results_bio_nail_weight                <int> 76, 100,…
#> $ bio_bm_biosample_nails_results_gestational_age                <dbl> 26, 22, …
#> $ `img_brainswipes_xcpd-T2w_AnatOnAtlasBrainSwipes_nrev`        <int> NA, NA, …
#> $ bio_bm_biosample_nails_results_bio_test_ordered_n_shadow      <chr> NA, NA, …
#> $ mh_cg_ibqr_surg_001_shadow                                    <chr> "Reason …
#> $ mh_cg_ibqr_date_taken_shadow                                  <chr> "Reason …
#> $ `bio_bm_biosample_nails_results_bio_c_delta-9-thc_n_shadow`   <chr> "Reason …
#> $ bio_bm_biosample_nails_results_bio_nail_weight_shadow         <chr> NA, NA, …
#> $ bio_bm_biosample_nails_results_gestational_age_shadow         <chr> NA, NA, …
#> $ `img_brainswipes_xcpd-T2w_AnatOnAtlasBrainSwipes_nrev_shadow` <chr> "Reason …
```

### Using a `naniar` shadow matrix

If there is no shadow matrix provided, like for ABCD data, or you do not
wish to use the provided shadow matrix, you can create a shadow matrix
using the [`naniar`](https://naniar.njtierney.com/) R package. To do so,
set `naniar_shadow = TRUE`.

> **IMPORTANT:** `naniar` is not a dependency of the `NBDCtools`
> package, so you need to install it separately. You can do so using the
> following command:

``` r
if (!requireNamespace("naniar", quietly = TRUE)) {
  install.packages("naniar")
}
```

To create a shadow matrix using `naniar`, you can use the following
command:

``` r
shadow_bind_data(
  data = data,
  shadow = NULL, # no shadow matrix provided
  naniar_shadow = TRUE
) |> 
  dplyr::glimpse()
#> Rows: 20
#> Columns: 17
#> $ participant_id                                            <chr> "sub-0000000…
#> $ session_id                                                <chr> "ses-V01", "…
#> $ run_id                                                    <chr> NA, NA, NA, …
#> $ bio_bm_biosample_nails_results_bio_test_ordered_n         <chr> "1", "1", "2…
#> $ mh_cg_ibqr_surg_001                                       <chr> NA, NA, NA, …
#> $ mh_cg_ibqr_date_taken                                     <dttm> NA, NA, NA,…
#> $ `bio_bm_biosample_nails_results_bio_c_delta-9-thc_n`      <dbl> NA, NA, NA, …
#> $ bio_bm_biosample_nails_results_bio_nail_weight            <int> 76, 100, 49,…
#> $ bio_bm_biosample_nails_results_gestational_age            <dbl> 26, 22, 37, …
#> $ `img_brainswipes_xcpd-T2w_AnatOnAtlasBrainSwipes_nrev`    <int> NA, NA, NA, …
#> $ bio_bm_biosample_nails_results_bio_test_ordered_n_NA      <fct> !NA, !NA, !N…
#> $ mh_cg_ibqr_surg_001_NA                                    <fct> NA, NA, NA, …
#> $ mh_cg_ibqr_date_taken_NA                                  <fct> NA, NA, NA, …
#> $ `bio_bm_biosample_nails_results_bio_c_delta-9-thc_n_NA`   <fct> NA, NA, NA, …
#> $ bio_bm_biosample_nails_results_bio_nail_weight_NA         <fct> !NA, !NA, !N…
#> $ bio_bm_biosample_nails_results_gestational_age_NA         <fct> !NA, !NA, !N…
#> $ `img_brainswipes_xcpd-T2w_AnatOnAtlasBrainSwipes_nrev_NA` <fct> NA, NA, NA, …
```

The downside of this approach is that the shadow matrix will not contain
reasons for missing values, but will only indicate that the value is
missing. The benefit of using `naniar_shadow = TRUE` is that you can use
functionality from the `naniar` R package to explore and visualize
missingness in the data (see
[here](https://naniar.njtierney.com/articles/naniar.html) for more
details).

## Fix missing values in the shadow matrix

Joining shadow matrices for different tables using
[`join_tabulated()`](https://software.nbdc-datahub.org/NBDCtools/reference/join_tabulated.md)
can result in missing values in the shadow matrix for certain
observations (e.g., because one instrument was administered at more time
points than another, or because a participant did not complete a certain
instrument). These missing values in the shadow matrix could be
mistakenly interpreted as indicating that the main data has values for
these cells. But indeed these cells are missing in both the main data
and the shadow matrix.

Here is a simplified example of this scenario:

``` r
my_table1 <- dplyr::tibble(
  participant_id = c("sub-001", "sub-002"),
  session_id = c("ses-001", "ses-002"),
  run_id = c("run-001", "run-002"),
  var1 = c("reason1", NA),
  var2 = c(NA, "reason2")
)
my_table1
#> # A tibble: 2 × 5
#>   participant_id session_id run_id  var1    var2   
#>   <chr>          <chr>      <chr>   <chr>   <chr>  
#> 1 sub-001        ses-001    run-001 reason1 NA     
#> 2 sub-002        ses-002    run-002 NA      reason2

my_table2 <- dplyr::tibble(
  participant_id = c("sub-001", "sub-003"),
  session_id = c("ses-001", "ses-003"),
  run_id = c("run-001", "run-003"),
  var3 = c(NA, "reason3"),
  var4 = c("reason4", NA)
)
my_table2
#> # A tibble: 2 × 5
#>   participant_id session_id run_id  var3    var4   
#>   <chr>          <chr>      <chr>   <chr>   <chr>  
#> 1 sub-001        ses-001    run-001 NA      reason4
#> 2 sub-003        ses-003    run-003 reason3 NA
```

When binding these two tables together, we will get the following
result:

``` r
id_table <- dplyr::full_join(
  dplyr::select(my_table1, participant_id, session_id, run_id),
  dplyr::select(my_table2, participant_id, session_id, run_id)
)
#> Joining with `by = join_by(participant_id, session_id, run_id)`
dplyr::left_join(id_table, my_table1) |> 
  dplyr::left_join(my_table2) 
#> Joining with `by = join_by(participant_id, session_id, run_id)`
#> Joining with `by = join_by(participant_id, session_id, run_id)`
#> # A tibble: 3 × 7
#>   participant_id session_id run_id  var1    var2    var3    var4   
#>   <chr>          <chr>      <chr>   <chr>   <chr>   <chr>   <chr>  
#> 1 sub-001        ses-001    run-001 reason1 NA      NA      reason4
#> 2 sub-002        ses-002    run-002 NA      reason2 NA      NA     
#> 3 sub-003        ses-003    run-003 NA      NA      reason3 NA
```

We can see that there are missing values (`NA`) in the `var3` and `var4`
columns for the `sub-002`/`ses-002`/`run-002` row and in the `var1` and
`var2` columns for the `sub-003`/`ses-003`/`run-003` row. If this table
was a shadow matrix, this would indicate that there should be values for
these cells in the main data, but in reality the shadow matrix just has
missing values due to joining the two tables.

To fix this issue, you can use the
[`shadow_replace_binding_missing()`](https://software.nbdc-datahub.org/NBDCtools/reference/shadow_replace_binding_missing.md)
function, which will replace the `NA` values in the shadow matrix with a
custom value (default `"Missing due to joining"`).

The simulated shadow matrix does not have cases where the shadow matrix
has `NA` values due to joining. For demonstration, we will manually
convert a few values to `NA`:

``` r
shadow$mh_cg_ibqr_surg_001
#>  [1] "Reason 1" "Reason 1" "Reason 1" "Reason 1" "Reason 1" "Reason 1"
#>  [7] "Reason 1" "Reason 1" "Reason 1" "Reason 1" "Reason 1" "Reason 1"
#> [13] "Reason 1" NA         "Reason 1" "Reason 1" "Reason 1" "Reason 1"
#> [19] "Reason 1" "Reason 1"
# set them to NA
shadow$mh_cg_ibqr_surg_001[1:5] <- NA_character_
shadow$mh_cg_ibqr_surg_001
#>  [1] NA         NA         NA         NA         NA         "Reason 1"
#>  [7] "Reason 1" "Reason 1" "Reason 1" "Reason 1" "Reason 1" "Reason 1"
#> [13] "Reason 1" NA         "Reason 1" "Reason 1" "Reason 1" "Reason 1"
#> [19] "Reason 1" "Reason 1"
```

Now we can use the
[`shadow_replace_binding_missing()`](https://software.nbdc-datahub.org/NBDCtools/reference/shadow_replace_binding_missing.md)
function to replace the `NA` values in the shadow matrix with an
indicator:

``` r
shadow_replace_binding_missing(
  data = data,
  shadow = shadow
)
#> # A tibble: 20 × 10
#>    participant_id session_id run_id bio_bm_biosample_nails…¹ mh_cg_ibqr_surg_001
#>    <chr>          <chr>      <chr>  <chr>                    <chr>              
#>  1 sub-0000000003 ses-V01    NA     NA                       Missing due to joi…
#>  2 sub-0000000004 ses-V01    NA     NA                       Missing due to joi…
#>  3 sub-0000000003 ses-V02    NA     NA                       Missing due to joi…
#>  4 sub-0000000005 ses-V01    NA     NA                       Missing due to joi…
#>  5 sub-0000000002 ses-V02    1      NA                       Missing due to joi…
#>  6 sub-0000000009 ses-V01    NA     Reason 3                 Reason 1           
#>  7 sub-0000000020 ses-V02    NA     NA                       Reason 1           
#>  8 sub-0000000013 ses-V01    NA     NA                       Reason 1           
#>  9 sub-0000000016 ses-V02    NA     NA                       Reason 1           
#> 10 sub-0000000014 ses-V01    NA     NA                       Reason 1           
#> 11 sub-0000000015 ses-V02    1      NA                       Reason 1           
#> 12 sub-0000000011 ses-V02    NA     NA                       Reason 1           
#> 13 sub-0000000017 ses-V02    1      NA                       Reason 1           
#> 14 sub-0000000007 ses-V01    NA     NA                       NA                 
#> 15 sub-0000000006 ses-V01    NA     NA                       Reason 1           
#> 16 sub-0000000018 ses-V01    NA     NA                       Reason 1           
#> 17 sub-0000000019 ses-V02    2      NA                       Reason 1           
#> 18 sub-0000000008 ses-V01    NA     NA                       Reason 1           
#> 19 sub-0000000010 ses-V01    NA     NA                       Reason 1           
#> 20 sub-0000000012 ses-V03    NA     NA                       Reason 1           
#> # ℹ abbreviated name: ¹​bio_bm_biosample_nails_results_bio_test_ordered_n
#> # ℹ 5 more variables: mh_cg_ibqr_date_taken <chr>,
#> #   `bio_bm_biosample_nails_results_bio_c_delta-9-thc_n` <chr>,
#> #   bio_bm_biosample_nails_results_bio_nail_weight <chr>,
#> #   bio_bm_biosample_nails_results_gestational_age <chr>,
#> #   `img_brainswipes_xcpd-T2w_AnatOnAtlasBrainSwipes_nrev` <chr>
```

As you can see, the `mh_cg_ibqr_surg_001` column now has
`"Missing due to joining"` values.

> **IMPORTANT:** Unlike the
> [`shadow_bind_data()`](https://software.nbdc-datahub.org/NBDCtools/reference/shadow_bind_data.md)
> where unmatched rows between the main data and the shadow matrix are
> allowed, this function requires the dimensions of the data and shadow
> matrix to match, i.e., to have the same number of rows and columns. If
> they do not match, the function will throw an error.
