# Get release version number(s)

These functions retrieve all release version number(s) for a given
study, or the latest release version number.

## Usage

``` r
get_releases(study)

get_releases_abcd()

get_releases_hbcd()

get_latest_release(study)

get_latest_release_abcd()

get_latest_release_hbcd()
```

## Arguments

- study:

  character. The study name. One of "abcd" or "hbcd".

## Value

character. The latest release version number(s) of the specified study.

## Examples

``` r
get_releases("abcd")
#> [1] "6.0" "6.1"
get_releases("hbcd")
#> [1] "1.0" "1.1" "2.0"
get_latest_release("abcd")
#> [1] "6.1"
get_latest_release("hbcd")
#> [1] "2.0"
```
