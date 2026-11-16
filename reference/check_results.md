# Check a results object against the contract

`check_results()` checks a results object against the contract of
[`save_results()`](https://www.rwhite.no/pbn/reference/save_results.md).
It reports each rule as PASS or FAIL, and it stops on nothing.

## Usage

``` r
check_results(x)
```

## Arguments

- x:

  The results object, normally from
  [`read_results()`](https://www.rwhite.no/pbn/reference/read_results.md).

## Value

A data.frame with one row per rule and the columns `rule`, `status` and
`detail`. `status` is `"PASS"` or `"FAIL"`.

## Details

The four rules:

- `top_level_names`: the names are `manifest`, `exclusions`,
  `estimates`, `tables` and `extra`, each once, and there are no others.

- `exclusions_table`: `exclusions` is a data.frame with the columns
  `step`, `n_before`, `n_excluded` and `n_after`. On every row,
  `n_before - n_excluded == n_after`.

- `no_fitted_models`: no element anywhere in `x` inherits from `lm`,
  `glm`, `gam`, `merMod`, `brmsfit`, `coxph` or `lavaan`.

- `no_row_per_person`: no data.frame anywhere in `x` has
  `manifest$n_rows_data` rows. When `n_rows_data` is `NA`, this rule
  checks nothing.

## See also

Other results:
[`read_results()`](https://www.rwhite.no/pbn/reference/read_results.md),
[`save_results()`](https://www.rwhite.no/pbn/reference/save_results.md)

## Examples

``` r
x <- list(
  manifest = list(n_rows_data = 100L),
  exclusions = data.frame(
    step = "age 18 or more",
    n_before = 100,
    n_excluded = 4,
    n_after = 97
  ),
  estimates = data.frame(term = "x", estimate = 1.2),
  tables = list(),
  extra = list()
)
pbn::check_results(x)
#>                rule status                                            detail
#> 1   top_level_names   PASS           the five top-level names, and no others
#> 2  exclusions_table   FAIL n_before - n_excluded != n_after on these rows: 1
#> 3  no_fitted_models   PASS                     no fitted model in the object
#> 4 no_row_per_person   PASS                        no data.frame has 100 rows
```
