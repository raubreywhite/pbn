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

The seven rules:

- `top_level_names`: the names are `manifest`, `exclusions`,
  `estimates`, `tables` and `extra`, each once, and there are no others.

- `exclusions_table`: `exclusions` is a data.frame with the columns
  `step`, `n_before`, `n_excluded` and `n_after`. When
  `manifest$stacked` is `TRUE`, it also has the column `sim` that
  [`run_scenarios()`](https://www.rwhite.no/pbn/reference/run_scenarios.md)
  adds. On every row, `n_before - n_excluded == n_after`.

- `no_fitted_models`: no element anywhere in `x` inherits from `lm`,
  `glm`, `gam`, `merMod`, `brmsfit`, `coxph` or `lavaan`.

- `no_row_per_person`: no data.frame anywhere in `x` has
  `manifest$n_rows_data` rows. When `n_rows_data` is `NA`, this rule
  checks nothing.

- `manifest_scenario`: `manifest` has the elements `scenario`, `sim` and
  `sims`, and `scenario` is one string.

- `manifest_data`: `manifest` has exactly one of `data_md5` and
  `fake_md5`. It has `data_md5` when `scenario` is `"real"`, and
  `fake_md5` when `scenario` is another string. `fake_md5` MUST be one
  string of 32 lower-case hex digits. `data_md5` MUST be a character
  vector of such strings, named by file. It has length 0 when the data
  folder has no file. A results file that pbn 2026.11.18 or older wrote
  has `data_md5` for a fake scenario, so it fails this rule.

- `manifest_source`: `manifest$source_md5` is a named character vector
  of md5 values, each a string of 32 lower-case hex digits. It names the
  run script `<version>_run.R` of `manifest$version`, or a `vNN_run.R`
  when the manifest has no valid version. A results file that pbn
  2026.11.20 or older wrote has no `source_md5`, so it fails this rule.

## See also

Other results:
[`read_results()`](https://www.rwhite.no/pbn/reference/read_results.md),
[`save_results()`](https://www.rwhite.no/pbn/reference/save_results.md)

## Examples

``` r
x <- list(
  manifest = list(
    scenario = "real",
    sim = 1L,
    sims = 1L,
    data_md5 = c(d.csv = "0123456789abcdef0123456789abcdef"),
    source_md5 = c(v01_run.R = "fedcba9876543210fedcba9876543210"),
    n_rows_data = 100L
  ),
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
#>                rule status
#> 1   top_level_names   PASS
#> 2  exclusions_table   FAIL
#> 3  no_fitted_models   PASS
#> 4 no_row_per_person   PASS
#> 5 manifest_scenario   PASS
#> 6     manifest_data   PASS
#> 7   manifest_source   PASS
#>                                                                 detail
#> 1                              the five top-level names, and no others
#> 2                    n_before - n_excluded != n_after on these rows: 1
#> 3                                        no fitted model in the object
#> 4                                           no data.frame has 100 rows
#> 5  the manifest names the scenario, the simulation and the simulations
#> 6 the manifest has data_md5 for "real" or fake_md5 for a fake scenario
#> 7          the manifest has source_md5, with the md5 of the run script
```
