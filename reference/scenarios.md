# Choose the scenarios of a run

`scenarios()` checks the scenario table `cfg$scenarios` and returns it,
or the rows of the scenarios in `only`. The real data is the scenario
`"real"`, and each other scenario is fake.

## Usage

``` r
scenarios(cfg, only = NULL)
```

## Arguments

- cfg:

  The config list of the project, normally `CFG` from
  `<version>_functions/_cfg.R`. It MUST have the element `scenarios`.

- only:

  `NULL` for every scenario, or a character vector of scenario names.

## Value

A data.frame with the columns `scenario`, `sims`, `output` when
`cfg$scenarios` has it, and each setting column. It has one row for each
chosen scenario in the order of `cfg$scenarios`. `sims` is an integer.

## Details

`cfg$scenarios` MUST be a data.frame with these columns:

- `scenario`: unique names that match `^[a-z0-9_]+$`. One name MUST be
  `"real"`.

- `sims`: the number of simulations of the scenario, a whole number of 1
  or more.

- `output`: optional. `TRUE` or `FALSE` on each row, and `FALSE` for
  `"real"`.
  [`make_outputs()`](https://www.rwhite.no/pbn/reference/make_outputs.md)
  writes the output of a fake scenario with `TRUE`. This column is not a
  setting. It is not in the argsets, in the key of the cache or in
  `manifest$settings`.

Each other column is a setting of the scenario, for example `n_boot` for
a number of bootstrap replicates that is smaller in the fake scenarios.
A setting column MUST be an atomic vector with no `NA`, and its name
MUST be syntactic. Its name MUST NOT be one of `scenario`, `sim`,
`analysis`, `seed`, `index_analysis`, `name`, `fn`, `fn_name`, `data`
and `argset`.
[`add_analyses()`](https://www.rwhite.no/pbn/reference/add_analyses.md)
puts the setting of the scenario into each argset.

`scenarios()` stops on a table that breaks a rule, and on a name in
`only` that is not in the table. The error names the problem.

pbn keeps the value of the last call of `scenarios()` in this R session,
and
[`initialize_project()`](https://www.rwhite.no/pbn/reference/initialize_project.md)
removes it.
[`add_data()`](https://www.rwhite.no/pbn/reference/add_data.md) gives it
to the plan, and
[`get_data()`](https://www.rwhite.no/pbn/reference/get_data.md) reads it
there before
[`add_analyses()`](https://www.rwhite.no/pbn/reference/add_analyses.md).

## See also

[`vignette("pbn")`](https://www.rwhite.no/pbn/articles/pbn.md), which
builds and runs a small fake project.

Other project setup:
[`end_unless_real()`](https://www.rwhite.no/pbn/reference/end_unless_real.md),
[`initialize_project()`](https://www.rwhite.no/pbn/reference/initialize_project.md)

## Examples

``` r
CFG <- list(
  scenarios = data.frame(
    scenario = c("real", "null"),
    sims = c(1, 3),
    n_boot = c(200, 20)
  )
)
pbn::scenarios(CFG)
#>   scenario sims n_boot
#> 1     real    1    200
#> 2     null    3     20
pbn::scenarios(CFG, only = "null")
#>   scenario sims n_boot
#> 1     null    3     20
```
