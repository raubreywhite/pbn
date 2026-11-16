# Run every scenario of a project version in new R sessions

`run_scenarios()` runs `<home>/<version>_run.R` once for each simulation
of each scenario, each time in a new R session. It then stacks the
simulations of each scenario and checks the assertions of the project.

## Usage

``` r
run_scenarios(version, scenario = NULL, home = ".")
```

## Arguments

- version:

  The version, for example `"v01"`.

- scenario:

  `NULL` for every scenario, or a character vector of the scenarios to
  run.

- home:

  The project folder.

## Value

Invisibly, a data.frame of every assertion, with the columns `scenario`,
`assertion`, `expected`, `observed` and `pass`.

## Details

`run_scenarios()` does these steps in this order:

1.  It reads `CFG$scenarios` in a new R session that sources
    `<version>_functions/` with
    [`org::initialize_project()`](https://www.rwhite.no/org/reference/initialize_project.html).
    The R session of the caller does not change. It stops when a
    scenario other than `"real"` has no function `expect_<scenario>()`.
    It checks every chosen scenario before it runs any of them.

2.  It makes a
    [plnr::Plan](https://www.rwhite.no/plnr/reference/Plan.html) with
    one analysis for each scenario and simulation. Each analysis runs
    the script with
    [`callr::rscript()`](https://callr.r-lib.org/reference/rscript.html)
    and the arguments `scenario=<name> sim=<k>`, with `home` as the
    working directory. A run that fails stops `run_scenarios()` with the
    output of the run.

    Before each run, `run_scenarios()` deletes the `results.qs2` of that
    simulation. Before simulation 1, it also deletes the `results.qs2`
    and the `check.csv` in `<version>_results/<scenario>/`. A run that
    writes no `results.qs2` stops `run_scenarios()`. A run that quits
    with status 0 after it writes `results.qs2` succeeds, so
    [`end_if_fake_scenario()`](https://www.rwhite.no/pbn/reference/end_if_fake_scenario.md)
    can end the run of a fake scenario. Files of the scenarios that it
    does not run stay as they are.

3.  For each scenario with more than 1 simulation, it stacks the files
    `<version>_results/<scenario>/sim_<kkkk>/results.qs2` into
    `<version>_results/<scenario>/results.qs2`. It stops when the files
    are not exactly the simulations 1 to `sims`. `estimates`,
    `exclusions` and each data.frame in `tables` get a first column
    `sim`. Each keeps the class of its object in simulation 1, so a
    data.table stays a data.table. `extra` is an empty list. The
    `manifest` is that of simulation 1. In it, `sim` and `n_rows_data`
    are `NA`, `sims` is the number of simulations, and `stacked` is
    `TRUE`. For a fake scenario, it stops when a simulation has no valid
    `manifest$fake_md5`, or when the simulations do not have one value
    of it.

4.  For each scenario that has a function `expect_<scenario>()` in
    `<version>_functions/`, it calls that function on the results of the
    scenario in a new R session. `expect_real()` is optional. The
    function MUST return a data.frame with at least one row and the
    columns `assertion`, `expected`, `observed` and `pass`.
    `run_scenarios()` writes it to
    `<version>_results/<scenario>/check.csv` and prints it.

Each of these counts as one failed assertion, and `run_scenarios()`
writes it to `check.csv` too:

- `expect_<scenario>()` stops with an error. The assertion holds the
  error message.

- `expect_<scenario>()` returns zero rows.

- `expect_<scenario>()` returns something other than a data.frame with
  the four columns.

After every scenario is done, `run_scenarios()` stops when a value of
`pass` is not `TRUE`. The error lists the failed assertions.

## See also

Other project setup:
[`end_if_fake_scenario()`](https://www.rwhite.no/pbn/reference/end_if_fake_scenario.md),
[`initialize_project()`](https://www.rwhite.no/pbn/reference/initialize_project.md),
[`use_scenario()`](https://www.rwhite.no/pbn/reference/use_scenario.md)

## Examples

``` r
# \donttest{
home <- file.path(tempdir(), "pbn_run_scenarios_example")
dir.create(file.path(home, "v01_functions"), recursive = TRUE)
dir.create(file.path(home, "data"))
utils::write.csv(data.frame(x = 1:4), file.path(home, "data", "d.csv"))
writeLines(
  c(
    "CFG <- list(",
    "  scenarios = data.frame(scenario = c('real', 'null'), sims = c(1, 2))",
    ")",
    "fake_null <- function(dir) {",
    "  utils::write.csv(data.frame(x = rnorm(4)), file.path(dir, 'd.csv'))",
    "}",
    "expect_null <- function(results) {",
    "  n <- length(unique(results$estimates$sim))",
    "  data.frame(assertion = 'two simulations', expected = 2,",
    "    observed = n, pass = n == 2)",
    "}"
  ),
  file.path(home, "v01_functions", "_cfg.R")
)
writeLines(
  c(
    'p <- pbn::initialize_project("v01", home = ".", data_raw = "data")',
    "d <- utils::read.csv(file.path(p$data_raw, 'd.csv'))",
    "pbn::save_results(",
    "  exclusions = data.frame(step = character(0), n_before = numeric(0),",
    "    n_excluded = numeric(0), n_after = numeric(0)),",
    "  estimates = data.frame(term = 'mean_x', estimate = mean(d$x)),",
    "  tables = list()",
    ")"
  ),
  file.path(home, "v01_run.R")
)
pbn::run_scenarios("v01", home = home)
#> pbn: scenario real, simulation 1
#> pbn: scenario null, simulation 1
#> pbn: scenario null, simulation 2
#> Assertions of the scenario "null":
#>        assertion expected observed pass
#>  two simulations        2        2 TRUE
pbn::read_results("v01", scenario = "null", home = home)$estimates
#>   sim   term    estimate
#> 1   1 mean_x  0.07921043
#> 2   2 mean_x -0.06364893

unlink(home, recursive = TRUE)
# }
```
