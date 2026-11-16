# Run every scenario of a project version in new R sessions

`run_scenarios()` runs `<home>/<version>_run.R` once for each simulation
of each scenario, each time in a new R session. It then stacks the
simulations of each scenario and checks the assertions of the project.

## Usage

``` r
run_scenarios(
  version,
  scenario = NULL,
  home = ".",
  workers = 1,
  resume = FALSE
)
```

## Arguments

- version:

  The version, for example `"v01"`.

- scenario:

  `NULL` for every scenario, or a character vector of the scenarios to
  run.

- home:

  The project folder.

- workers:

  The number of simulations that run at the same time, one whole number
  of 1 or more. Each simulation runs in its own new R session.

- resume:

  `FALSE` runs every simulation of the chosen scenarios. `TRUE` keeps
  each simulation with results from the same code, and runs the others.
  See the section Resume.

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

2.  With `resume = TRUE`, it chooses the simulations to keep, and prints
    one line for each of them.

3.  It deletes these files, and it stops before any run when one of them
    stays:

    - the `check.csv` of each chosen scenario.

    - the stacked `results.qs2` of each chosen scenario with more than 1
      simulation.

    - the `results.qs2` and the `run.ok` of each simulation that it
      runs.

    Files of the scenarios that it does not run stay as they are.

4.  It runs the script once for each simulation that it does not keep.
    Each run is a new R session with the arguments
    `scenario=<name> sim=<k>`, and with `home` as the working directory.
    At most `workers` runs go at the same time. The stdout and the
    stderr of each run go to `run.log` in the results folder of the
    simulation. Each run writes `run.log` again, also a run that
    succeeds.

    A run succeeds when all of these hold:

    - It quits with status 0, so
      [`end_if_fake_scenario()`](https://www.rwhite.no/pbn/reference/end_if_fake_scenario.md)
      can end the run of a fake scenario.

    - Its `results.qs2` exists and passes
      [`check_results()`](https://www.rwhite.no/pbn/reference/check_results.md).

    - `manifest$scenario`, `manifest$sim` and `manifest$sims` are those
      of the simulation.

    After a run that succeeds, `run_scenarios()` writes `run.ok` beside
    `run.log`. `run.ok` holds one line, the md5 of `results.qs2`. A run
    that stops after it writes `results.qs2` thus has no `run.ok`. This
    is also the case when the caller is interrupted. `run.log` and
    `run.ok` are run artefacts. List both in the `.gitignore` of the
    project.

    Each other run fails. For a run that fails, `run_scenarios()` prints
    the whole `run.log` with [`cat()`](https://rdrr.io/r/base/cat.html).
    After the first failure, it starts no new run, and the runs that
    started go on to their end. It then stops with an error that names
    the scenario, the simulation and the `run.log` of each failed run.
    It stacks and checks no scenario.

5.  For each scenario with more than 1 simulation, it stacks the files
    `<version>_results/<scenario>/sim_<kkkk>/results.qs2` into
    `<version>_results/<scenario>/results.qs2`. It stops when the files
    are not exactly the simulations 1 to `sims`. After `sims` in
    `CFG$scenarios` becomes smaller, the folders of the higher
    simulations stay. Stacking then stops, and the error names their
    files. Delete those folders to stack again. The section Stacking
    gives the rules.

6.  For each scenario that has a function `expect_<scenario>()` in
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

## Stacking

The stacked results hold every element of the results of simulation 1,
in the same order. These rules apply at each depth:

- A data.frame gets a first column `sim`, and the rows of the
  simulations in order. It keeps the class of simulation 1, so a
  data.table stays a data.table.

- A list is stacked element by element, and pbn matches the elements by
  name. So each element of a list MUST have its own name. An unnamed or
  partly named list stops stacking. A scenario with 1 simulation MAY
  hold such a list.

- Each other element becomes a list with one entry for each simulation,
  named `sim_0001`, `sim_0002` and so on. This includes an atomic
  vector, a matrix, a factor, a date and `NULL`.

Stacking stops when two simulations differ in the structure at one path.
The error names the path and the two simulations. Examples:

- a list with other names or another length.

- a data.frame with other columns, a column of another class, or a
  factor column with other levels.

- a list in the place of a data.frame, or `NULL` in the place of a
  value.

Stacking also stops on a data.frame with a column `sim`, because pbn
adds that column.

The `manifest` is that of simulation 1. In it, `sim` and `n_rows_data`
are `NA`, `sims` is the number of simulations, and `stacked` is `TRUE`.
For a fake scenario, stacking stops when a simulation has no valid
`manifest$fake_md5`, or when the simulations do not have one value of
it. It also stops when the simulations do not have one value of
`manifest$settings`.

A scenario with 1 simulation is not stacked. Its `results.qs2` is the
file that the run wrote.

## Workers

Each simulation depends only on its scenario and its number, so
`workers` does not change the results files or `check.csv`. Only the
order of the lines on the console can change. pbn does not limit the
threads that R or the BLAS library starts in each R session.

## Resume

With `resume = TRUE`, `run_scenarios()` keeps a simulation when all of
these hold:

- Its `results.qs2` exists and passes
  [`check_results()`](https://www.rwhite.no/pbn/reference/check_results.md).

- Its `run.ok` exists and holds the md5 of `results.qs2` now. So the run
  completed, and nothing changed `results.qs2` after it.

- `manifest$scenario`, `manifest$sim` and `manifest$sims` are those of
  the simulation.

- `manifest$source_md5` is the md5 of the files of `<version>_run.R` and
  `<version>_functions/` now.

- For a fake scenario, `manifest$fake_md5` is the md5 of the code of
  `fake_<scenario>()` now.

Each other simulation runs again, and the console line of its start
gives the reason. The stacking and the `expect_` assertions always run
again.

Resume does not find a change in the real data, or in the version of R
or of a package. It also does not find a change in a file outside
`<version>_run.R` and `<version>_functions/`. After such a change, use
`resume = FALSE`.

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
    "  estimates = data.frame(term = 'mean_x', estimate = mean(d$x)),",
    "  n_rows = nrow(d),",
    "  d = d",
    ")"
  ),
  file.path(home, "v01_run.R")
)
pbn::run_scenarios("v01", home = home)
#> pbn: scenario real, simulation 1: started
#> pbn: scenario real, simulation 1: ran
#> pbn: scenario null, simulation 1: started
#> pbn: scenario null, simulation 1: ran
#> pbn: scenario null, simulation 2: started
#> pbn: scenario null, simulation 2: ran
#> Assertions of the scenario "null":
#>        assertion expected observed pass
#>  two simulations        2        2 TRUE
stacked <- pbn::read_results("v01", scenario = "null", home = home)
stacked$estimates
#>   sim   term    estimate
#> 1   1 mean_x  0.07921043
#> 2   2 mean_x -0.06364893
stacked$n_rows
#> $sim_0001
#> [1] 4
#> 
#> $sim_0002
#> [1] 4
#> 

unlink(home, recursive = TRUE)
# }
```
