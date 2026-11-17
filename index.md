# pbn

# pbn

pbn (“paint by numbers”) holds the house layout of an analysis project,
the checks of that layout, and a few shared helpers. It is the one
policy package of the family. `org` (paths), `cohort` (exclusions) and
`plnr` (plans) are mechanism packages without opinions.

This is a trial version. It holds the smallest set of functions that one
real project can use.

## Installation

``` r
pak::pak("raubreywhite/pbn")
```

## Quick start

A project version has a run script `v01_run.R` and a folder
`v01_functions/`. `v01_functions/_cfg.R` names the scenarios in
`CFG$scenarios`: the real data, and fake data that the project makes
itself. The run script is one `plnr` plan over every scenario and
simulation:

``` r
pbn::initialize_project(version = "v01", home = ".", data_raw = "/data/raw")
scenarios <- pbn::scenarios(CFG)

p <- plnr::Plan$new()
pbn::add_data(p, "d", fn = function(scenario, sim) get_data(scenario, sim))
pbn::add_analyses(p, scenarios, name = "models", fn = fit_models, outcome = CFG$outcomes)
r <- pbn::run_plan(p)
pbn::save_results(r, p, scenarios)

pbn::end_unless_real(scenarios)
```

From the project folder, run it with `Rscript v01_run.R`, and check the
layout with
[`pbn::check_project()`](https://www.rwhite.no/pbn/reference/check_project.md).

The article [Get started](https://www.rwhite.no/pbn/articles/pbn.html)
builds a small fake project file by file, and runs it. Read it first.

## The functions

| Function | What it does |
|----|----|
| [`initialize_project()`](https://www.rwhite.no/pbn/reference/initialize_project.md) | Sets the paths of a project version and sources `vNN_functions/` with the house defaults of `org`. |
| [`scenarios()`](https://www.rwhite.no/pbn/reference/scenarios.md) | Checks `CFG$scenarios` and chooses the scenarios of a run. |
| [`add_data()`](https://www.rwhite.no/pbn/reference/add_data.md) | Adds a data function to a plan. plnr calls it once for each scenario and simulation. |
| [`add_analyses()`](https://www.rwhite.no/pbn/reference/add_analyses.md) | Adds one analysis for each scenario, simulation and value of the extra dimensions. |
| [`run_plan()`](https://www.rwhite.no/pbn/reference/run_plan.md) | Runs the plan in this R session or on a PSOCK cluster, and stops with every failed analysis. |
| [`save_results()`](https://www.rwhite.no/pbn/reference/save_results.md) | Writes one results object for each scenario and simulation, stacks the simulations, and checks the fake scenarios. |
| [`end_unless_real()`](https://www.rwhite.no/pbn/reference/end_unless_real.md) | Ends a run script after [`save_results()`](https://www.rwhite.no/pbn/reference/save_results.md) when the run had no real data. |
| [`read_results()`](https://www.rwhite.no/pbn/reference/read_results.md) | Reads the `results.qs2` of one scenario. The default is `"real"`. |
| [`check_results()`](https://www.rwhite.no/pbn/reference/check_results.md) | Checks a results object against the contract. |
| [`check_project()`](https://www.rwhite.no/pbn/reference/check_project.md) | Checks a project folder against the house layout, and quits with status 1 on a failure when R is not interactive. |
| [`table1()`](https://www.rwhite.no/pbn/reference/table1.md) | Computes the numbers of a descriptive table, with chi-square or Fisher’s exact p-values. |

The help page of each function holds its rules. The
[reference](https://www.rwhite.no/pbn/reference/index.html) lists them.
