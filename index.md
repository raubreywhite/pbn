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
`v01_functions/`. `v01_functions/_cfg.R` names the scenarios: the real
data, and fake data that the project writes itself. The same run script
runs on each scenario. From the project folder, run every scenario and
check the layout:

``` r
pbn::run_scenarios("v01")
pbn::check_project()
```

The article [Get started](https://www.rwhite.no/pbn/articles/pbn.html)
builds a small fake project file by file, and runs it. Read it first.

## The nine functions

| Function | What it does |
|----|----|
| [`initialize_project()`](https://www.rwhite.no/pbn/reference/initialize_project.md) | Starts one simulation of one scenario with the house defaults of `org`, and sources `vNN_functions/`. |
| [`use_scenario()`](https://www.rwhite.no/pbn/reference/use_scenario.md) | Chooses the scenario of the next [`initialize_project()`](https://www.rwhite.no/pbn/reference/initialize_project.md) when a person steps through a run script by hand. |
| [`end_if_fake_scenario()`](https://www.rwhite.no/pbn/reference/end_if_fake_scenario.md) | Ends a run script after [`save_results()`](https://www.rwhite.no/pbn/reference/save_results.md) when the scenario is fake. |
| [`run_scenarios()`](https://www.rwhite.no/pbn/reference/run_scenarios.md) | Runs `vNN_run.R` for each scenario and simulation in a new R session, stacks the simulations and checks the assertions. |
| [`save_results()`](https://www.rwhite.no/pbn/reference/save_results.md) | Writes `results.qs2` with a manifest, and stops when the object fails the contract. |
| [`read_results()`](https://www.rwhite.no/pbn/reference/read_results.md) | Reads the `results.qs2` of one scenario. The default is `"real"`. |
| [`check_results()`](https://www.rwhite.no/pbn/reference/check_results.md) | Checks a results object against the contract. |
| [`check_project()`](https://www.rwhite.no/pbn/reference/check_project.md) | Checks a project folder against the house layout, and quits with status 1 on a failure when R is not interactive. |
| [`table1()`](https://www.rwhite.no/pbn/reference/table1.md) | Computes the numbers of a descriptive table, with chi-square or Fisher’s exact p-values. |

The help page of each function holds its rules. The
[reference](https://www.rwhite.no/pbn/reference/index.html) lists them.
