# pbn <a href="https://www.rwhite.no/pbn/"><img src="man/figures/logo.png" align="right" width="120" /></a>

pbn ("paint by numbers") holds the house layout of an analysis project, the
checks of that layout, and a few shared helpers. It is the one policy package
of the family. `org` (paths), `cohort` (exclusions) and `plnr` (plans) are
mechanism packages without opinions.

This is a trial version. It holds the smallest set of functions that one real
project can use.

## Installation

```r
pak::pak("raubreywhite/pbn")
```

## Quick start

A project version has a run script `v01_run.R` and a folder
`v01_functions/`. `v01_functions/_cfg.R` names the scenarios in
`CFG$scenarios`: the real data, and fake data that the project makes itself.
The run script is one `plnr` plan over every scenario and simulation. Its
data steps read the data, clean it and build a `cohort`:

```r
pbn::initialize_project(version = "v01", home = ".", data_raw = "/data/raw")
scenarios <- pbn::scenarios(CFG)

p <- plnr::Plan$new()
pbn::add_data(p, "d", raw = data_raw, clean = data_clean, cohort = data_cohort)
pbn::add_analyses(p, scenarios, name = "models", fn = fit_models, outcome = CFG$outcomes)
r <- pbn::run_plan(p)
pbn::save_results(r, p, scenarios)

pbn::make_outputs(make_output, scenarios)
```

From the project folder, run it with `Rscript v01_run.R`, and check the layout
with `pbn::check_project()`.

The article
[Get started](https://www.rwhite.no/pbn/articles/pbn.html) builds a small
fake project file by file, and runs it. Read it first.

## The functions

| Function | What it does |
|---|---|
| `initialize_project()` | Sets the paths of a project version and sources `vNN_functions/` with the house defaults of `org`. |
| `scenarios()` | Checks `CFG$scenarios` and chooses the scenarios of a run. |
| `add_data()` | Adds the data steps to a plan. plnr runs them once for each scenario and simulation. A cohort gives its data and its exclusion table, and `cache = TRUE` keeps the data on disk. |
| `cohort_data()`, `keep_exclusions()` | Give the data of a cohort to a step after it, such as an imputation, and keep the exclusion table on the value of that step. |
| `add_analyses()` | Adds one analysis for each scenario, simulation and value of the extra dimensions. |
| `run_plan()` | Runs the plan in this R session or on a PSOCK cluster, and stops with every failed analysis. |
| `save_results()` | Writes one results object for each scenario and simulation, stacks the simulations, and checks the fake scenarios. `gitexclude` puts the results with data for each person in a file that git ignores. |
| `expect_recovers()`, `expect_biased()` | Check the bias, the standard error and the coverage of an estimate over the simulations of a fake scenario, and the bias of a control. |
| `make_outputs()`, `stamp_fake()` | Write the figures and the tables of the real data and of chosen fake scenarios, with fake output marked. |
| `end_unless_real()` | Ends a run script after `save_results()` when the run had no real data. |
| `read_results()` | Reads the results files of one scenario. The default is `"real"`. |
| `check_results()` | Checks a results object against the contract. |
| `check_project()` | Checks a project folder against the house layout, and quits with status 1 on a failure when R is not interactive. |
| `table1()` | Computes the numbers of a descriptive table, with chi-square or Fisher's exact p-values. |

The help page of each function holds its rules. The
[reference](https://www.rwhite.no/pbn/reference/index.html) lists them.
