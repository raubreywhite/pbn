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

## The house layout

All names are lower case. The version is a prefix.

```
v01_run.R                 data -> results -> figures and tables; calls pbn::initialize_project(version = "v01", ...)
v01_functions/            sourced folder; holds _cfg.R (top-level `CFG <- list(...)`) and project functions
v01_results/<scenario>/   the results of each scenario
v01_test/                 optional testthat files for project functions
v01_check_functions.R     optional checks of the functions
check_project.R           one line: pbn::check_project()
run_scenarios.R           one line: pbn::run_scenarios("v01")
pbn_allow.txt             optional exceptions to check_project()
manuscript/manual/vNN_<initials>.docx   hand-edited Word files (initials upper case, e.g. v16_SA.docx)
manuscript/built/vNN/     pipeline output
background/  adhoc/  data_describe/
README.md  CHANGELOG.md  AGENTS.md  .gitignore
```

## Scenarios

The real data is one scenario among fake ones. The same run script runs on
each scenario, and only the data changes. `CFG$scenarios` in `_cfg.R` names
the scenarios and the number of simulations of each:

```r
CFG <- list(
  scenarios = data.frame(scenario = c("real", "known", "null"), sims = c(1, 1, 3))
)
```

A scenario named `"real"` MUST be there. For each other scenario, the project
MUST define two functions:

- `fake_<scenario>(dir)` writes data files into `dir`. pbn sets the seed to
  the simulation number before it sources the functions and before it calls
  this function.
- `expect_<scenario>(results)` returns a data.frame with at least one row and
  the columns `assertion`, `expected`, `observed` and `pass`. An error, zero
  rows or another shape counts as a failed assertion.

`expect_real(results)` is optional.

```
v01_results/real/results.qs2                 a scenario with 1 simulation writes here directly
v01_results/null/sim_0001/results.qs2        a scenario with more than 1 simulation
v01_results/null/sim_0002/results.qs2
v01_results/null/results.qs2                 all simulations stacked
v01_results/null/check.csv                   the assertions on the stacked results
```

## Quick start

`v01_run.R`, with one commented `use_scenario()` line for each scenario in
`CFG$scenarios`:

```r
# Run one of these lines to step through a scenario by hand. The choice lasts for the R session.
# pbn::use_scenario("real", sim = 1)
# pbn::use_scenario("known", sim = 1)
# pbn::use_scenario("null", sim = 1)
p <- pbn::initialize_project(version = "v01", home = ".", data_raw = "/data/raw")
d <- data.table::fread(file.path(p$data_raw, "d.csv"))
pbn::save_results(
  exclusions = data.frame(step = "all", n_before = nrow(d), n_excluded = 0, n_after = nrow(d)),
  estimates = data.frame(term = "n", estimate = nrow(d)),
  tables = list(table1 = pbn::table1(d, by = "group", vars = CFG$vars)),
  d = d
)
pbn::end_if_fake_scenario()

# PART 3 -- OUTPUT ====
r <- pbn::read_results("v01")
utils::write.csv(r$estimates, file.path(p$results, "estimates.csv"))
```

To step through a scenario by hand, run one of the commented lines without
the `#`. Then run the script line by line. `initialize_project()` prints
which scenario it uses. For a fake scenario,
`end_if_fake_scenario()` stops with an error, and the R session stays open.
The lines MUST stay commented in the file: `use_scenario()` stops under
`Rscript`, and `check_project()` fails an active call. `check_project()` also
fails a missing line, and a line for a scenario that is not in
`CFG$scenarios`.

To run every simulation of every scenario, each in a new R session:

```r
pbn::run_scenarios("v01")
pbn::run_scenarios("v01", scenario = "known")
```

`run_scenarios()` stops when an assertion fails. `check_project()` fails a
`read_results()` call in `manuscript/built/` that reads a scenario other than
`"real"`.

## The nine functions

| Function | What it does |
|---|---|
| `initialize_project()` | Starts one simulation of one scenario with the house defaults of `org`, and sources `vNN_functions/`. |
| `use_scenario()` | Chooses the scenario of the next `initialize_project()` when a person steps through a run script by hand. |
| `end_if_fake_scenario()` | Ends a run script after `save_results()` when the scenario is fake. |
| `run_scenarios()` | Runs `vNN_run.R` for each scenario and simulation in a new R session, stacks the simulations and checks the assertions. |
| `save_results()` | Writes `results.qs2` with a manifest, and stops when the object fails the contract. |
| `read_results()` | Reads the `results.qs2` of one scenario. The default is `"real"`. |
| `check_results()` | Checks a results object against the contract. |
| `check_project()` | Checks a project folder against the house layout, and quits with status 1 on a failure when R is not interactive. |
| `table1()` | Computes the numbers of a descriptive table, with chi-square or Fisher's exact p-values. |

The help page of each function holds its rules. Start with
`?initialize_project` and `?run_scenarios`.
