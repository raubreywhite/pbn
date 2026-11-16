# Changelog

## pbn 2026.11.17

- Scenarios: `CFG$scenarios` names the real data and the made-up
  scenarios, with the number of simulations of each. The same run script
  runs on each.
- [`initialize_project()`](https://www.rwhite.no/pbn/reference/initialize_project.md)
  takes `scenario`, and reads `scenario=<name>` and `sim=<k>` from the
  command line. It sets the seed to the simulation before org sources
  the functions and again before it calls `fake_<scenario>(dir)` for
  made-up data.
- Results go to `vNN_results/<scenario>/results.qs2`, or to
  `vNN_results/<scenario>/sim_NNNN/results.qs2` for a scenario with more
  than 1 simulation. pbn needs `org` 2026.11.16 for this.
- New
  [`run_scenarios()`](https://www.rwhite.no/pbn/reference/run_scenarios.md)
  runs every simulation of every scenario in a new R session with a
  `plnr` plan, stacks the simulations, and checks them with
  `expect_<scenario>()`.
- [`run_scenarios()`](https://www.rwhite.no/pbn/reference/run_scenarios.md)
  needs `expect_<scenario>()` for every made-up scenario, and stops
  before any run when one is missing. An `expect_` function that errors
  or returns no assertions counts as a failed assertion.
- [`run_scenarios()`](https://www.rwhite.no/pbn/reference/run_scenarios.md)
  deletes the results of a simulation before it runs it, and stops when
  the run writes no `results.qs2`. An older file is never stacked or
  checked.
- [`read_results()`](https://www.rwhite.no/pbn/reference/read_results.md)
  takes `scenario` (default `"real"`) and `sim`.
- The manifest gains `scenario`, `sim` and `sims`. New
  [`check_results()`](https://www.rwhite.no/pbn/reference/check_results.md)
  rule `manifest_scenario`.
- New
  [`check_project()`](https://www.rwhite.no/pbn/reference/check_project.md)
  rules `scenario_checks` and `manuscript_reads_real`.
  `results_contract` fails a `results.qs2` directly in `vNN_results/`.
  `unknown_path` knows `vNN_output.R`.
- Removed: `run_version()`, the command-line argument `data_raw=<path>`,
  the folder `vNN_results/scenario/<name>/`, `manifest$data_raw_source`,
  the stops of a `data_raw=` run, and `vNN_check_scenarios.R` from the
  known names of `unknown_path`.

The entries below describe the first push of this version.

- First trial version: the house layout,
  [`check_project()`](https://www.rwhite.no/pbn/reference/check_project.md),
  the results contract,
  [`table1()`](https://www.rwhite.no/pbn/reference/table1.md), and
  `run_version()`.
- A run with `data_raw=<path>` writes its results to
  `<version>_results/scenario/<basename of the normalised path>/`, so it
  never overwrites the real `results.qs2`.
- [`initialize_project()`](https://www.rwhite.no/pbn/reference/initialize_project.md)
  resolves `..`, `~` and a trailing slash in `data_raw=<path>` first.
  Before, `data_raw=..` wrote over the real `results.qs2`.
- A scenario run stops when its data folder is the default data folder,
  or is or holds the real results folder.
- A scenario run stops when its scenario folder holds the results of
  another data folder with the same basename.
- `manifest$data_md5` lists the files in the data folder at
  [`initialize_project()`](https://www.rwhite.no/pbn/reference/initialize_project.md),
  so a file that the run writes later is not an input.
- `manifest$packages` always names `qs2`, and `manifest$run_date` is the
  date only, so an unchanged run on the same day writes the same bytes.
- [`table1()`](https://www.rwhite.no/pbn/reference/table1.md) stops on a
  level of `by` named `"All"`. A level with no non-missing value is left
  out of the test. With fewer than two groups left, `p` and `test` are
  `NA`.
- In a git repository,
  [`check_project()`](https://www.rwhite.no/pbn/reference/check_project.md)
  reads only the files that git does not ignore. It never reads
  `.DS_Store`, `.Rhistory`, `.RData`, `~$*` or `.Rproj.user/`.
- [`check_project()`](https://www.rwhite.no/pbn/reference/check_project.md)
  checks the `results.qs2` of every scenario folder, and `unknown_path`
  knows `vNN_check_functions.R`, `vNN_check_scenarios.R`, top-level
  dotfiles and `*.Rproj`.
