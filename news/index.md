# Changelog

## pbn 2026.11.21

- [`run_scenarios()`](https://www.rwhite.no/pbn/reference/run_scenarios.md)
  writes the stdout and the stderr of each run to `run.log` in the
  results folder of the simulation. For a failed run, it prints the
  whole log, and the error names the run and the log. Before, the output
  was in the error message, and R cuts an error message at 1000 bytes by
  default.
- New argument `workers` of
  [`run_scenarios()`](https://www.rwhite.no/pbn/reference/run_scenarios.md):
  the number of simulations that run at the same time, each in a new R
  session. After a failed run, no new run starts, the started runs end,
  and the error names each failed run.
- New argument `resume` of
  [`run_scenarios()`](https://www.rwhite.no/pbn/reference/run_scenarios.md).
  With `TRUE`, it keeps each simulation with a `results.qs2` from a
  completed run of the same run script and functions folder, and runs
  the others. The default `FALSE` runs every simulation.
- After a run that succeeds,
  [`run_scenarios()`](https://www.rwhite.no/pbn/reference/run_scenarios.md)
  writes `run.ok` with the md5 of `results.qs2`. A run that quits with
  status 0 but writes no valid `results.qs2` for its scenario and
  simulation now fails. Before, a scenario with 1 simulation and no
  `expect_` function could pass with an invalid file.
- `run.log` and `run.ok` are run artefacts: list both in the
  `.gitignore` of a project.
  [`check_project()`](https://www.rwhite.no/pbn/reference/check_project.md)
  does not read them.
- The manifest gains `source_md5`, the md5 of the run script and of each
  file of the functions folder, for every scenario. New
  [`check_results()`](https://www.rwhite.no/pbn/reference/check_results.md)
  rule `manifest_source`.
- A `results.qs2` that pbn 2026.11.20 or older wrote has no
  `source_md5`. It fails `manifest_source` and the
  [`check_project()`](https://www.rwhite.no/pbn/reference/check_project.md)
  rule `results_contract`: run the scenarios again to write it.
- Each column of `CFG$scenarios` other than `scenario` and `sims` is a
  setting of the scenario.
  [`initialize_project()`](https://www.rwhite.no/pbn/reference/initialize_project.md)
  returns the settings of the run as `settings`, and the manifest stores
  them. A list column or an `NA` is an error. Before, pbn dropped these
  columns without a message.
- [`run_scenarios()`](https://www.rwhite.no/pbn/reference/run_scenarios.md)
  deletes the `check.csv` of each chosen scenario and each stacked
  `results.qs2` before the first run, and the `results.qs2` of each
  simulation that it runs.
- pbn no longer imports `plnr`.
  [`run_scenarios()`](https://www.rwhite.no/pbn/reference/run_scenarios.md)
  starts and watches its R sessions with `callr`.

## pbn 2026.11.20

- New article
  [`vignette("pbn")`](https://www.rwhite.no/pbn/articles/pbn.md), Get
  started. It builds a small fake project file by file and runs it. The
  README links to it and no longer holds the walk-through.

## pbn 2026.11.19

- For a fake scenario, the manifest has `fake_md5`, the md5 of the code
  of `fake_<scenario>()`, in the place of `data_md5`. A writer such as
  `haven::write_sav()` stamps the time into a file, so `data_md5`
  changed on every run. This changes the format of `results.qs2`. The
  new
  [`check_results()`](https://www.rwhite.no/pbn/reference/check_results.md)
  rule `manifest_data` fails the results file of a fake scenario that an
  older pbn wrote: run the scenarios again to write it. The rule also
  fails a `data_md5` or `fake_md5` that is not in the form of an md5.
- [`run_scenarios()`](https://www.rwhite.no/pbn/reference/run_scenarios.md)
  keeps the class of each stacked table, so a data.table in simulation 1
  gives a stacked data.table. Before, every stacked table was a
  data.frame.
- New
  [`check_project()`](https://www.rwhite.no/pbn/reference/check_project.md)
  rule `use_scenario_lines`. A `vNN_run.R` MUST have exactly one
  top-level comment `# pbn::use_scenario("<name>", sim = 1)` above
  [`pbn::initialize_project()`](https://www.rwhite.no/pbn/reference/initialize_project.md)
  for each scenario of `CFG$scenarios`. A comment for another scenario
  fails too. To read them,
  [`check_project()`](https://www.rwhite.no/pbn/reference/check_project.md)
  sources `vNN_functions/` in a new R session, so it runs project code.

## pbn 2026.11.18

- Removed the `scenario` argument of
  [`initialize_project()`](https://www.rwhite.no/pbn/reference/initialize_project.md).
  A call that still passes it stops with R’s unused-argument error. The
  command line `scenario=<name> sim=<k>` gives the scenario, or else the
  new
  [`use_scenario()`](https://www.rwhite.no/pbn/reference/use_scenario.md),
  or else `"real"`.
- New `use_scenario(scenario, sim = 1)` chooses the scenario for the
  rest of the R session when a person steps through a run script by
  hand. It stops in a non-interactive R session.
- In an interactive R session,
  [`initialize_project()`](https://www.rwhite.no/pbn/reference/initialize_project.md)
  prints one line with the scenario and the simulation, and `(fake)` for
  a fake scenario. A call that stops leaves no run for
  [`save_results()`](https://www.rwhite.no/pbn/reference/save_results.md)
  or
  [`end_if_fake_scenario()`](https://www.rwhite.no/pbn/reference/end_if_fake_scenario.md).
- New
  [`end_if_fake_scenario()`](https://www.rwhite.no/pbn/reference/end_if_fake_scenario.md)
  ends a run script after
  [`save_results()`](https://www.rwhite.no/pbn/reference/save_results.md)
  for a fake scenario, so the output part runs for the real data only.
  It quits with status 0 under `Rscript`, and
  [`run_scenarios()`](https://www.rwhite.no/pbn/reference/run_scenarios.md)
  counts that as a success.
- New
  [`check_project()`](https://www.rwhite.no/pbn/reference/check_project.md)
  rule `no_active_use_scenario`. It fails an active call or the
  reference
  [`pbn::use_scenario`](https://www.rwhite.no/pbn/reference/use_scenario.md),
  also in a function body or an `if`. `run_script_initialises` names
  each argument that
  [`initialize_project()`](https://www.rwhite.no/pbn/reference/initialize_project.md)
  does not have.
- `unknown_path` knows `run_scenarios.R`. It no longer knows
  `vNN_output.R`: the output part goes in `vNN_run.R`, after
  [`end_if_fake_scenario()`](https://www.rwhite.no/pbn/reference/end_if_fake_scenario.md).

## pbn 2026.11.17

- Scenarios: `CFG$scenarios` names the real data and the fake scenarios,
  with the number of simulations of each. The same run script runs on
  each.
- [`initialize_project()`](https://www.rwhite.no/pbn/reference/initialize_project.md)
  takes `scenario`, and reads `scenario=<name>` and `sim=<k>` from the
  command line. It sets the seed to the simulation before org sources
  the functions and again before it calls `fake_<scenario>(dir)` for
  fake data.
- Results go to `vNN_results/<scenario>/results.qs2`, or to
  `vNN_results/<scenario>/sim_NNNN/results.qs2` for a scenario with more
  than 1 simulation. pbn needs `org` 2026.11.16 for this.
- New
  [`run_scenarios()`](https://www.rwhite.no/pbn/reference/run_scenarios.md)
  runs every simulation of every scenario in a new R session with a
  `plnr` plan, stacks the simulations, and checks them with
  `expect_<scenario>()`.
- [`run_scenarios()`](https://www.rwhite.no/pbn/reference/run_scenarios.md)
  needs `expect_<scenario>()` for every fake scenario, and stops before
  any run when one is missing. An `expect_` function that errors or
  returns no assertions counts as a failed assertion.
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
