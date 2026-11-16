# pbn 2026.11.19

- For a fake scenario, the manifest has `fake_md5`, the md5 of the code of `fake_<scenario>()`, in the place of `data_md5`. A writer such as `haven::write_sav()` stamps the time into a file, so `data_md5` changed on every run. This changes the format of `results.qs2`. The new `check_results()` rule `manifest_data` fails the results file of a fake scenario that an older pbn wrote: run the scenarios again to write it. The rule also fails a `data_md5` or `fake_md5` that is not in the form of an md5.
- `run_scenarios()` keeps the class of each stacked table, so a data.table in simulation 1 gives a stacked data.table. Before, every stacked table was a data.frame.
- New `check_project()` rule `use_scenario_lines`. A `vNN_run.R` MUST have exactly one top-level comment `# pbn::use_scenario("<name>", sim = 1)` above `pbn::initialize_project()` for each scenario of `CFG$scenarios`. A comment for another scenario fails too. To read them, `check_project()` sources `vNN_functions/` in a new R session, so it runs project code.

# pbn 2026.11.18

- Removed the `scenario` argument of `initialize_project()`. A call that still passes it stops with R's unused-argument error. The command line `scenario=<name> sim=<k>` gives the scenario, or else the new `use_scenario()`, or else `"real"`.
- New `use_scenario(scenario, sim = 1)` chooses the scenario for the rest of the R session when a person steps through a run script by hand. It stops in a non-interactive R session.
- In an interactive R session, `initialize_project()` prints one line with the scenario and the simulation, and `(fake)` for a fake scenario. A call that stops leaves no run for `save_results()` or `end_if_fake_scenario()`.
- New `end_if_fake_scenario()` ends a run script after `save_results()` for a fake scenario, so the output part runs for the real data only. It quits with status 0 under `Rscript`, and `run_scenarios()` counts that as a success.
- New `check_project()` rule `no_active_use_scenario`. It fails an active call or the reference `pbn::use_scenario`, also in a function body or an `if`. `run_script_initialises` names each argument that `initialize_project()` does not have.
- `unknown_path` knows `run_scenarios.R`. It no longer knows `vNN_output.R`: the output part goes in `vNN_run.R`, after `end_if_fake_scenario()`.

# pbn 2026.11.17

- Scenarios: `CFG$scenarios` names the real data and the fake scenarios, with the number of simulations of each. The same run script runs on each.
- `initialize_project()` takes `scenario`, and reads `scenario=<name>` and `sim=<k>` from the command line. It sets the seed to the simulation before org sources the functions and again before it calls `fake_<scenario>(dir)` for fake data.
- Results go to `vNN_results/<scenario>/results.qs2`, or to `vNN_results/<scenario>/sim_NNNN/results.qs2` for a scenario with more than 1 simulation. pbn needs `org` 2026.11.16 for this.
- New `run_scenarios()` runs every simulation of every scenario in a new R session with a `plnr` plan, stacks the simulations, and checks them with `expect_<scenario>()`.
- `run_scenarios()` needs `expect_<scenario>()` for every fake scenario, and stops before any run when one is missing. An `expect_` function that errors or returns no assertions counts as a failed assertion.
- `run_scenarios()` deletes the results of a simulation before it runs it, and stops when the run writes no `results.qs2`. An older file is never stacked or checked.
- `read_results()` takes `scenario` (default `"real"`) and `sim`.
- The manifest gains `scenario`, `sim` and `sims`. New `check_results()` rule `manifest_scenario`.
- New `check_project()` rules `scenario_checks` and `manuscript_reads_real`. `results_contract` fails a `results.qs2` directly in `vNN_results/`. `unknown_path` knows `vNN_output.R`.
- Removed: `run_version()`, the command-line argument `data_raw=<path>`, the folder `vNN_results/scenario/<name>/`, `manifest$data_raw_source`, the stops of a `data_raw=` run, and `vNN_check_scenarios.R` from the known names of `unknown_path`.

The entries below describe the first push of this version.

- First trial version: the house layout, `check_project()`, the results contract, `table1()`, and `run_version()`.
- A run with `data_raw=<path>` writes its results to `<version>_results/scenario/<basename of the normalised path>/`, so it never overwrites the real `results.qs2`.
- `initialize_project()` resolves `..`, `~` and a trailing slash in `data_raw=<path>` first. Before, `data_raw=..` wrote over the real `results.qs2`.
- A scenario run stops when its data folder is the default data folder, or is or holds the real results folder.
- A scenario run stops when its scenario folder holds the results of another data folder with the same basename.
- `manifest$data_md5` lists the files in the data folder at `initialize_project()`, so a file that the run writes later is not an input.
- `manifest$packages` always names `qs2`, and `manifest$run_date` is the date only, so an unchanged run on the same day writes the same bytes.
- `table1()` stops on a level of `by` named `"All"`. A level with no non-missing value is left out of the test. With fewer than two groups left, `p` and `test` are `NA`.
- In a git repository, `check_project()` reads only the files that git does not ignore. It never reads `.DS_Store`, `.Rhistory`, `.RData`, `~$*` or `.Rproj.user/`.
- `check_project()` checks the `results.qs2` of every scenario folder, and `unknown_path` knows `vNN_check_functions.R`, `vNN_check_scenarios.R`, top-level dotfiles and `*.Rproj`.
