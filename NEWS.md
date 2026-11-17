# pbn 2026.11.23

The run script is now one `plnr` plan over every scenario and simulation. pbn no longer starts the run script in a new R session for each scenario. To move a project, change these items:

| Removed | Use instead |
|---|---|
| `p <- pbn::initialize_project(...)`, with `p$scenario`, `p$sim`, `p$data_raw` and `p$settings` | `pbn::initialize_project(...)` sets the paths only. A data function reads `org::project$data_raw`, and each argset holds `scenario`, `sim` and the settings. |
| `use_scenario()` and its commented lines in `vNN_run.R` | `pbn::get_argset(p, "<scenario>", <sim>, "<name>", <i>)` and `pbn::get_data(p, "<scenario>", <sim>)` inside `if (plnr::is_run_directly())`. |
| `run_scenarios()`, `run_scenarios.R`, `workers` and `resume` | `pbn::scenarios(CFG)`, or `pbn::scenarios(CFG, only = "null")`, and `pbn::run_plan(p, workers = 2)`. A run always runs every chosen scenario. |
| `fake_<scenario>(dir)` and the folder `data/` of a fake run | One data function `get_data(scenario, sim)` for every scenario, added with `pbn::add_data()`. |
| The command-line arguments `scenario=` and `sim=` | `pbn::scenarios(CFG, only = ...)` in the run script. |
| `save_results(..., d = d)` | `pbn::save_results(r, p, scenarios)`. Each element is an analysis kind of `pbn::add_analyses()`. |
| `end_if_fake_scenario()` | `pbn::end_unless_real(scenarios)`. |
| `run.log` and `run.ok` | Nothing. `run_plan()` prints each failed analysis with its full error. |

- New `scenarios()`, `add_data()`, `add_analyses()`, `run_plan()` and `end_unless_real()`.
- `initialize_project()` sets no seed and reads no command line. It creates no folder, and it stops when `vNN_functions/` is missing. `data_raw` MAY be absent: only `save_results()` for `"real"` then stops.
- A data function MUST take `scenario` and `sim` without a default, or neither. `add_data()` stops on a default for either and on `sim` without `scenario`, and it sets the seed to `sim` before each call.
- Each analysis gets its own seed in `argset$seed`, from its kind, its counter and `sim`, and never `sim` itself for sims 1 to 9999. To step through an analysis by hand, run `set.seed(argset$seed)`.
- With more workers, `run_plan()` gives the same results when a function reads only its arguments, the functions folder, the attached packages and the seed. R options are not copied to the workers.
- `add_analyses(control = TRUE)` adds an analysis for the fake scenarios only. The real results never hold it.
- `run_plan(workers > 1)` needs doParallel and foreach, which are now in Suggests. It copies the objects of the sourced environment and the paths of `org::project` to each worker, and attaches the attached packages there.
- `run_plan()` fails an analysis that adds, removes, renames or replaces a column of the shared data. It does not find a change of values in place. When it stops, the results files of an earlier run stay.
- `save_results()` stops on a value of `run_plan()` from another plan, and on `scenarios` with other settings than the argsets.
- `read_results()` stops on results whose `check.csv` holds an assertion that did not pass, holds no assertion, or has no column `pass`. The error says how to read them with `qs2::qs_read()`.
- `save_results()` marks new results "not checked yet" in `check.csv` until their `expect_` function has run. It writes each `check.csv` to a temporary file and renames it.
- `scenario_checks` fails a `check.csv` with no assertion.
- The manifest gains `analyses`, the analysis kinds with `control`. `n_rows_data` is a named vector with one count for each data element. `fake_md5` is the md5 of the code of the data functions of `add_data()`. The manifest no longer holds `data_raw`.
- `no_row_per_person` checks each count of `n_rows_data` of 2 or more. A results file with one count works as before.
- `check_project()` no longer has the rules `no_active_use_scenario` and `use_scenario_lines`, and it no longer starts an R session. `unknown_path` no longer knows `run_scenarios.R`.
- pbn imports `plnr` (>= 2026.11.17) again. `callr` moves to Suggests.
- A data function MAY return a `cohort::CohortPipeline`. The analyses get its data.table after the exclusions, and `save_results()` writes its exclusion table to the element `exclusions`, with a column `data`. `cohort` is in Suggests.
- New argument `cache` of `add_data()`. With `TRUE`, `run_plan()` keeps the data of each scenario and simulation in `vNN_cache_gitexclude/`, and reads it again while the cache key is the same. It prints one line for each scenario and simulation, with the reason of each new computation.
- In a git repository, `run_plan()` with a cache and `save_results()` with `gitexclude` stop until git ignores the files they write.
- `save_results(gitexclude = )` writes the kinds that it names to `results_gitexclude.qs2`, and deletes an old one when it names none. `read_results()` reads both files, and gives one warning when the second is missing.
- The manifest gains `data_cache`, and `manifest$analyses` gains the column `file`.
- `add_analyses()` stops on the kind name `exclusions`. The rule `exclusions_table` allows the column `data`.
- New `cohort_data()` and `keep_exclusions()`, for a step after the cohort such as `data_cohort() |> data_impute()`. Its value MAY be any R object. For a list of data.frames with one number of rows, `n_rows_data` is the rows of one of them.
- The manifest gains `n_rows_before_exclusions`, the rows given to the cohort. `no_row_per_person` checks it too.
- `p$get_data()` and `p$run_one()` by hand use the cache. The cache line gives the date and the time.
- The cache key holds the md5 of the raw data at the time of the use, and `RNGkind()`. A use stops when a `data_*.R` file changed after `initialize_project()`. The key file records the versions of the loaded packages, and a new version computes the data again.
- `run_plan()` computes the md5 of a changed raw data file again, and `manifest$data_md5` names the data that the run read.
- The manifest gains `n_rows_after_exclusions`, which `no_row_per_person` checks too.
- `element_names` passes a `results.qs2` with `manifest` only when the manifest puts kinds in `results_gitexclude.qs2`.
- New `check_project()` rule `gitexclude_not_tracked`. `results_contract` also checks each `results_gitexclude.qs2`, and `unknown_path` knows `vNN_cache_gitexclude/`.
- New `expect_recovers()` checks the bias, the standard error and the coverage of an estimate over the simulations of a fake scenario. Each limit is 3.89 Monte Carlo standard errors, and each row shows the smallest error that it finds with a probability of 80%.
- New `expect_biased()`, for a control analysis that MUST be biased. Both functions fail an assertion below 100 simulations, or 288 for the coverage of a 95% interval, and above 5% missing values. They also fail an `se` of 0 or less, a `lower` above its `upper`, and estimates that are all equal.
- New `make_outputs()` calls the output function of the project for `"real"` and for each fake scenario with `output = TRUE`. The fake output goes to `<scenario>/output_fake_data/` with the name prefix `fake_data_`, and pbn empties each folder first. It stops when the call for a fake scenario changes a file outside its folder. It deletes the output of a scenario whose checks failed.
- New `stamp_fake()`, which puts the label "FAKE DATA" on a ggplot of a fake scenario. ggplot2 is in Suggests, and grid in Imports.
- `CFG$scenarios` MAY have the column `output`, `TRUE` or `FALSE`, and `FALSE` for `"real"`. It is not a setting. Before, a column `output` was a setting of any atomic type.
- New `check_project()` rule `fake_data_names`.
- The article Get started has the scenarios `null` and `confounded`, with 140 simulations each and recovery checks, and an output part with `make_outputs()`.
- `add_data()` takes the data as the steps `raw`, `clean`, `cohort` and `after_cohort`, or as one function `fn`. The data fails when `clean` changes the number of rows, or when the cohort does not start with every row. Each exclusion thus goes through the cohort, which counts it.
- pbn puts the exclusion table of the cohort on the value of `after_cohort`, so the step needs no `keep_exclusions()`. `keep_exclusions()` is for a data function `fn`.
- With the steps, the cache key and `fake_md5` hold each step, and the cache line names a changed step. The article Get started uses the steps.
- The error of `run_plan()` also names the first failed analysis and holds its error. pbn cuts it so that the whole error stays below 900 bytes. A knitr build keeps this error and loses the output of `cat()`.
- `run_plan()` with workers starts each worker with the library paths of this R session, through `R_LIBS`. Before, a worker did not find a library that the session added with `.libPaths()`, for example that of `R CMD build` on Windows.
- git gets the stacked `<scenario>/results.qs2` only. `save_results()` writes the results of each simulation to `<scenario>/sims_gitexclude/sim_<kkkk>.qs2`, and in a git repository it stops until git ignores that folder.
- `save_results()` deletes the folders `sim_<kkkk>/` of the old layout of a scenario, and prints one line.
- `save_results()` empties the folder `sims_gitexclude/` of a scenario before it writes it, and deletes it for a scenario with 1 simulation. It prints one line when it deleted a file. Before, a stale simulation stayed, and `read_results(sim = )` read it.
- In a git repository, `save_results()` checks each file that it writes into `sims_gitexclude/`, with one call of git, and names the first 3 that git does not ignore.
- `read_results(sim = k)` reads `sims_gitexclude/sim_<kkkk>.qs2`. Without that file, for example in a clone, it stops and says to run the scenario.
- `results_contract` checks each file of `sims_gitexclude/` on the disk, and needs none in a clone.
- `save_results()` prints each failed assertion with `cat()`. Its error gives the count, the scenarios and the path of `check.csv`, because R cut the list at 1000 bytes.
- `no_row_per_person` checks stacked results for each simulation, with the new `manifest$n_rows_by_sim`. Before, it checked nothing on stacked results.
- New argument `label` of `expect_recovers()` and `expect_biased()`. The text of the bias and coverage assertions names the truth, for example "bias of estimate against 0.673".
- A kind whose analyses each return a list of data.frames with the same names is a named list of row-bound data.frames, for example `results$sem$estimates`. This changes the shape of such a kind in results that this unreleased version wrote, where it was `results$sem$sem__1` and so on.
- `manifest$packages` also names the packages that the workers loaded, for example the package of a model.
- New `get_data(p, scenario, sim)` and `get_argset(p, scenario, sim, name, i)` give the data and the argset of one analysis without its name. An example is `pbn::get_data(p, "real")$d`. `no_local_copy_of_pbn_function` allows a project function `get_data`.
- New argument `coverage` of `expect_recovers()`. With `FALSE`, it makes no coverage assertion, needs no `lower` and `upper`, and needs 100 simulations.
- `read_results(sim = 1)` reads the `results.qs2` of a scenario with 1 simulation.
- On Windows, in a git repository, `run_plan()` and `save_results()` stopped even when `.gitignore` held `*gitexclude*`. pbn now gives `git check-ignore` its paths with LF line ends, because git reads the CR of CR LF as part of a path.
- `save_results()` writes the run log `vNN_results/run.md`. It holds the result, the failures first, the scenarios, the data, the exclusions, the assertions, the files and the timings. It also writes it when it stops on a failed assertion or a failed rule of `check_results()`.
- Only the sections When and Timings of the run log change in every run. So the log goes to git, and `git diff` shows the real changes of a run.
- `run_plan()` measures each data step and each analysis where it runs, also on a worker. The timings go to the run log, and not to the value of `run_plan()`, a results file or a cache key.
- `run_plan()` writes the run log with the failed analyses when it stops on them after `initialize_project()`. At its start, it replaces the run log with one that says the run has not finished.
- The run log never holds the text of an error. It numbers each error, and `vNN_results/run_errors_gitexclude.md` holds the text when git ignores that file.
- `save_results()` writes the run log as failed when it stops in the checks of its arguments.
- `make_outputs()` adds its files and its seconds to the run log of `save_results()` in the same R session.
- New `render_run()` renders the run log to `run.pdf` with Quarto and its Typst format, without LaTeX. A run never calls it. It writes the PDF to a temporary file and renames it.
- `unknown_path` reads the files directly in `vNN_results/`, and knows `run.md`, `run.pdf` and `run_errors_gitexclude.md` there.
- `get_data(p, scenario, sim)` works directly after `add_data()`. It takes the scenarios of the last `scenarios()` before `add_data()`, which the plan keeps.
- `save_results()` and `make_outputs()` stop when a file that they delete stays, for example because another program holds it open on Windows. The error names the file.
- `make_outputs()` compares the new names of the files of a fake scenario in lower case, as Windows and macOS do.
- pbn passes R CMD check on Windows (R-hub, R-devel).
- `run_plan()` with workers now runs the analyses through doFuture, and no longer through doParallel. The future plan is `future::cluster`, on the PSOCK cluster that pbn makes. doParallel does not relay the progress of a worker to this R session, and a future does. future and doFuture replace doParallel in Suggests.
- A future empties the global environment of its worker. So with workers, each batch of analyses takes the objects of the functions folder to its worker. The output, the messages and the warnings of a worker now reach this R session. A worker has no option `mc.cores`, as before.
- `run_plan()` shows one line for each scenario, drawn again in place in a console. A log gets no line in the first 60 seconds. Then it gets one when a scenario finishes, and at most one every 60 seconds for a running scenario. It replaces the progress bar of plnr. pbn imports progressr.
- New `progress(done, total, label)` shows the progress of a long analysis on the line of its scenario. It does nothing outside `run_plan()` and in a forked child process.
- `run_plan()` fails an analysis that changes an object of the functions folder. An example is the index that a data.table subset `CFG$table[x == 1]` adds in place. `initialize_project()` records these objects.
- The rule `no_local_copy_of_pbn_function` also fails a function `progress` of a project.
- `progress()` checks its arguments at each call, also within 0.5 seconds of a call that it showed.
- An error outside an analysis, for example of a worker that stops, no longer ends `run_plan()` with a raw error of future. `run_plan()` writes the run log with the error, sets the backend back, and stops with one message.
- An analysis fails when it leaves a new object in the global environment. A data function fails when it changes an object of the functions folder.
- The data functions and the analyses set the kind of random number generator of the run before each seed. An analysis that calls `RNGkind()` then changes no later draw.
- `run_plan()` and `save_results()` read the argsets of a plan in one pass. Before, the time grew with the square of the number of analyses.

# pbn 2026.11.22

- `save_results(..., d)` saves each named argument as one result under its name. A call with the names `exclusions`, `estimates`, `tables` and `extra` works as before.
- `save_results()` stops on a result named `manifest`, an unnamed result, a repeated or non-syntactic name, and a call with no result.
- `d` is required, so a call of `save_results()` without `d` stops. Pass `d = d`, or `d = NULL` when the run has no single data table. Before, `d` was `NULL` by default.
- `exclusions` is optional. When it is there, the rule `exclusions_table` checks it as before. `estimates` no longer has to be a data.frame, and `tables` no longer has to be a list of data.frames.
- A result MAY be a data.frame, an atomic vector, `NULL`, or a list of these to any depth. The new rule `allowed_objects` replaces `no_fitted_models`. It also fails a function, an environment, an S4 or R6 object, a formula and a list with a class outside a fixed set. The detail names the path and the class.
- The new rule `element_names` replaces `top_level_names`. The rules `exclusions_table`, `no_row_per_person`, `manifest_scenario`, `manifest_data` and `manifest_source` keep their names.
- `run_scenarios()` stacks every element of the results. Before, it dropped `extra`. A data.frame gets a first column `sim`, and a list is stacked element by element. Each other value becomes a list with one entry for each simulation.
- Stacking stops when two simulations differ in the structure at one path, and the error names the path and the two simulations.
- `no_row_per_person` also fails an atomic vector of length `n_rows_data` and a matrix or an array with a dimension `n_rows_data`, for example `fitted(fit)`. It does not look in `manifest`, and it checks nothing when `n_rows_data` is 0 or 1. Before, a 1-row table failed when the data had 1 row.
- `allowed_objects` and `no_row_per_person` also walk the attributes of each object, so the `terms` attribute of a model frame fails. They skip the attributes that R and data.table set, for example `names` and `.internal.selfref`.
- The detail of `allowed_objects` gives a fix for a model, a formula, an `htest`, a `POSIXlt`, a `package_version` and an `sf` geometry.
- Stacking stops on a nested list that is unnamed or partly named, and on `NULL` in the place of a value. It also stops on a data.frame column of another class, a factor column with other levels, and a data.frame with a column `sim`.
- A results file that pbn 2026.11.21 wrote passes the new rules, unless it holds an object that `allowed_objects` or `no_row_per_person` fails.

# pbn 2026.11.21

- `run_scenarios()` writes the stdout and the stderr of each run to `run.log` in the results folder of the simulation. For a failed run, it prints the whole log, and the error names the run and the log. Before, the output was in the error message, and R cuts an error message at 1000 bytes by default.
- New argument `workers` of `run_scenarios()`: the number of simulations that run at the same time, each in a new R session. After a failed run, no new run starts, the started runs end, and the error names each failed run.
- New argument `resume` of `run_scenarios()`. With `TRUE`, it keeps each simulation with a `results.qs2` from a completed run of the same run script and functions folder, and runs the others. The default `FALSE` runs every simulation.
- After a run that succeeds, `run_scenarios()` writes `run.ok` with the md5 of `results.qs2`. A run that quits with status 0 but writes no valid `results.qs2` for its scenario and simulation now fails. Before, a scenario with 1 simulation and no `expect_` function could pass with an invalid file.
- `run.log` and `run.ok` are run artefacts: list both in the `.gitignore` of a project. `check_project()` does not read them.
- The manifest gains `source_md5`, the md5 of the run script and of each file of the functions folder, for every scenario. New `check_results()` rule `manifest_source`.
- A `results.qs2` that pbn 2026.11.20 or older wrote has no `source_md5`. It fails `manifest_source` and the `check_project()` rule `results_contract`: run the scenarios again to write it.
- Each column of `CFG$scenarios` other than `scenario` and `sims` is a setting of the scenario. `initialize_project()` returns the settings of the run as `settings`, and the manifest stores them. A list column or an `NA` is an error. Before, pbn dropped these columns without a message.
- `run_scenarios()` deletes the `check.csv` of each chosen scenario and each stacked `results.qs2` before the first run, and the `results.qs2` of each simulation that it runs.
- pbn no longer imports `plnr`. `run_scenarios()` starts and watches its R sessions with `callr`.

# pbn 2026.11.20

- New article `vignette("pbn")`, Get started. It builds a small fake project file by file and runs it. The README links to it and no longer holds the walk-through.

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
