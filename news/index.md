# Changelog

## pbn 2026.11.29

- Fix: a .R file with CR LF line ends no longer counts as changed in
  [`freeze_version()`](https://www.rwhite.no/pbn/reference/freeze_version.md)
  and the rule `frozen_versions_unchanged` of `house_2026_11_19`, which
  pbn 2026.11.26 released. pbn hashes a .R file with each CR LF as LF,
  and each other file as its bytes.
- Behaviour change: `fake_md5` also holds the code of each function of
  the functions folder that the data code calls, as the cache key does.
  Data code that calls no such function keeps its `fake_md5`.
- Each record of the loaded packages of a cache key now holds the
  packages that qs2 loads, such as Rcpp. Before, only the records after
  the first save of the cache in an R session held them.
- The run log, the progress line of a failed analysis, the error of
  [`save_results()`](https://www.rwhite.no/pbn/reference/save_results.md)
  on failed assertions and the error of
  [`scenarios()`](https://www.rwhite.no/pbn/reference/scenarios.md) on a
  missing setting write each scenario name as code, for example “the
  scenario `real`”. The error of
  [`save_results()`](https://www.rwhite.no/pbn/reference/save_results.md)
  says “scenario” for one scenario.
- The fix of `frozen_versions_unchanged` says to run
  [`pbn::freeze_version()`](https://www.rwhite.no/pbn/reference/freeze_version.md)
  again “to record the change as intended”. The message of
  [`freeze_version()`](https://www.rwhite.no/pbn/reference/freeze_version.md)
  says “now records these changes as intended:”.

## pbn 2026.11.28

- New
  [`create_project()`](https://www.rwhite.no/pbn/reference/create_project.md)
  writes a new project at version v01 that passes
  [`check_project()`](https://www.rwhite.no/pbn/reference/check_project.md).
  It stops, and writes nothing, when one of its files exists. Its run
  script runs the fake scenarios only, so `verify("v01")` gives a PASS
  on the fake scenarios before code reads the real data. It stops before
  it writes anything when the package cohort is not installed.
- New
  [`new_version()`](https://www.rwhite.no/pbn/reference/new_version.md)
  freezes the newest version and copies its run script and its functions
  folder to a new version. It changes the version of the call
  [`initialize_project()`](https://www.rwhite.no/pbn/reference/initialize_project.md),
  and adds the section of the version to `CHANGELOG.md`. It stops in the
  cases that
  [`?new_version`](https://www.rwhite.no/pbn/reference/new_version.md)
  lists.
- [`create_project()`](https://www.rwhite.no/pbn/reference/create_project.md)
  and
  [`new_version()`](https://www.rwhite.no/pbn/reference/new_version.md)
  remove each file and folder that they created when they stop part way,
  so a second call can start again.
- Behaviour change: after a run without real results, the verdict line
  of a PASS of
  [`verify()`](https://www.rwhite.no/pbn/reference/verify.md) names the
  fake scenarios only. The verdict and the exit status stay the same.
- [`verify()`](https://www.rwhite.no/pbn/reference/verify.md) writes
  “The run wrote no check of a fake scenario.” for a run without
  `check.csv`.
- New template `inst/templates/AGENTS.md`.
  [`create_project()`](https://www.rwhite.no/pbn/reference/create_project.md)
  copies it into the project.
- The set `house_2026_11_19` gets three new rules:
  `readme_and_changelog` (fail), `changelog_with_code` (warn) and
  `comment_share` (warn, `limit` 0.25). pbn 2026.11.26 released this
  set, and no project used it yet. The maintainer approved the change. A
  project on `house_2026_11_17` sees no change.
- In `house_2026_11_19`, `no_local_copy_of_pbn_function` knows the
  exports `create_project` and `new_version`.

## pbn 2026.11.27

- Behaviour change:
  [`run_plan()`](https://www.rwhite.no/pbn/reference/run_plan.md) fails
  an analysis that changes values of the shared data in place, for
  example `data$d[1, x := 0]`. The error names each changed column.
  Before, each later analysis in the same R session got the changed
  data.
- The cache key of `add_data(cache = TRUE)` holds the code of each
  function of `vNN_functions/` that the data code calls, also through
  other functions. A changed helper in another file now computes the
  data again, and the cache line names it.
- `rlang` moves into Imports, because the check of the shared data uses
  [`rlang::hash()`](https://rlang.r-lib.org/reference/hash.html).

## pbn 2026.11.26

- New rule set `house_2026_11_19`: the set `house_2026_11_17` with three
  new rules. A project on `house_2026_11_17` sees no change. The line
  for a new `pbn.yml` now names `house_2026_11_19`.
- New
  [`freeze_version()`](https://www.rwhite.no/pbn/reference/freeze_version.md)
  writes `vNN_frozen.md5`, the md5 of `vNN_run.R` and of each file under
  `vNN_functions/`. The new rule `frozen_versions_unchanged` fails when
  such a file changes, comes or goes, and when a version older than the
  newest has no `vNN_frozen.md5`.
- New rule `no_cross_version_reads` fails a string of a run script or a
  functions folder that names another version, such as `"v01_results"`
  in `v02_functions/`, or that names the folder `adhoc/`.
- New rule `no_escape_hatches` warns on
  [`suppressWarnings()`](https://rdrr.io/r/base/warning.html),
  [`suppressMessages()`](https://rdrr.io/r/base/message.html),
  [`try()`](https://rdrr.io/r/base/try.html),
  [`tryCatch()`](https://rdrr.io/r/base/conditions.html) and
  [`withCallingHandlers()`](https://rdrr.io/r/base/conditions.html) in a
  run script or a functions folder.
- New [`verify()`](https://www.rwhite.no/pbn/reference/verify.md) runs
  `vNN_run.R` in a new R session, checks the project, and prints the
  exit status, the failed assertions and each FAIL and WARN. Its verdict
  is FAIL also for a failed assertion when `scenario_checks` is off, and
  for a run that wrote no `check.csv`. It reads only the `check.csv`
  files of this run.
- In `house_2026_11_19`, `unknown_path` names `vNN_frozen.md5`, and
  `no_local_copy_of_pbn_function` knows the exports `freeze_version` and
  `verify`.
- A `pbn.yml` that names a rule that its set does not have stops, and
  the message names the oldest set with the rule.
- `callr` moves from Suggests to Imports, because
  [`verify()`](https://www.rwhite.no/pbn/reference/verify.md) uses it.

## pbn 2026.11.25

- Behaviour change:
  [`read_results()`](https://www.rwhite.no/pbn/reference/read_results.md)
  for `"real"` now stops when a fake scenario of the same run has a
  failed assertion. The fake and the real results come from the same
  code, so a failed fake check can show a fault in the real results.
- [`save_results()`](https://www.rwhite.no/pbn/reference/save_results.md)
  writes each fake scenario with a failed assertion to the new
  `manifest$failed_fake_checks` of the real results. It writes the fake
  scenarios first. Real results of pbn 2026.11.24 or older have no such
  element, and read as before.
- [`show_block()`](https://www.rwhite.no/pbn/reference/show_blocks.md)
  and
  [`show_blocks()`](https://www.rwhite.no/pbn/reference/show_blocks.md)
  stop on such real results too.
  [`make_outputs()`](https://www.rwhite.no/pbn/reference/make_outputs.md)
  then skips `"real"` and prints one line.
- The rule `manifest_data` of
  [`check_results()`](https://www.rwhite.no/pbn/reference/check_results.md)
  checks `failed_fake_checks` of `"real"`. It needs it when
  `manifest$pbn_version` is 2026.11.25 or newer.
- Each WARN and FAIL of
  [`check_project()`](https://www.rwhite.no/pbn/reference/check_project.md)
  ends with `Fix:` and the change that removes it. The rules, their
  levels, each status and the columns of the value stay the same.

## pbn 2026.11.24

- [`initialize_project()`](https://www.rwhite.no/pbn/reference/initialize_project.md)
  now applies the rule `loc_per_file` as
  [`check_project()`](https://www.rwhite.no/pbn/reference/check_project.md)
  does, also the section `allow` of `pbn.yml`. Before, a file under
  `allow: loc_per_file:` stopped the start of the project.

## pbn 2026.11.23

The run script is now one `plnr` plan over every scenario and
simulation. pbn no longer starts the run script in a new R session for
each scenario. To move a project, change these items:

| Removed | Use instead |
|----|----|
| `p <- pbn::initialize_project(...)`, with `p$scenario`, `p$sim`, `p$data_raw` and `p$settings` | `pbn::initialize_project(...)` sets the paths only. A data function reads `pbn::folder("main")`, and each argset holds `scenario`, `sim` and the settings. |
| `use_scenario()` and its commented lines in `vNN_run.R` | `pbn::get_argset(p, "<scenario>", <sim>, "<name>", <i>)` and `pbn::get_data(p, "<scenario>", <sim>)` inside `if (plnr::is_run_directly())`. |
| `run_scenarios()`, `run_scenarios.R`, `workers` and `resume` | `pbn::scenarios(CFG)`, or `pbn::scenarios(CFG, only = "null")`, and `pbn::run_plan(p, workers = 2)`. A run always runs every chosen scenario. |
| `fake_<scenario>(dir)` and the folder `data/` of a fake run | One data function `get_data(scenario, sim)` for every scenario, added with [`pbn::add_data()`](https://www.rwhite.no/pbn/reference/add_data.md). |
| The command-line arguments `scenario=` and `sim=` | `pbn::scenarios(CFG, only = ...)` in the run script. |
| `save_results(..., d = d)` | `pbn::save_results(r, p, scenarios)`. Each element is an analysis kind of [`pbn::add_analyses()`](https://www.rwhite.no/pbn/reference/add_analyses.md). |
| `end_if_fake_scenario()` | `pbn::end_unless_real(scenarios)`. |
| `run.log` and `run.ok` | Nothing. [`run_plan()`](https://www.rwhite.no/pbn/reference/run_plan.md) prints each failed analysis with its full error. |

- New [`scenarios()`](https://www.rwhite.no/pbn/reference/scenarios.md),
  [`add_data()`](https://www.rwhite.no/pbn/reference/add_data.md),
  [`add_analyses()`](https://www.rwhite.no/pbn/reference/add_analyses.md),
  [`run_plan()`](https://www.rwhite.no/pbn/reference/run_plan.md) and
  [`end_unless_real()`](https://www.rwhite.no/pbn/reference/end_unless_real.md).
- [`initialize_project()`](https://www.rwhite.no/pbn/reference/initialize_project.md)
  sets no seed and reads no command line. It creates no folder, and it
  stops when `vNN_functions/` is missing. A folder of the project MAY be
  absent: only
  [`save_results()`](https://www.rwhite.no/pbn/reference/save_results.md)
  for `"real"` then stops.
- A data function MUST take `scenario` and `sim` without a default, or
  neither.
  [`add_data()`](https://www.rwhite.no/pbn/reference/add_data.md) stops
  on a default for either and on `sim` without `scenario`, and it sets
  the seed to `sim` before each call.
- Each analysis gets its own seed in `argset$seed`, from its kind, its
  counter and `sim`, and never `sim` itself for sims 1 to 9999. To step
  through an analysis by hand, run `set.seed(argset$seed)`.
- With more workers,
  [`run_plan()`](https://www.rwhite.no/pbn/reference/run_plan.md) gives
  the same results when a function reads only its arguments, the
  functions folder, the attached packages and the seed. R options are
  not copied to the workers.
- `add_analyses(control = TRUE)` adds an analysis for the fake scenarios
  only. The real results never hold it.
- `run_plan(workers > 1)` needs doParallel and foreach, which are now in
  Suggests. It copies the objects of the sourced environment and the
  paths of
  [`org::project`](https://www.rwhite.no/org/reference/project.html) to
  each worker, and attaches the attached packages there.
- [`run_plan()`](https://www.rwhite.no/pbn/reference/run_plan.md) fails
  an analysis that adds, removes, renames or replaces a column of the
  shared data. It does not find a change of values in place. When it
  stops, the results files of an earlier run stay.
- [`save_results()`](https://www.rwhite.no/pbn/reference/save_results.md)
  stops on a value of
  [`run_plan()`](https://www.rwhite.no/pbn/reference/run_plan.md) from
  another plan, and on `scenarios` with other settings than the argsets.
- [`read_results()`](https://www.rwhite.no/pbn/reference/read_results.md)
  stops on results whose `check.csv` holds an assertion that did not
  pass, holds no assertion, or has no column `pass`. The error says how
  to read them with
  [`qs2::qs_read()`](https://rdrr.io/pkg/qs2/man/qs_read.html).
- [`save_results()`](https://www.rwhite.no/pbn/reference/save_results.md)
  marks new results “not checked yet” in `check.csv` until their
  `expect_` function has run. It writes each `check.csv` to a temporary
  file and renames it.
- `scenario_checks` fails a `check.csv` with no assertion.
- The manifest gains `analyses`, the analysis kinds with `control`.
  `n_rows_data` is a named vector with one count for each data element.
  `fake_md5` is the md5 of the code of the data functions of
  [`add_data()`](https://www.rwhite.no/pbn/reference/add_data.md). The
  manifest no longer holds `data_raw`.
- `no_row_per_person` checks each count of `n_rows_data` of 2 or more. A
  results file with one count works as before.
- [`check_project()`](https://www.rwhite.no/pbn/reference/check_project.md)
  no longer has the rules `no_active_use_scenario` and
  `use_scenario_lines`, and it no longer starts an R session.
  `unknown_path` no longer knows `run_scenarios.R`.
- pbn imports `plnr` (\>= 2026.11.17) again. `callr` moves to Suggests.
- A data function MAY return a
  [`cohort::CohortPipeline`](https://www.rwhite.no/cohort/reference/CohortPipeline.html).
  The analyses get its data.table after the exclusions, and
  [`save_results()`](https://www.rwhite.no/pbn/reference/save_results.md)
  writes its exclusion table to the element `exclusions`, with a column
  `data`. `cohort` is in Suggests.
- New argument `cache` of
  [`add_data()`](https://www.rwhite.no/pbn/reference/add_data.md). With
  `TRUE`,
  [`run_plan()`](https://www.rwhite.no/pbn/reference/run_plan.md) keeps
  the data of each scenario and simulation in `vNN_cache_gitexclude/`,
  and reads it again while the cache key is the same. It prints one line
  for each scenario and simulation, with the reason of each new
  computation.
- In a git repository,
  [`run_plan()`](https://www.rwhite.no/pbn/reference/run_plan.md) with a
  cache and
  [`save_results()`](https://www.rwhite.no/pbn/reference/save_results.md)
  with `gitexclude` stop until git ignores the files they write.
- `save_results(gitexclude = )` writes the kinds that it names to
  `results_gitexclude.qs2`, and deletes an old one when it names none.
  [`read_results()`](https://www.rwhite.no/pbn/reference/read_results.md)
  reads both files, and gives one warning when the second is missing.
- The manifest gains `data_cache`, and `manifest$analyses` gains the
  column `file`.
- [`add_analyses()`](https://www.rwhite.no/pbn/reference/add_analyses.md)
  stops on the kind name `exclusions`. The rule `exclusions_table`
  allows the column `data`.
- New
  [`cohort_data()`](https://www.rwhite.no/pbn/reference/cohort_data.md)
  and
  [`keep_exclusions()`](https://www.rwhite.no/pbn/reference/cohort_data.md),
  for a step after the cohort such as `data_cohort() |> data_impute()`.
  Its value MAY be any R object. For a list of data.frames with one
  number of rows, `n_rows_data` is the rows of one of them.
- The manifest gains `n_rows_before_exclusions`, the rows given to the
  cohort. `no_row_per_person` checks it too.
- `p$get_data()` and `p$run_one()` by hand use the cache. The cache line
  gives the date and the time.
- The cache key holds the md5 of the raw data at the time of the use,
  and [`RNGkind()`](https://rdrr.io/r/base/Random.html). A use stops
  when a `data_*.R` file changed after
  [`initialize_project()`](https://www.rwhite.no/pbn/reference/initialize_project.md).
  The key file records the versions of the loaded packages, and a new
  version computes the data again.
- [`run_plan()`](https://www.rwhite.no/pbn/reference/run_plan.md)
  computes the md5 of a changed raw data file again, and
  `manifest$data_md5` names the data that the run read.
- The manifest gains `n_rows_after_exclusions`, which
  `no_row_per_person` checks too.
- `element_names` passes a `results.qs2` with `manifest` only when the
  manifest puts kinds in `results_gitexclude.qs2`.
- New
  [`check_project()`](https://www.rwhite.no/pbn/reference/check_project.md)
  rule `gitexclude_not_tracked`. `results_contract` also checks each
  `results_gitexclude.qs2`, and `unknown_path` knows
  `vNN_cache_gitexclude/`.
- New
  [`expect_recovers()`](https://www.rwhite.no/pbn/reference/expect_recovers.md)
  checks the bias, the standard error and the coverage of an estimate
  over the simulations of a fake scenario. Each limit is 3.89 Monte
  Carlo standard errors, and each row shows the smallest error that it
  finds with a probability of 80%.
- New
  [`expect_biased()`](https://www.rwhite.no/pbn/reference/expect_biased.md),
  for a control analysis that MUST be biased. Both functions fail an
  assertion below 100 simulations, or 288 for the coverage of a 95%
  interval, and above 5% missing values. They also fail an `se` of 0 or
  less, a `lower` above its `upper`, and estimates that are all equal.
- New
  [`make_outputs()`](https://www.rwhite.no/pbn/reference/make_outputs.md)
  calls the output function of the project for `"real"` and for each
  fake scenario with `output = TRUE`. The fake output goes to
  `<scenario>/output_fake_data/` with the name prefix `fake_data_`, and
  pbn empties each folder first. It stops when the call for a fake
  scenario changes a file outside its folder. It deletes the output of a
  scenario whose checks failed.
- New
  [`stamp_fake()`](https://www.rwhite.no/pbn/reference/stamp_fake.md),
  which puts the label “FAKE DATA” on a ggplot of a fake scenario.
  ggplot2 is in Suggests, and grid in Imports.
- `CFG$scenarios` MAY have the column `output`, `TRUE` or `FALSE`, and
  `FALSE` for `"real"`. It is not a setting. Before, a column `output`
  was a setting of any atomic type.
- New
  [`check_project()`](https://www.rwhite.no/pbn/reference/check_project.md)
  rule `fake_data_names`.
- The article Get started has the scenarios `null` and `confounded`,
  with 140 simulations each and recovery checks, and an output part with
  [`make_outputs()`](https://www.rwhite.no/pbn/reference/make_outputs.md).
- [`add_data()`](https://www.rwhite.no/pbn/reference/add_data.md) takes
  the data as the steps `raw`, `clean`, `cohort` and `after_cohort`, or
  as one function `fn`. The data fails when `clean` changes the number
  of rows, or when the cohort does not start with every row. Each
  exclusion thus goes through the cohort, which counts it.
- pbn puts the exclusion table of the cohort on the value of
  `after_cohort`, so the step needs no
  [`keep_exclusions()`](https://www.rwhite.no/pbn/reference/cohort_data.md).
  [`keep_exclusions()`](https://www.rwhite.no/pbn/reference/cohort_data.md)
  is for a data function `fn`.
- With the steps, the cache key and `fake_md5` hold each step, and the
  cache line names a changed step. The article Get started uses the
  steps.
- The error of
  [`run_plan()`](https://www.rwhite.no/pbn/reference/run_plan.md) also
  names the first failed analysis and holds its error. pbn cuts it so
  that the whole error stays below 900 bytes. A knitr build keeps this
  error and loses the output of
  [`cat()`](https://rdrr.io/r/base/cat.html).
- [`run_plan()`](https://www.rwhite.no/pbn/reference/run_plan.md) with
  workers starts each worker with the library paths of this R session,
  through `R_LIBS`. Before, a worker did not find a library that the
  session added with
  [`.libPaths()`](https://rdrr.io/r/base/libPaths.html), for example
  that of `R CMD build` on Windows.
- git gets the stacked `<scenario>/results.qs2` only.
  [`save_results()`](https://www.rwhite.no/pbn/reference/save_results.md)
  writes the results of each simulation to
  `<scenario>/sims_gitexclude/sim_<kkkk>.qs2`, and in a git repository
  it stops until git ignores that folder.
- [`save_results()`](https://www.rwhite.no/pbn/reference/save_results.md)
  deletes the folders `sim_<kkkk>/` of the old layout of a scenario, and
  prints one line.
- [`save_results()`](https://www.rwhite.no/pbn/reference/save_results.md)
  empties the folder `sims_gitexclude/` of a scenario before it writes
  it, and deletes it for a scenario with 1 simulation. It prints one
  line when it deleted a file. Before, a stale simulation stayed, and
  `read_results(sim = )` read it.
- In a git repository,
  [`save_results()`](https://www.rwhite.no/pbn/reference/save_results.md)
  checks each file that it writes into `sims_gitexclude/`, with one call
  of git, and names the first 3 that git does not ignore.
- `read_results(sim = k)` reads `sims_gitexclude/sim_<kkkk>.qs2`.
  Without that file, for example in a clone, it stops and says to run
  the scenario.
- `results_contract` checks each file of `sims_gitexclude/` on the disk,
  and needs none in a clone.
- [`save_results()`](https://www.rwhite.no/pbn/reference/save_results.md)
  prints each failed assertion with
  [`cat()`](https://rdrr.io/r/base/cat.html). Its error gives the count,
  the scenarios and the path of `check.csv`, because R cut the list at
  1000 bytes.
- `no_row_per_person` checks stacked results for each simulation, with
  the new `manifest$n_rows_by_sim`. Before, it checked nothing on
  stacked results.
- New argument `label` of
  [`expect_recovers()`](https://www.rwhite.no/pbn/reference/expect_recovers.md)
  and
  [`expect_biased()`](https://www.rwhite.no/pbn/reference/expect_biased.md).
  The text of the bias and coverage assertions names the truth, for
  example “bias of estimate against 0.673”.
- A kind whose analyses each return a list of data.frames with the same
  names is a named list of row-bound data.frames, for example
  `results$sem$estimates`. This changes the shape of such a kind in
  results that this unreleased version wrote, where it was
  `results$sem$sem__1` and so on.
- `manifest$packages` also names the packages that the workers loaded,
  for example the package of a model.
- New `get_data(p, scenario, sim)` and
  `get_argset(p, scenario, sim, name, i)` give the data and the argset
  of one analysis without its name. An example is
  `pbn::get_data(p, "real")$d`. `no_local_copy_of_pbn_function` allows a
  project function `get_data`.
- New argument `coverage` of
  [`expect_recovers()`](https://www.rwhite.no/pbn/reference/expect_recovers.md).
  With `FALSE`, it makes no coverage assertion, needs no `lower` and
  `upper`, and needs 100 simulations.
- `read_results(sim = 1)` reads the `results.qs2` of a scenario with 1
  simulation.
- On Windows, in a git repository,
  [`run_plan()`](https://www.rwhite.no/pbn/reference/run_plan.md) and
  [`save_results()`](https://www.rwhite.no/pbn/reference/save_results.md)
  stopped even when `.gitignore` held `*gitexclude*`. pbn now gives
  `git check-ignore` its paths with LF line ends, because git reads the
  CR of CR LF as part of a path.
- [`save_results()`](https://www.rwhite.no/pbn/reference/save_results.md)
  writes the run log `vNN_results/run.md`. It holds the result, the
  failures first, the scenarios, the data, the exclusions, the
  assertions, the files and the timings. It also writes it when it stops
  on a failed assertion or a failed rule of
  [`check_results()`](https://www.rwhite.no/pbn/reference/check_results.md).
- Only the sections When and Timings of the run log change in every run.
  So the log goes to git, and `git diff` shows the real changes of a
  run.
- [`run_plan()`](https://www.rwhite.no/pbn/reference/run_plan.md)
  measures each data step and each analysis where it runs, also on a
  worker. The timings go to the run log, and not to the value of
  [`run_plan()`](https://www.rwhite.no/pbn/reference/run_plan.md), a
  results file or a cache key.
- [`run_plan()`](https://www.rwhite.no/pbn/reference/run_plan.md) writes
  the run log with the failed analyses when it stops on them after
  [`initialize_project()`](https://www.rwhite.no/pbn/reference/initialize_project.md).
  At its start, it replaces the run log with one that says the run has
  not finished.
- The run log never holds the text of an error. It numbers each error,
  and `vNN_results/run_errors_gitexclude.md` holds the text when git
  ignores that file.
- [`save_results()`](https://www.rwhite.no/pbn/reference/save_results.md)
  writes the run log as failed when it stops in the checks of its
  arguments.
- [`make_outputs()`](https://www.rwhite.no/pbn/reference/make_outputs.md)
  adds its files and its seconds to the run log of
  [`save_results()`](https://www.rwhite.no/pbn/reference/save_results.md)
  in the same R session.
- New
  [`render_run()`](https://www.rwhite.no/pbn/reference/render_run.md)
  renders the run log to `run.pdf` with Quarto and its Typst format,
  without LaTeX. A run never calls it. It writes the PDF to a temporary
  file and renames it.
- `unknown_path` reads the files directly in `vNN_results/`, and knows
  `run.md`, `run.pdf` and `run_errors_gitexclude.md` there.
- `get_data(p, scenario, sim)` works directly after
  [`add_data()`](https://www.rwhite.no/pbn/reference/add_data.md). It
  takes the scenarios of the last
  [`scenarios()`](https://www.rwhite.no/pbn/reference/scenarios.md)
  before
  [`add_data()`](https://www.rwhite.no/pbn/reference/add_data.md), which
  the plan keeps.
- [`save_results()`](https://www.rwhite.no/pbn/reference/save_results.md)
  and
  [`make_outputs()`](https://www.rwhite.no/pbn/reference/make_outputs.md)
  stop when a file that they delete stays, for example because another
  program holds it open on Windows. The error names the file.
- [`make_outputs()`](https://www.rwhite.no/pbn/reference/make_outputs.md)
  compares the new names of the files of a fake scenario in lower case,
  as Windows and macOS do.
- pbn passes R CMD check on Windows (R-hub, R-devel).
- [`run_plan()`](https://www.rwhite.no/pbn/reference/run_plan.md) with
  workers now runs the analyses through doFuture, and no longer through
  doParallel. The future plan is
  [`future::cluster`](https://future.futureverse.org/reference/cluster.html),
  on the PSOCK cluster that pbn makes. doParallel does not relay the
  progress of a worker to this R session, and a future does. future and
  doFuture replace doParallel in Suggests.
- A future empties the global environment of its worker. So with
  workers, each batch of analyses takes the objects of the functions
  folder to its worker. The output, the messages and the warnings of a
  worker now reach this R session. A worker has no option `mc.cores`, as
  before.
- [`run_plan()`](https://www.rwhite.no/pbn/reference/run_plan.md) shows
  one line for each scenario, drawn again in place in a console. A log
  gets no line in the first 60 seconds. Then it gets one when a scenario
  finishes, and at most one every 60 seconds for a running scenario. It
  replaces the progress bar of plnr. pbn imports progressr.
- New `progress(done, total, label)` shows the progress of a long
  analysis on the line of its scenario. It does nothing outside
  [`run_plan()`](https://www.rwhite.no/pbn/reference/run_plan.md) and in
  a forked child process.
- [`run_plan()`](https://www.rwhite.no/pbn/reference/run_plan.md) fails
  an analysis that changes an object of the functions folder. An example
  is the index that a data.table subset `CFG$table[x == 1]` adds in
  place.
  [`initialize_project()`](https://www.rwhite.no/pbn/reference/initialize_project.md)
  records these objects.
- The rule `no_local_copy_of_pbn_function` also fails a function
  `progress` of a project.
- [`progress()`](https://www.rwhite.no/pbn/reference/progress.md) checks
  its arguments at each call, also within 0.5 seconds of a call that it
  showed.
- An error outside an analysis, for example of a worker that stops, no
  longer ends
  [`run_plan()`](https://www.rwhite.no/pbn/reference/run_plan.md) with a
  raw error of future.
  [`run_plan()`](https://www.rwhite.no/pbn/reference/run_plan.md) writes
  the run log with the error, sets the backend back, and stops with one
  message.
- An analysis fails when it leaves a new object in the global
  environment. A data function fails when it changes an object of the
  functions folder.
- The data functions and the analyses set the kind of random number
  generator of the run before each seed. An analysis that calls
  [`RNGkind()`](https://rdrr.io/r/base/Random.html) then changes no
  later draw.
- [`run_plan()`](https://www.rwhite.no/pbn/reference/run_plan.md) and
  [`save_results()`](https://www.rwhite.no/pbn/reference/save_results.md)
  read the argsets of a plan in one pass. Before, the time grew with the
  square of the number of analyses.
- [`run_plan()`](https://www.rwhite.no/pbn/reference/run_plan.md) stops
  the run at its first failed analysis, because pbn saves nothing from a
  run with a failure. No new data function or analysis starts, and the
  error and the run log give the number of analyses that did not run.
  Before, every analysis ran.
- With workers, a worker sees the stop before its next data function, or
  within 0.5 seconds before its next analysis. pbn does not stop an
  analysis that runs.
- The display counts the failed analyses of each scenario, and the time
  left comes from the analyses without a failure. A scenario whose data
  failed shows its time. The header and each line of a log give the time
  since the start of the run.
- The section Timings of the run log gives seconds on the clock only,
  and no longer sums seconds over the workers. It gives the wall seconds
  of each scenario. Each data step and analysis kind gets its calls, the
  median and the maximum of one call, and its share.
- The run log ends with the section History, one line for each run of
  this version, also for a run that did not finish. A deleted `run.md`
  starts the history again.
- The check of the objects of the functions folder names the first
  element that changed, for example `CFG$vars`, and how. The output
  function of
  [`make_outputs()`](https://www.rwhite.no/pbn/reference/make_outputs.md)
  and each `expect_` function get the same check.
- The section Run gives the number of the other packages. A cohort
  without an exclusion step gets one sentence. The section Output gives
  the number of warnings of the output function. The row of dashes of a
  table is as wide as its column.
- New
  [`check_project()`](https://www.rwhite.no/pbn/reference/check_project.md)
  rule `cyclomatic_complexity`: a function of the project MUST score at
  most 15 in cyclocomp. pbn imports cyclocomp.
- New
  [`check_project()`](https://www.rwhite.no/pbn/reference/check_project.md)
  rule `no_local_assign`: an `.R` file of the project MUST NOT call
  [`local()`](https://rdrr.io/r/base/eval.html),
  [`assign()`](https://rdrr.io/r/base/assign.html),
  [`delayedAssign()`](https://rdrr.io/r/base/delayedAssign.html),
  [`assignInNamespace()`](https://rdrr.io/r/utils/getFromNamespace.html)
  or
  [`assignInMyNamespace()`](https://rdrr.io/r/utils/getFromNamespace.html),
  or use `<<-` or `->>`.
- [`check_project()`](https://www.rwhite.no/pbn/reference/check_project.md)
  reads its rules from the file `pbn.yml` in the project folder, and
  each project MUST have it. Its one line `extends: house_2026_11_17`
  names a dated house rule set, which pbn never changes after its
  release. So a new pbn does not change the rules of a project.
- `pbn.yml` MAY set the level of a rule to `fail`, `warn` or `off`, and
  MAY change `limit` of `loc_per_file` and `cyclomatic_complexity` and
  `forbid` of `no_local_assign`. It lists exceptions under `allow:`. It
  cannot weaken `results_contract`, `gitexclude_not_tracked` or
  `fake_data_names`.
- [`check_project()`](https://www.rwhite.no/pbn/reference/check_project.md)
  prints the rule set on its first line, OFF for a rule that is off, and
  the count of OFF on its last line.
- pbn reads no `pbn_allow.txt`, a change from the earlier builds of this
  version.
  [`check_project()`](https://www.rwhite.no/pbn/reference/check_project.md)
  stops on the file and prints its lines as the YAML of `allow:`. Paste
  them into `pbn.yml`, and delete `pbn_allow.txt`.
- [`initialize_project()`](https://www.rwhite.no/pbn/reference/initialize_project.md)
  stops without `pbn.yml`. It gives org the `limit` of `loc_per_file`,
  or `Inf` when the rule is `warn` or `off`. pbn imports `yaml`.
- [`initialize_project()`](https://www.rwhite.no/pbn/reference/initialize_project.md)
  takes `folders`, the named folders that the project reads, such as
  `c(main = "../data", lookup = "../codes")`, in the place of
  `data_raw`. One folder without a name is `main`. A call with
  `data_raw =` stops and gives the new argument.
- New `folder(name, ...)` gives the path of a folder of the project, or
  of a path inside it, such as `pbn::folder("main", "sub", "d.csv")`,
  also on a worker of
  [`run_plan()`](https://www.rwhite.no/pbn/reference/run_plan.md). A
  part with a leading `/`, a drive or `..` stops. pbn gives org no
  folder, so `org::project$data_raw` is gone.
- pbn reads no content of a data file. `manifest$data_files`, in the
  place of `data_md5`, lists each file under each folder at any depth,
  as `<folder>/<path>`, with its size and its modification time in whole
  seconds. A file that changed in place and kept its size and that
  second is not seen.
- The cache key holds that fingerprint and no path. So the same files in
  another place, copied with their modification times, give the same key
  and the same results. A changed, new or removed file gives the cache
  line “the raw data in the folder lookup changed”.
- The rule `manifest_data` of
  [`check_results()`](https://www.rwhite.no/pbn/reference/check_results.md)
  needs `data_files` for `"real"`, and fails a manifest with `data_md5`.
- [`initialize_project()`](https://www.rwhite.no/pbn/reference/initialize_project.md)
  stops on a folder inside another, on a folder that holds or is inside
  the results or the cache folder, and on a broken link.
- The run log names the folders of the scenario `real` with their number
  of files. It holds no absolute path.
- New
  [`check_project()`](https://www.rwhite.no/pbn/reference/check_project.md)
  rule `no_absolute_paths`: no string of an `.R` file is an absolute
  path, and no [`file.path()`](https://rdrr.io/r/base/file.path.html) or
  [`fs::path()`](https://fs.r-lib.org/reference/path.html) starts from a
  string, outside the arguments of
  [`pbn::initialize_project()`](https://www.rwhite.no/pbn/reference/initialize_project.md)
  and
  [`pbn::to_one_file()`](https://www.rwhite.no/pbn/reference/to_one_file.md).
- New
  [`check_project()`](https://www.rwhite.no/pbn/reference/check_project.md)
  rule `blocks_list`: the object `blocks` of the functions folder is a
  list of named functions.
- The set `house_2026_11_17` gets both new rules, and its md5 changes.
  It had no release yet. After the first release of pbn, a new rule
  needs a new dated set.
- `run_script_initialises` does not count a call with an argument that
  [`initialize_project()`](https://www.rwhite.no/pbn/reference/initialize_project.md)
  does not have.
- New
  [`to_one_file()`](https://www.rwhite.no/pbn/reference/to_one_file.md)
  writes the run of the real data as one R script, `vNN_one_file.R` in
  the project of the session, for a server without a network. It stops
  on kept code that uses a left-out `expect_` function, and on a file
  that does not parse.
  [`initialize_project()`](https://www.rwhite.no/pbn/reference/initialize_project.md)
  gains the argument `one_file` for that script.
- New
  [`check_one_file()`](https://www.rwhite.no/pbn/reference/check_one_file.md)
  runs that script on copies of the folders and compares its real
  results with those of the project. When the run stops, it prints the
  exit status and the last 20 lines of its output.
- `unknown_path` knows `vNN_one_file.R`, and the code rules do not read
  it.
- New
  [`show_blocks()`](https://www.rwhite.no/pbn/reference/show_blocks.md)
  and
  [`show_block()`](https://www.rwhite.no/pbn/reference/show_blocks.md)
  print the blocks of the manuscript one page at a time, from the real
  results on the disk. They set back each option that a block function
  sets.
- [`to_one_file()`](https://www.rwhite.no/pbn/reference/to_one_file.md)
  writes an ASCII file from code that is not ASCII. Each such character
  of a string becomes its escape `\uXXXX`, and of a comment the text
  `<U+XXXX>`. The code of the file is
  [`identical()`](https://rdrr.io/r/base/identical.html) to that of the
  project. A name that is not ASCII stops. Before, any such byte
  stopped.
- [`to_one_file()`](https://www.rwhite.no/pbn/reference/to_one_file.md)
  drops the comment lines directly above a left-out `expect_` function.
  It stops with the file and the line on a string that is not valid
  UTF-8 after its escapes.
- `manifest$source_md5` leaves out `vNN_functions/blocks.R`, so a change
  of a block keeps the results current.
  [`check_one_file()`](https://www.rwhite.no/pbn/reference/check_one_file.md)
  leaves it out too, also for older results.
- The rule `blocks_list` needs `blocks` in `vNN_functions/blocks.R`
  only. It fails a top-level expression of that file other than an
  assignment of a name to `function(...)` or of `blocks` to `list(...)`.
  It also fails a name that another file assigns too, and a use of one
  of its names in the other code. Its detail gives the file and the
  line.
- [`initialize_project()`](https://www.rwhite.no/pbn/reference/initialize_project.md)
  leaves `blocks.R` out of `source_md5` only when it passes these
  checks, also when the rule is off.
- [`save_results()`](https://www.rwhite.no/pbn/reference/save_results.md)
  deletes `output/` and `output_fake_data/` of each scenario that it
  writes, with one printed line. The run log then says that the run made
  no output until
  [`make_outputs()`](https://www.rwhite.no/pbn/reference/make_outputs.md)
  runs.
- [`check_one_file()`](https://www.rwhite.no/pbn/reference/check_one_file.md)
  runs each block on both real results, in a new R session without the
  packages of the run script. It fails a block that stops or prints
  other text. Its PASS line counts the blocks.
- [`check_one_file()`](https://www.rwhite.no/pbn/reference/check_one_file.md)
  says that the results are older than the code, and runs nothing, when
  a file of the project changed after the run. Without results, it runs
  the checks of
  [`to_one_file()`](https://www.rwhite.no/pbn/reference/to_one_file.md)
  and stops.
- A block that calls a function that R does not find stops with the hint
  to name its package.
  [`show_block()`](https://www.rwhite.no/pbn/reference/show_blocks.md)
  attaches no package.
- An entry of `allow:` in `pbn.yml` covers its path and each path under
  it, with or without a final `/`. Before, a folder entry covered the
  folder only. An entry `/`, `.`, a path from `/` or a path with `..`
  stops.
- `run_plan(workers > 1)` starts each worker with `OMP_NUM_THREADS`,
  `OPENBLAS_NUM_THREADS` and `MKL_NUM_THREADS` at 1, and calls
  `data.table::setDTthreads(1)` there. With 1 worker, nothing changes.
- In a log, each line of the display gives the time of its event, for
  example the end of a scenario. The lines come in that order. Before,
  each line had the time of the first line after 60 seconds.
- [`stamp_fake()`](https://www.rwhite.no/pbn/reference/stamp_fake.md)
  puts one label over the whole figure of a patchwork. Before, the label
  was on its last panel. patchwork is in Suggests.
- A setting column of `CFG$scenarios` MAY have `NA` for `"real"`.
  `"real"` then has no such setting in its argsets, its cache key and
  its manifest.
- The manifest gains `n_rows_lists`, the rows of each data.frame of a
  list of data.frames with 2 or more numbers of rows.
  `no_row_per_person` checks each of them.
- New argument `fake_real_data` of
  [`initialize_project()`](https://www.rwhite.no/pbn/reference/initialize_project.md),
  for folders that hold fake files in the formats of the real data. The
  real manifest gains `fake_real_data`, and `manifest_data` fails a real
  manifest without it. The run log,
  [`make_outputs()`](https://www.rwhite.no/pbn/reference/make_outputs.md),
  [`stamp_fake()`](https://www.rwhite.no/pbn/reference/stamp_fake.md)
  and the blocks label such results. All but the run log read the flag
  from the manifest of the real results, never from the R session. The
  file of
  [`to_one_file()`](https://www.rwhite.no/pbn/reference/to_one_file.md)
  never has the argument.
- New
  [`check_project()`](https://www.rwhite.no/pbn/reference/check_project.md)
  rule `real_results_are_fake`, a warning while the real results on the
  disk come from fake real data. The set `house_2026_11_17` gets it and
  the new `blocks_list`, and its md5 changes before the first release.
- [`stamp_fake()`](https://www.rwhite.no/pbn/reference/stamp_fake.md)
  keeps every panel of a patchwork. Before, the label covered the last
  plot with an opaque background, so a figure `a / b` lost `b`. It now
  wraps the whole patchwork with
  [`patchwork::wrap_elements()`](https://patchwork.data-imaginist.com/reference/wrap_elements.html)
  and puts the label on it without a background.
- [`check_one_file()`](https://www.rwhite.no/pbn/reference/check_one_file.md)
  first compares `<version>_one_file.R` in the project with the file of
  [`to_one_file()`](https://www.rwhite.no/pbn/reference/to_one_file.md)
  now, with the paths of the server from that file. It prints FAIL with
  the first line that differs, or when there is no such file. Then it
  runs the file of the project, with temporary paths in place of the
  paths of the server. Before, it ran a new file and passed while the
  file in git was older.
- [`to_one_file()`](https://www.rwhite.no/pbn/reference/to_one_file.md)
  changes only the characters that are not ASCII in a string, and keeps
  its quotes, escapes and line breaks. Before, it wrote each such string
  again from its value, so a multi-line string became one long line. A
  raw string, and a string with an escape `\x` or an octal escape, still
  come from the value.
- [`stamp_fake()`](https://www.rwhite.no/pbn/reference/stamp_fake.md)
  puts one label over the whole figure of a ggplot too, also with
  facets. Before, each panel of a faceted figure clipped its own copy of
  the label. ggplot2 and patchwork move from Suggests to Imports.
- The FAIL of
  [`check_one_file()`](https://www.rwhite.no/pbn/reference/check_one_file.md)
  for an old one file cuts each quoted line to 80 characters, with ”
  \[…\]“.
- [`initialize_project()`](https://www.rwhite.no/pbn/reference/initialize_project.md)
  gains `one_file_home` and `one_file_folders`, the paths on the server
  for
  [`to_one_file()`](https://www.rwhite.no/pbn/reference/to_one_file.md).
  Give both or neither, with the folder names of `folders`.
- [`to_one_file()`](https://www.rwhite.no/pbn/reference/to_one_file.md)
  and
  [`check_one_file()`](https://www.rwhite.no/pbn/reference/check_one_file.md)
  take no argument: the version and the paths of the server come from
  the last
  [`initialize_project()`](https://www.rwhite.no/pbn/reference/initialize_project.md).
  A call with the old arguments stops, and so does a call without the
  paths. This changes the unreleased
  `to_one_file(version, home, folders)` and `check_one_file(version)`.
- The argument `one_file` of
  [`initialize_project()`](https://www.rwhite.no/pbn/reference/initialize_project.md)
  in the one file is now `generated`. Generate each one file again with
  [`to_one_file()`](https://www.rwhite.no/pbn/reference/to_one_file.md).
  [`check_one_file()`](https://www.rwhite.no/pbn/reference/check_one_file.md)
  says when an older pbn wrote the file.
- [`check_one_file()`](https://www.rwhite.no/pbn/reference/check_one_file.md)
  takes the paths of the server from the R session, not from the one
  file. A file with other paths is older than the run script.
- The one file leaves out each line that holds only a comment
  `# pbn::to_one_file()` or `# pbn::check_one_file()`.
- `manifest$source_md5` leaves out the paths of the server, when they
  are literal strings, and those two comment lines of the run script. So
  a run script that gains them keeps its results current.
  [`?initialize_project`](https://www.rwhite.no/pbn/reference/initialize_project.md)
  gives the rule.
- `no_absolute_paths` no longer exempts the arguments of
  [`pbn::to_one_file()`](https://www.rwhite.no/pbn/reference/to_one_file.md).

## pbn 2026.11.22

- `save_results(..., d)` saves each named argument as one result under
  its name. A call with the names `exclusions`, `estimates`, `tables`
  and `extra` works as before.
- [`save_results()`](https://www.rwhite.no/pbn/reference/save_results.md)
  stops on a result named `manifest`, an unnamed result, a repeated or
  non-syntactic name, and a call with no result.
- `d` is required, so a call of
  [`save_results()`](https://www.rwhite.no/pbn/reference/save_results.md)
  without `d` stops. Pass `d = d`, or `d = NULL` when the run has no
  single data table. Before, `d` was `NULL` by default.
- `exclusions` is optional. When it is there, the rule
  `exclusions_table` checks it as before. `estimates` no longer has to
  be a data.frame, and `tables` no longer has to be a list of
  data.frames.
- A result MAY be a data.frame, an atomic vector, `NULL`, or a list of
  these to any depth. The new rule `allowed_objects` replaces
  `no_fitted_models`. It also fails a function, an environment, an S4 or
  R6 object, a formula and a list with a class outside a fixed set. The
  detail names the path and the class.
- The new rule `element_names` replaces `top_level_names`. The rules
  `exclusions_table`, `no_row_per_person`, `manifest_scenario`,
  `manifest_data` and `manifest_source` keep their names.
- `run_scenarios()` stacks every element of the results. Before, it
  dropped `extra`. A data.frame gets a first column `sim`, and a list is
  stacked element by element. Each other value becomes a list with one
  entry for each simulation.
- Stacking stops when two simulations differ in the structure at one
  path, and the error names the path and the two simulations.
- `no_row_per_person` also fails an atomic vector of length
  `n_rows_data` and a matrix or an array with a dimension `n_rows_data`,
  for example `fitted(fit)`. It does not look in `manifest`, and it
  checks nothing when `n_rows_data` is 0 or 1. Before, a 1-row table
  failed when the data had 1 row.
- `allowed_objects` and `no_row_per_person` also walk the attributes of
  each object, so the `terms` attribute of a model frame fails. They
  skip the attributes that R and data.table set, for example `names` and
  `.internal.selfref`.
- The detail of `allowed_objects` gives a fix for a model, a formula, an
  `htest`, a `POSIXlt`, a `package_version` and an `sf` geometry.
- Stacking stops on a nested list that is unnamed or partly named, and
  on `NULL` in the place of a value. It also stops on a data.frame
  column of another class, a factor column with other levels, and a
  data.frame with a column `sim`.
- A results file that pbn 2026.11.21 wrote passes the new rules, unless
  it holds an object that `allowed_objects` or `no_row_per_person`
  fails.

## pbn 2026.11.21

- `run_scenarios()` writes the stdout and the stderr of each run to
  `run.log` in the results folder of the simulation. For a failed run,
  it prints the whole log, and the error names the run and the log.
  Before, the output was in the error message, and R cuts an error
  message at 1000 bytes by default.
- New argument `workers` of `run_scenarios()`: the number of simulations
  that run at the same time, each in a new R session. After a failed
  run, no new run starts, the started runs end, and the error names each
  failed run.
- New argument `resume` of `run_scenarios()`. With `TRUE`, it keeps each
  simulation with a `results.qs2` from a completed run of the same run
  script and functions folder, and runs the others. The default `FALSE`
  runs every simulation.
- After a run that succeeds, `run_scenarios()` writes `run.ok` with the
  md5 of `results.qs2`. A run that quits with status 0 but writes no
  valid `results.qs2` for its scenario and simulation now fails. Before,
  a scenario with 1 simulation and no `expect_` function could pass with
  an invalid file.
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
- `run_scenarios()` deletes the `check.csv` of each chosen scenario and
  each stacked `results.qs2` before the first run, and the `results.qs2`
  of each simulation that it runs.
- pbn no longer imports `plnr`. `run_scenarios()` starts and watches its
  R sessions with `callr`.

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
- `run_scenarios()` keeps the class of each stacked table, so a
  data.table in simulation 1 gives a stacked data.table. Before, every
  stacked table was a data.frame.
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
  new `use_scenario()`, or else `"real"`.
- New `use_scenario(scenario, sim = 1)` chooses the scenario for the
  rest of the R session when a person steps through a run script by
  hand. It stops in a non-interactive R session.
- In an interactive R session,
  [`initialize_project()`](https://www.rwhite.no/pbn/reference/initialize_project.md)
  prints one line with the scenario and the simulation, and `(fake)` for
  a fake scenario. A call that stops leaves no run for
  [`save_results()`](https://www.rwhite.no/pbn/reference/save_results.md)
  or `end_if_fake_scenario()`.
- New `end_if_fake_scenario()` ends a run script after
  [`save_results()`](https://www.rwhite.no/pbn/reference/save_results.md)
  for a fake scenario, so the output part runs for the real data only.
  It quits with status 0 under `Rscript`, and `run_scenarios()` counts
  that as a success.
- New
  [`check_project()`](https://www.rwhite.no/pbn/reference/check_project.md)
  rule `no_active_use_scenario`. It fails an active call or the
  reference `pbn::use_scenario`, also in a function body or an `if`.
  `run_script_initialises` names each argument that
  [`initialize_project()`](https://www.rwhite.no/pbn/reference/initialize_project.md)
  does not have.
- `unknown_path` knows `run_scenarios.R`. It no longer knows
  `vNN_output.R`: the output part goes in `vNN_run.R`, after
  `end_if_fake_scenario()`.

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
- New `run_scenarios()` runs every simulation of every scenario in a new
  R session with a `plnr` plan, stacks the simulations, and checks them
  with `expect_<scenario>()`.
- `run_scenarios()` needs `expect_<scenario>()` for every fake scenario,
  and stops before any run when one is missing. An `expect_` function
  that errors or returns no assertions counts as a failed assertion.
- `run_scenarios()` deletes the results of a simulation before it runs
  it, and stops when the run writes no `results.qs2`. An older file is
  never stacked or checked.
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
