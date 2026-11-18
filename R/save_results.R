#' Save the results of a run
#'
#' `save_results()` writes one results object for each scenario and
#' simulation of a run, and stacks the simulations. It checks the fake
#' scenarios with their `expect_` functions, and writes no results file when
#' an object fails [check_results()].
#'
#' @param r The value of [run_plan()] for the plan `p`.
#' @param p The plan, with data from [add_data()] and analyses from
#'   [add_analyses()].
#' @param scenarios The scenarios of the run, from [scenarios()]. They MUST
#'   be the scenarios of the analyses of `p`.
#' @param gitexclude `NULL`, or the names of the analysis kinds whose
#'   results hold data for each person. See the section Two files.
#' @details
#' Call [initialize_project()] first. For each scenario and simulation,
#' `save_results()` builds one results object, a named list. Its first
#' element is `manifest`. When a data function returned a
#' `cohort::CohortPipeline`, the next element is `exclusions`, the exclusion
#' tables of the cohorts row-bound in the order of [add_data()]. The column
#' `data` names the data of each row. The section A cohort as the data of
#' [add_data()] gives the columns. A plan without a cohort has no element
#' `exclusions`. Then comes one element for each analysis kind of
#' [add_analyses()], under the kind name and in the order of the calls. The
#' scenario `"real"` has no element of a kind with `control = TRUE`. The
#' element of a kind holds the values of its analyses for that scenario and
#' simulation, in the order of the plan:
#' - When each value is a data.frame, the element is the values row-bound.
#'   Each extra dimension of the kind comes first as a column, unless the
#'   data.frame has a column of that name already. A data.table stays a
#'   data.table. A kind without extra dimensions has the data.frame of its one
#'   analysis.
#' - When each value is a list of data.frames with the same names, each name
#'   once, the element is a named list with these names. Each of its data.frames is the data.frames of that name
#'   row-bound, with the rule of a data.frame above. An example is
#'   `results$sem$estimates` and `results$sem$fit`.
#' - Otherwise, a kind without extra dimensions has the value of its one
#'   analysis.
#' - Otherwise, the element is a named list with one entry for each
#'   analysis. Each name is `<name>__<i>`, the analysis name without the
#'   scenario and the simulation. So the names are the same in each
#'   simulation. [add_analyses()] gives the order of `<i>`.
#'
#' The `manifest` is a list with these elements:
#' - `pbn_version` and `r_version`.
#' - `packages`, a named character vector. The names are the attached and
#'   loaded packages of this R session that are not base packages, and the
#'   values are their versions. `qs2` is always in the list, because it writes
#'   the file. With 2 or more workers of [run_plan()], the list also holds
#'   the packages loaded on a worker at the end of the run. An example is the
#'   package of a model. With 2 or more workers, this R session also loads
#'   doFuture and the packages that it imports, such as future, so the list
#'   then holds these too.
#' - `run_date`, the date of the call as `YYYY-MM-DD`. It has no time, so two
#'   runs on one day with the same inputs and packages write the same bytes.
#' - `version`, `scenario`, `sim` and `sims`.
#' - `settings`, the named list of the setting columns of the scenario. It is
#'   an empty list when there are none.
#' - `source_md5`, the md5 of `<version>_run.R` and of each file in
#'   `<version>_functions/` at any depth, also a file that is not an `.R`
#'   file, named by the path relative to `home`. The one file that it MAY
#'   leave out is `<version>_functions/blocks.R`, the blocks of
#'   [show_block()]. [initialize_project()] leaves it out only when it
#'   cannot change the results, which is when each of these holds:
#'   - it parses.
#'   - each top-level expression assigns with `<-` or `=` a name to
#'     `function(...)`, or `blocks` to `list(...)`.
#'   - no other file of the folder assigns one of its names.
#'   - no other file and no run script uses one of its names.
#'
#'   Otherwise
#'   `source_md5` holds it, also when the rule `blocks_list` of
#'   [check_project()] is `warn` or `off`. A change of a blocks file of that
#'   shape thus keeps the results current. [initialize_project()] computes the md5 before it
#'   sources the files. The cache key of [add_data()] holds the md5 of the
#'   files `data_*.R` only, so it never held `blocks.R`. The rule
#'   `manifest_source` needs the run script in `source_md5`, so
#'   `save_results()` stops when `<version>_run.R` did not exist.
#' - For the scenario `"real"` only, `data_files`, the fingerprint of the
#'   files under the folders of `folders` of [initialize_project()] when
#'   [run_plan()] started. It is a data.frame with the columns `file`, `size`
#'   and `mtime`. Each `file` is `<folder name>/<path in the folder>`, for
#'   example `main/sub/d.csv`, and never the path of the folder. pbn reads no
#'   content of a data file. The section The fingerprint of the data of
#'   [initialize_project()] gives the rules. So the manifest names the data
#'   that the run read, also after a change of a file since
#'   [initialize_project()]. The same files in another place, with the same
#'   modification times, give the same manifest.
#'   `save_results()` stops for `"real"` when a folder did not exist when
#'   [initialize_project()] ran.
#' - For the scenario `"real"` only, `fake_real_data`, `TRUE` or `FALSE`:
#'   the argument `fake_real_data` of [initialize_project()].
#' - For a fake scenario only, `fake_md5`, one md5 of the code of the data
#'   functions of [add_data()], without their source references: each `fn`,
#'   and each step of the steps form. A change in a function that `fn` or a
#'   step calls does not change `fake_md5`. `source_md5` holds the md5 of
#'   the files of those functions.
#' - `n_rows_data`, a named integer vector with the number of rows of each
#'   data element. A list of data.frames with one number of rows, for
#'   example a list of imputed data sets, has the rows of one of them. Each
#'   other element that is not a data.frame has `NA`.
#' - `n_rows_lists`, a named list with one entry for each data element that
#'   is a list of data.frames with 2 or more numbers of rows. The entry is
#'   an integer vector with the rows of each data.frame, in the order of the
#'   list. Such an element has `NA` in `n_rows_data`. The list is empty when
#'   no data element is such a list. The rule `no_row_per_person` checks
#'   each of these numbers. The
#'   analysis `<scenario>__sim_<kkkk>__pbn_n_rows` of [add_analyses()] counts
#'   them where the data is, also on a worker. The rule `no_row_per_person`
#'   reads it.
#' - `n_rows_before_exclusions`, a named integer vector with the number of
#'   rows given to the cohort of each data element, or `NA` for data without
#'   a cohort. `no_row_per_person` reads it too.
#' - `n_rows_after_exclusions`, the same for the rows after the exclusions:
#'   `n_after` of the last step of the exclusion table. It has a number also
#'   when `n_rows_data` is `NA`, for example for a `mids` object.
#' - `analyses`, a data.frame with the columns `analysis`, `control` and
#'   `file`: the analysis kinds in the object, and the file of each,
#'   `"results.qs2"` or `"results_gitexclude.qs2"`.
#' - `data_cache`, a data.frame with one row for each data element of
#'   [add_data()] and the columns `data`, `cache`, `from_cache` and
#'   `cache_key`. `cache` is the argument of [add_data()]. `from_cache` is
#'   `TRUE` when [run_plan()] read the data from the cache. `cache_key` is the
#'   md5 of the cache key, or `NA` without a cache.
#'
#' `save_results()` does these steps in this order. It stops at the first step
#' that fails:
#' 1. It checks its arguments. It stops when `r` has no stamp of
#'    [run_plan()] for `p`. It stops when the settings of `scenarios` are not
#'    the settings in the argsets of `p`. It stops when a fake scenario has no function
#'    `expect_<scenario>(results)` in the environment of
#'    [initialize_project()], or when an analysis of `r` failed. In a git
#'    repository, it stops when git does not ignore each file that it would
#'    write into a folder `sims_gitexclude/`. The error names the first 3 of
#'    the files that git does not ignore. It writes no results file then,
#'    and it writes the run log with its error.
#' 2. It builds every results object, and the stacked object of each scenario
#'    with more than 1 simulation, and checks each with [check_results()]. On
#'    a failure, it prints each failed rule with `cat()`, writes no results
#'    file and stops.
#' 3. For each scenario, it replaces `check.csv` with one failed assertion,
#'    `not checked yet`, so that [read_results()] refuses the new results
#'    until step 4 checks them. It deletes the folders `output/` and
#'    `output_fake_data/` of each scenario that it writes, with one printed
#'    line. That output belongs to the results of an earlier run.
#'    The section Output of the run log then says that this run made no
#'    output, until [make_outputs()] runs. Then it writes the files of the
#'    section The files. The files of the scenarios that are not in
#'    `scenarios` stay as they are.
#' 4. For each scenario with a function `expect_<scenario>()`, it calls that
#'    function on the results of the scenario. `expect_real()` is optional.
#'    The function MUST return a data.frame with at least one row and the
#'    columns `assertion`, `expected`, `observed` and `pass`.
#'    `save_results()` writes it to `<version>_results/<scenario>/check.csv`
#'    and prints it. For `"real"` without `expect_real()`, it deletes
#'    `check.csv`. An error, zero rows or another shape is one failed
#'    assertion. A change of an object of the functions folder in place, for
#'    example `CFG`, adds one failed assertion that names the first element
#'    that changed. The section on these objects in [add_analyses()] gives
#'    the rules. This rule is the same for `expect_real()` and for the fake
#'    scenarios. `save_results()` writes `check.csv` to a temporary file in
#'    the same folder and renames it, so a reader never sees a part of it.
#' 5. It writes the run log `<version>_results/run.md`, also after a stop in
#'    step 2, 3 or 4. The section The run log gives its content.
#' 6. After every scenario, it stops when a value of `pass` is not `TRUE`.
#'    It prints each failed assertion in full with `cat()`, because R cuts an
#'    error message at 1000 bytes by default. The error gives the number of
#'    failed assertions, their scenarios and the path of `check.csv`. The
#'    files stay, so that you can find the fault, and [read_results()] stops
#'    on them until the assertions pass.
#' @section The run log:
#' [run_plan()] replaces the run log at its start with a short log that says
#' that the run has not finished. A run that ends before the next log, for
#' example when R stops, leaves that log. [run_plan()] also writes the full
#' log when it stops on failed analyses, and [make_outputs()] adds its files
#' to it in the same R session. Each run replaces the file, and carries its
#' section `History` forward. The log is GitHub-flavoured Markdown in ASCII,
#' with these sections in this order:
#' - `When`: the date and the time of the start of the run.
#' - `Run`: the result in one sentence and the scenarios of the run. Then the
#'   run date of each other scenario on the disk, from its manifest. Then the
#'   versions of R, pbn, plnr and cohort, the number of the other packages,
#'   and the number of workers. The manifest of each results file lists the
#'   other packages.
#' - `Failures`, only after a failure: the failed analyses with their errors,
#'   the number of analyses that did not run after the stop of the run, the
#'   failed assertions, and the failed rules of [check_results()].
#' - `Scenarios`: the simulations and the analyses of each scenario.
#' - `Data`: the cache and the rows of each data element.
#' - `Exclusions`: the exclusion table of simulation 1 of each scenario. A
#'   cohort without an exclusion step gets one sentence instead.
#' - `Assertions`: the count for each scenario, and its assertions. For more
#'   than 30 assertions, the failed ones only.
#' - `Output` and `Files`: the files of [make_outputs()] and the number of
#'   warnings of the output function for each scenario, without their text.
#'   Then the results files, with their sizes.
#' - `Timings`: seconds on the clock. The wall seconds of each step of the
#'   main session, and of each scenario. Then the calls of each data step and
#'   analysis kind of each scenario, with the median and the maximum seconds
#'   of one call. The share gives its part of the seconds of all data steps
#'   and analyses.
#' - `History`: one line for each run of this version, oldest first. Each
#'   line gives the start and the result in a few words. It also gives the
#'   scenarios, the workers, the wall seconds of `run_plan()`,
#'   `save_results()` and [make_outputs()], and the version of pbn.
#'
#' The log holds counts, exclusion tables and the rows of `check.csv`. It
#' never holds the text of an error, because an error message can hold a
#' value of the data. These errors get a number in the log:
#' - the error of each failed analysis or data step, one number for each
#'   message,
#' - the error of an `expect_` function that stopped,
#' - the detail of each failed rule of [check_results()], which can quote a
#'   name of the project,
#' - the error of `save_results()` itself, also in the checks of its
#'   arguments,
#' - the error of the output function of [make_outputs()].
#'
#' The file `<version>_results/run_errors_gitexclude.md` holds the full text
#' of each error under its number. In a git repository, pbn writes it only
#' when git ignores it, as the line `*gitexclude*` of `.gitignore` does.
#' Otherwise the log says that the file is missing. Each run deletes the old
#' file, and pbn writes it only when the run has an error.
#'
#' A project string outside a table, for example a file name of
#' [make_outputs()], is on one line as a code span. A string in a table cell
#' is on one line too. So a line end in a string cannot start a heading or a
#' list item.
#'
#' Only the sections `When`, `Timings` and `History` change in every run. Two
#' runs of the same code on one day, with the same data and cache, give the
#' same other lines. So put `run.md` in git, and `git diff` shows the real
#' changes of a run. [render_run()] renders the log to a PDF.
#'
#' pbn measures each time with `proc.time()` in the R session that does the
#' work, also on a worker. A data step gets the time of its function. Data
#' from the cache gets the time of the read. An analysis gets the time of its
#' action function. Each one also records the time of the clock of the
#' computer at its end. A scenario runs from the start of its first data step
#' or analysis to the end of its last one. All workers run on this computer,
#' so their clocks are one clock. With workers, the scenarios overlap, so
#' their seconds do not add up to those of `run_plan()`. No timing goes to
#' the value of [run_plan()], a results file, `check.csv` or a cache key.
#'
#' The history is in `run.md` only. The log at the start of [run_plan()]
#' adds the line "did not finish" for the run. The full log replaces that
#' line, so each run has one line. A run that R stops keeps its line "did not
#' finish". pbn reads the history from the `run.md` on the disk. When it
#' cannot read a line, for example after a hand edit, it leaves the line out
#' and prints one line. The run goes on. A deleted `run.md` starts the
#' history again.
#' @section The files:
#' git gets the results of each scenario as one file, `results.qs2`, and
#' never the results of each simulation. These are the files of
#' `<version>_results/<scenario>/`:
#'
#' ```
#' check.csv                  the assertions of expect_<scenario>()
#' results.qs2                the results, stacked for more than 1 simulation
#' results_gitexclude.qs2     the kinds of `gitexclude`, stacked the same way
#' sims_gitexclude/           only for more than 1 simulation
#'   sim_<kkkk>.qs2           the results of simulation <kkkk>
#'   sim_<kkkk>_gitexclude.qs2  the kinds of `gitexclude` of simulation <kkkk>
#' ```
#'
#' A scenario with 1 simulation has no folder `sims_gitexclude/`. Its
#' `results.qs2` is the results object of its one simulation. For more
#' simulations, `results.qs2` holds the simulations stacked, with the rules
#' of the section Stacking. The name of `sims_gitexclude/` holds
#' `gitexclude`, so the line `*gitexclude*` of `.gitignore` keeps it out of
#' git. A clone of the repository thus has no results of one simulation.
#' [read_results()] with `sim` reads them.
#'
#' Before it writes a scenario, `save_results()` deletes each file in its
#' `sims_gitexclude/`, and prints one line when there was one. So no
#' simulation of an earlier run stays, for example after `sims` became
#' smaller. For a scenario that now has 1 simulation, it deletes the folder.
#'
#' `save_results()` stops when a file or a folder that it deletes is still
#' there afterwards, for example because another program holds it open on
#' Windows. The error names the file.
#'
#' pbn 2026.11.22 and older wrote the results of each simulation to
#' `sim_<kkkk>/results.qs2`, and git tracked them. When `save_results()`
#' writes a scenario with more than 1 simulation, it deletes each folder
#' `sim_<kkkk>/` of that scenario, and prints one line.
#' @section Stacking:
#' The stacked results hold every element of the results of simulation 1,
#' in the same order. These rules apply at each depth:
#' - A data.frame gets a first column `sim`, and the rows of the simulations
#'   in order. It keeps the class of simulation 1, so a data.table stays a
#'   data.table.
#' - A list is stacked element by element, and pbn matches the elements by
#'   name. So each element of a list MUST have its own name.
#' - Each other element becomes a list with one entry for each simulation,
#'   named `sim_0001`, `sim_0002` and so on. This includes an atomic vector,
#'   a matrix, a factor, a date and `NULL`.
#'
#' Stacking stops when two simulations differ in the structure at one path,
#' and the error names the path and the two simulations. It also stops on a
#' data.frame with a column `sim`, because pbn adds that column.
#'
#' The `manifest` is that of simulation 1. In it, `sim` and `n_rows_data`
#' are `NA`, `sims` is the number of simulations, and `stacked` is `TRUE`.
#' The columns `from_cache` and `cache_key` of `data_cache` are `NA`. The
#' counts `n_rows_before_exclusions` and `n_rows_after_exclusions` are `NA`
#' too, and `n_rows_lists` is an empty list. The element `n_rows_by_sim`
#' holds the counts of each simulation. It is a data.frame with one row for
#' each simulation and data element. Its columns are `sim`, `data`,
#' `n_rows_data`, `n_rows_before_exclusions` and `n_rows_after_exclusions`.
#' Each data.frame of an entry of `n_rows_lists` adds a row with the `data`
#' `<element>[[i]]` and its rows in `n_rows_data`. The rule
#' `no_row_per_person` of [check_results()] reads it.
#' @section Two files:
#' `save_results()` writes the results of the kinds in `gitexclude` to
#' `results_gitexclude.qs2`, beside `results.qs2`, and to
#' `sim_<kkkk>_gitexclude.qs2`, beside each `sim_<kkkk>.qs2`. The section The
#' files shows them. `results.qs2` and `sim_<kkkk>.qs2` hold `manifest`,
#' `exclusions` and every other kind. Use `gitexclude` for a
#' kind with fitted models, imputations or one row for each person. The
#' project's `.gitignore` MUST hold the line `*gitexclude*`, so that git
#' never tracks that file. [read_results()] reads both files.
#'
#' These rules apply:
#' - `save_results()` stops before it writes anything when a name in
#'   `gitexclude` is not an analysis kind of `p`.
#' - In a git repository, it also stops when git does not ignore
#'   `results_gitexclude.qs2`.
#' - [check_results()] checks `results.qs2` and `sim_<kkkk>.qs2`, and not
#'   the files of `gitexclude`.
#' - The simulations of the two files are stacked apart, with the rules of
#'   the section Stacking.
#' - Without kinds in `gitexclude`, `save_results()` deletes an old
#'   `results_gitexclude.qs2` of the scenario, and prints one line. The old
#'   `sim_<kkkk>_gitexclude.qs2` files go with the rest of
#'   `sims_gitexclude/`, see the section The files.
#' @return Invisibly, a data.frame of every assertion, with the columns
#'   `scenario`, `assertion`, `expected`, `observed` and `pass`.
#' @examples
#' home <- file.path(tempdir(), "pbn_save_example")
#' dir.create(file.path(home, "v01_functions"), recursive = TRUE)
#' dir.create(file.path(home, "data"))
#' writeLines("extends: house_2026_11_17", file.path(home, "pbn.yml"))
#' writeLines("# The run script.", file.path(home, "v01_run.R"))
#' writeLines(
#'   c(
#'     "CFG <- list(scenarios = data.frame(scenario = c('real', 'null'), sims = 1))",
#'     "expect_null <- function(results) {",
#'     "  data.frame(assertion = 'one row', expected = 1,",
#'     "    observed = nrow(results$means), pass = nrow(results$means) == 1)",
#'     "}"
#'   ),
#'   file.path(home, "v01_functions", "_cfg.R")
#' )
#' env <- new.env()
#' pbn::initialize_project("v01", home, c(main = file.path(home, "data")), env = env)
#'
#' scenarios <- pbn::scenarios(env$CFG)
#' p <- plnr::Plan$new()
#' pbn::add_data(p, "d", fn = function(scenario, sim) {
#'   return(data.frame(x = stats::rnorm(5)))
#' })
#' pbn::add_analyses(p, scenarios, name = "means", fn = function(data, argset) {
#'   return(data.frame(mean_x = mean(data$d$x)))
#' })
#' r <- pbn::run_plan(p)
#' pbn::save_results(r, p, scenarios)
#' names(pbn::read_results("v01", home = home))
#'
#' unlink(home, recursive = TRUE)
#' @family results
#' @seealso `vignette("pbn")`, which builds and runs a small fake project.
#' @export
save_results <- function(r, p, scenarios, gitexclude = NULL) {
  start <- proc.time()[["elapsed"]]
  check_state("save_results")
  # A stop in the checks of the arguments writes the run log as failed, so
  # run.md never keeps the result of an earlier run.
  info <- NULL
  failed <- tryCatch(
    {
      check_plan(p)
      table <- check_scenario_table(scenarios, "`scenarios`")
      info <- plan_info(p)
      check_run(r, p, info)
      gitexclude <- check_gitexclude(gitexclude, info, table)
      check_sims_ignored(table, gitexclude)
      index <- analysis_index(p, info)
      check_coverage(index, table, info)
      check_settings(index, table, info)
      check_inputs(table)
      NULL
    },
    error = function(e) return(e)
  )
  if (!is.null(failed)) {
    write_preflight_log(info, failed)
    stop(failed)
  }

  # From here on, save_results() writes the run log, also when it stops.
  done <- new.env(parent = emptyenv())
  done$seconds <- c(write = 0, contract = 0, expect = 0)
  checks <- tryCatch(
    save_steps(r, index, table, info, gitexclude, done),
    error = function(e) return(e)
  )
  run <- info$run
  if (is.null(run)) {
    run <- run_record(p, info, r, NULL, 1, start, Sys.time())
  }
  write_run_log(saved_log(run, table, done, checks, elapsed_since(start)))
  if (inherits(checks, "error")) {
    stop(checks)
  }
  stop_on_failed_checks(checks, state$results)
  return(invisible(checks))
}

# Steps 2 to 4 of save_results(): builds, checks and writes the results, and
# returns the assertions of the expect_ functions. The environment `done`
# gets the seconds of each part, the failed rules of check_results() and the
# assertions.
save_steps <- function(r, index, table, info, gitexclude, done) {
  # The fingerprint of the raw data that run_plan() read, from its stamp.
  stamp <- attr(r, "pbn_run", exact = TRUE)
  data_files <- stamp[["data_files"]]
  if (is.null(data_files)) {
    data_files <- state$data_files
  }
  start <- proc.time()[["elapsed"]]
  objects <- build_objects(r, index, table, info, gitexclude, data_files)
  done$seconds[["write"]] <- elapsed_since(start)
  start <- proc.time()[["elapsed"]]
  done$contract <- contract_failures(objects)
  done$seconds[["contract"]] <- elapsed_since(start)
  stop_on_contract(done$contract)
  root <- state$results
  start <- proc.time()[["elapsed"]]
  done$outputs_deleted <- drop_outputs(root, table$scenario)
  for (s in table$scenario) {
    write_scenario(objects[[s]], root, s)
  }
  done$seconds[["write"]] <- done$seconds[["write"]] + elapsed_since(start)
  start <- proc.time()[["elapsed"]]
  checks <- list()
  for (s in table$scenario) {
    fn <- expect_function(s)
    if (!is.null(fn)) {
      checks[[s]] <- check_expectations(fn, root, s)
    } else {
      # Only "real" MAY have no expect_ function.
      delete_paths(file.path(root, s, "check.csv"))
    }
  }
  checks <- if (length(checks) == 0) {
    empty_checks()
  } else {
    do.call(rbind, unname(checks))
  }
  done$seconds[["expect"]] <- elapsed_since(start)
  done$checks <- checks
  return(checks)
}

# Stops when the plan has no data or analysis of pbn, or when `r` is not the
# value of run_plan(p): other names, an analysis that pbn did not add, or a
# failed analysis.
check_run <- function(r, p, info) {
  if (is.null(info) || length(info$names) == 0 || length(info$data) == 0) {
    stop(
      "save_results() needs a plan with data from pbn::add_data() and ",
      "analyses from pbn::add_analyses().",
      call. = FALSE
    )
  }
  check_stamp(r, p, info)
  other <- setdiff(names(p$analyses), c(info$names, info$hidden))
  if (length(other) > 0) {
    stop(
      "pbn::add_analyses() did not add these analyses of the plan, so pbn ",
      "saved nothing: ",
      paste(other, collapse = ", "),
      call. = FALSE
    )
  }
  stop_on_failed(r)
  return(invisible(NULL))
}

# Stops when `r` is not a list with the analyses of `p` and the stamp that
# run_plan() gives the value of the plan `p`.
check_stamp <- function(r, p, info) {
  stamp <- attr(r, "pbn_run", exact = TRUE)
  problem <- if (!is.list(r) || !identical(names(r), names(p$analyses))) {
    "It has other analyses."
  } else if (is.null(stamp)) {
    "It has no stamp of run_plan()."
  } else if (!identical(stamp[["plan"]], info$token)) {
    "Its stamp names another plan."
  }
  if (!is.null(problem)) {
    stop(
      "`r` must be the value of pbn::run_plan(p) for the plan `p`. ",
      problem,
      call. = FALSE
    )
  }
  return(invisible(NULL))
}

# One row for each analysis of the plan: its name, scenario, simulation,
# kind, whether it counts the rows, and its argset.
analysis_index <- function(p, info) {
  analyses <- p$analyses
  nm <- names(analyses)
  # One pass over the list. A lookup by name for each analysis takes a time
  # that grows with the square of the number of analyses.
  argsets <- unname(lapply(analyses, function(a) return(a[["argset"]])))
  out <- data.frame(
    name = nm,
    scenario = vapply(argsets, function(a) return(a[["scenario"]]), ""),
    sim = vapply(argsets, function(a) return(as.integer(a[["sim"]])), 1L),
    analysis = vapply(argsets, function(a) return(a[["analysis"]]), ""),
    hidden = nm %in% info$hidden
  )
  out$argset <- argsets
  return(out)
}

# Stops when the scenarios and simulations of the plan are not those of
# `table`, or when a kind has no analysis for one of them.
check_coverage <- function(index, table, info) {
  want <- paste(rep(table$scenario, table$sims), sequence(table$sims))
  have <- unique(paste(index$scenario, index$sim))
  if (!setequal(want, have)) {
    stop(
      "The analyses of the plan are not for the scenarios and simulations ",
      "of `scenarios`. Give save_results() the scenarios that ",
      "add_analyses() got. In the plan only: ",
      paste(setdiff(have, want), collapse = ", "),
      ". In `scenarios` only: ",
      paste(setdiff(want, have), collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  for (i in seq_len(nrow(info$kinds))) {
    kind <- info$kinds$analysis[i]
    need <- want
    if (info$kinds$control[i]) {
      need <- want[!startsWith(want, "real ")]
    }
    of_kind <- paste(index$scenario, index$sim)[index$analysis == kind]
    absent <- setdiff(need, of_kind)
    if (length(absent) > 0) {
      stop(
        "The analysis kind \"",
        kind,
        "\" has no analysis for these scenarios and simulations: ",
        paste(absent, collapse = ", "),
        ". Give each call of add_analyses() the same scenarios.",
        call. = FALSE
      )
    }
  }
  return(invisible(NULL))
}

# Stops when the settings of a scenario in `table` are not the settings in
# the argsets of its analyses, for example when `scenarios` comes from
# another CFG than the plan.
check_settings <- function(index, table, info) {
  cols <- union(setting_columns(table), info$settings)
  bad <- character(0)
  for (i in seq_len(nrow(index))) {
    want <- settings_of(table, index$scenario[i])
    got <- index$argset[[i]][intersect(cols, names(index$argset[[i]]))]
    # An empty list from settings_of() has no names, and an empty subset of
    # an argset has names character(0). Both mean no settings.
    same <- length(got) == 0 && length(want) == 0
    if (!same && !identical(got[sort(names(got))], want[sort(names(want))])) {
      bad <- union(bad, index$scenario[i])
    }
  }
  if (length(bad) > 0) {
    stop(
      "The settings of these scenarios in `scenarios` are not the settings ",
      "that add_analyses() put in the argsets: ",
      paste(bad, collapse = ", "),
      ". Give save_results() the scenarios that add_analyses() got. pbn ",
      "saved nothing.",
      call. = FALSE
    )
  }
  return(invisible(NULL))
}

# Stops when the run script is not in source_md5, when a folder of the
# project did not exist for "real", or when a fake scenario has no expect_
# function.
check_inputs <- function(table) {
  script <- paste0(state$version, "_run.R")
  if (!script %in% names(state$source_md5)) {
    stop(
      "There was no run script ",
      script,
      " in ",
      state$home,
      " when initialize_project() ran, so the manifest cannot name it. ",
      "pbn saved nothing.",
      call. = FALSE
    )
  }
  absent <- state$folders_absent
  if ("real" %in% table$scenario && length(absent) > 0) {
    stop(
      if (length(absent) == 1) "The folder " else "The folders ",
      paste0(
        absent,
        " (",
        state$folders[absent],
        ")",
        collapse = ", "
      ),
      " did not exist when initialize_project() ran, so pbn cannot save ",
      "the scenario \"real\". pbn saved nothing.",
      call. = FALSE
    )
  }
  fake <- table$scenario[table$scenario != "real"]
  missing <- fake[vapply(
    fake,
    function(s) return(is.null(expect_function(s))),
    logical(1)
  )]
  if (length(missing) > 0) {
    stop(
      "Each fake scenario needs a function expect_<scenario>(results) in ",
      state$version,
      "_functions/. These are missing: ",
      paste0("expect_", missing, "()", collapse = ", "),
      ". pbn saved nothing.",
      call. = FALSE
    )
  }
  return(invisible(NULL))
}

# The function expect_<scenario>() of the sourced environment, or NULL.
expect_function <- function(scenario) {
  return(get0(
    paste0("expect_", scenario),
    envir = state$env,
    mode = "function",
    inherits = FALSE
  ))
}

# For each scenario of `table`, a list with `sims`, the object of
# results.qs2 of each simulation in `runs`, the object of
# results_gitexclude.qs2 or NULL of each simulation in `private`, and, for
# more than 1 simulation, the stacked object of results.qs2.
build_objects <- function(r, index, table, info, gitexclude, data_files) {
  fake_md5 <- function_md5(data_code(info))
  out <- list()
  for (i in seq_len(nrow(table))) {
    s <- table$scenario[i]
    runs <- lapply(seq_len(table$sims[i]), function(sim) {
      x <- build_object(r, index, table, info, s, sim, fake_md5, data_files)
      return(split_results(x, gitexclude))
    })
    out[[s]] <- list(
      sims = table$sims[i],
      runs = lapply(runs, function(x) return(x$public)),
      private = lapply(runs, function(x) return(x$private))
    )
    if (table$sims[i] > 1) {
      out[[s]]$stacked <- stack_results(out[[s]]$runs, s)
    }
  }
  return(out)
}

# The results object of one scenario and simulation, with every element.
# split_results() splits it into the two files.
build_object <- function(r, index, table, info, s, sim, fake_md5, data_files) {
  here <- index[index$scenario == s & index$sim == sim, , drop = FALSE]
  kinds <- info$kinds
  if (s == "real") {
    kinds <- kinds[!kinds$control, , drop = FALSE]
  }
  counted <- r[[here$name[here$hidden]]]
  results <- exclusions_element(attr(counted, "exclusions", exact = TRUE))
  for (kind in kinds$analysis) {
    rows <- !here$hidden & here$analysis == kind
    results[kind] <- list(combine_kind(
      r[here$name[rows]],
      here$argset[rows],
      kind,
      info$dims[[kind]]
    ))
  }
  rownames(kinds) <- NULL
  kinds$file <- rep("results.qs2", nrow(kinds))
  n_rows <- counted
  attributes(n_rows) <- list(names = names(counted))
  cohort_n <- cohort_counts(counted)
  manifest <- c(
    list(
      pbn_version = as.character(getNamespaceVersion("pbn")),
      r_version = R.version.string,
      packages = loaded_packages(info$worker_packages),
      run_date = format(Sys.Date(), "%Y-%m-%d"),
      version = state$version,
      scenario = s,
      sim = sim,
      sims = table$sims[table$scenario == s],
      settings = settings_of(table, s),
      source_md5 = state$manifest_md5
    ),
    if (s == "real") {
      list(
        data_files = data_files,
        fake_real_data = isTRUE(state$fake_real_data)
      )
    } else {
      list(fake_md5 = fake_md5)
    },
    list(
      n_rows_data = n_rows,
      n_rows_lists = n_rows_lists(counted),
      n_rows_before_exclusions = cohort_n$before,
      n_rows_after_exclusions = cohort_n$after,
      analyses = kinds,
      data_cache = data_cache_table(info, attr(counted, "cache", exact = TRUE))
    )
  )
  return(c(list(manifest = manifest), results))
}

# The rows of each data.frame of each data element that is a list of
# data.frames with 2 or more numbers of rows, from the value `counted` of the
# analysis that counts the rows: a named list of integer vectors, empty
# when no data element is such a list.
n_rows_lists <- function(counted) {
  parts <- attr(counted, "parts", exact = TRUE)
  return(if (is.null(parts)) stats::setNames(list(), character(0)) else parts)
}

# The rows given to the cohort of each data element, `before`, and the rows
# after its exclusions, `after`, from the value `counted` of the analysis
# that counts the rows. `after` is n_after of the last step of the exclusion
# table, or `before` for a cohort without a step. Both are NA for data
# without a cohort.
cohort_counts <- function(counted) {
  start <- attr(counted, "n_before", exact = TRUE)
  tables <- attr(counted, "exclusions", exact = TRUE)
  nm <- names(counted)
  before <- vapply(
    nm,
    function(n) return(if (is.null(start[[n]])) NA_integer_ else start[[n]]),
    integer(1)
  )
  after <- vapply(
    nm,
    function(n) {
      e <- tables[[n]]
      if (is.null(e)) {
        return(NA_integer_)
      }
      if (nrow(e) == 0) {
        return(before[[n]])
      }
      return(as.integer(e$n_after[nrow(e)]))
    },
    integer(1)
  )
  return(list(before = before, after = after))
}

# The element exclusions: the exclusion tables of the cohorts, row-bound in
# the order of the data. An empty list without a cohort.
exclusions_element <- function(tables) {
  if (length(tables) == 0) {
    return(list())
  }
  out <- do.call(rbind, unname(tables))
  rownames(out) <- NULL
  return(list(exclusions = out))
}

# One row for each data element of the plan: whether it has a cache, whether
# the data came from the cache, and the md5 of its cache key. `cache` holds
# the record of each data element with a cache, from the analysis that
# counts the rows.
data_cache_table <- function(info, cache) {
  nm <- names(info$data)
  record <- function(n, field, none) {
    x <- cache[[n]]
    return(if (is.null(x)) none else x[[field]])
  }
  return(data.frame(
    data = nm,
    cache = vapply(nm, function(n) return(isTRUE(info$cache[[n]])), TRUE),
    from_cache = vapply(nm, record, TRUE, field = "from_cache", none = FALSE),
    cache_key = vapply(nm, record, "", field = "key", none = NA_character_),
    row.names = NULL
  ))
}

# The element of one analysis kind from the values `values` of its analyses
# and their argsets. `dims` names the extra dimensions of the kind.
combine_kind <- function(values, argsets, kind, dims) {
  if (all(vapply(values, is.data.frame, logical(1)))) {
    return(bind_frames(values, argsets, kind, dims))
  }
  if (is_list_of_frames(values)) {
    nm <- names(values[[1]])
    out <- lapply(nm, function(n) {
      parts <- lapply(values, function(v) return(v[[n]]))
      return(bind_frames(parts, argsets, paste0(kind, "$", n), dims))
    })
    names(out) <- nm
    return(out)
  }
  if (length(dims) == 0) {
    return(values[[1]])
  }
  return(stats::setNames(
    unname(values),
    paste0(kind, "__", seq_along(values))
  ))
}

# TRUE when each of `values` is a plain list with the same names as the
# first, each name once and not empty, and each element is a data.frame.
is_list_of_frames <- function(values) {
  nm <- names(values[[1]])
  if (length(nm) == 0 || anyNA(nm) || !all(nzchar(nm))) {
    return(FALSE)
  }
  if (anyDuplicated(nm) > 0) {
    return(FALSE)
  }
  return(all(vapply(values, is_frame_list, logical(1), nm = nm)))
}

# TRUE when `v` is a plain list with the names `nm`, and each element is a
# data.frame.
is_frame_list <- function(v, nm) {
  if (!is.list(v) || is.data.frame(v) || !is.null(oldClass(v))) {
    return(FALSE)
  }
  return(
    identical(names(v), nm) && all(vapply(v, is.data.frame, logical(1)))
  )
}

# The data.frames `values` row-bound, each with its extra dimensions first,
# in the class of the first data.frame.
bind_frames <- function(values, argsets, kind, dims) {
  if (length(dims) == 0) {
    return(values[[1]])
  }
  parts <- lapply(values, function(v) return(as.data.frame(v)))
  for (i in seq_along(parts)) {
    lead <- setdiff(dims, names(parts[[i]]))
    if (length(lead) == 0) {
      next
    }
    first <- as.data.frame(argsets[[i]][lead], stringsAsFactors = FALSE)
    first <- first[rep(1L, nrow(parts[[i]])), , drop = FALSE]
    parts[[i]] <- cbind(first, parts[[i]])
  }
  out <- tryCatch(do.call(rbind, unname(parts)), error = function(e) {
    stop(
      "pbn cannot row-bind the data.frames of the analysis kind \"",
      kind,
      "\": ",
      conditionMessage(e),
      ". Give each analysis of the kind the same columns.",
      call. = FALSE
    )
  })
  rownames(out) <- NULL
  if (data.table::is.data.table(values[[1]])) {
    data.table::setDT(out)
  } else if (!identical(class(out), class(values[[1]]))) {
    class(out) <- class(values[[1]])
  }
  return(out)
}

# Prints each failed rule of each object with cat(), and stops when there is
# one.
check_objects <- function(objects) {
  stop_on_contract(contract_failures(objects))
  return(invisible(NULL))
}

# Each failed rule of check_results() of each object, as
# "<object>: <rule>: <detail>".
contract_failures <- function(objects) {
  failures <- character(0)
  for (s in names(objects)) {
    o <- objects[[s]]
    all <- o$runs
    names(all) <- if (o$sims == 1) {
      s
    } else {
      file.path(s, sim_file(seq_len(o$sims)))
    }
    if (!is.null(o$stacked)) {
      all[[paste(s, "stacked")]] <- o$stacked
    }
    for (nm in names(all)) {
      found <- check_results(all[[nm]])
      failed <- found[found$status == "FAIL", , drop = FALSE]
      failures <- c(
        failures,
        paste0(nm, ": ", failed$rule, ": ", failed$detail, recycle0 = TRUE)
      )
    }
  }
  return(failures)
}

# Prints each failed rule `failures` with cat(), and stops when there is one.
stop_on_contract <- function(failures) {
  if (length(failures) == 0) {
    return(invisible(NULL))
  }
  cat("pbn: these results fail check_results():\n")
  cat(paste0("  ", failures, "\n"), sep = "")
  stop(
    length(failures),
    " failures of check_results(), printed above. pbn saved nothing.",
    call. = FALSE
  )
}

# Deletes the folders output/ and output_fake_data/ of each scenario of
# `scenarios` under `root`, the output of an earlier run, because the new
# results replace the results of that output. Prints one line when it
# deleted a folder, and returns the deleted folders relative to the project.
drop_outputs <- function(root, scenarios) {
  dirs <- file.path(
    rep(scenarios, each = 2),
    rep(c("output", "output_fake_data"), length(scenarios))
  )
  dirs <- dirs[dir.exists(file.path(root, dirs))]
  if (length(dirs) == 0) {
    return(character(0))
  }
  delete_paths(file.path(root, dirs), recursive = TRUE)
  rel <- paste0(file.path(basename(root), dirs), "/")
  cat(
    "pbn: deleted the output of an earlier run beside the new results: ",
    paste(rel, collapse = ", "),
    ". Run pbn::make_outputs() for the output of these results.\n",
    sep = ""
  )
  return(rel)
}

# Writes the results of one scenario under `root`, and stacks them when the
# scenario has more than 1 simulation.
write_scenario <- function(o, root, s) {
  folder <- file.path(root, s)
  dir.create(folder, recursive = TRUE, showWarnings = FALSE)
  # Until the expect_ function has checked the new results, check.csv holds
  # one failed assertion, so read_results() refuses them. A reader thus
  # never gets new results with the check.csv of an earlier run.
  write_csv_atomic(
    failed_assertion(
      "save_results() checks these results with expect_<scenario>()",
      "not checked yet"
    ),
    file.path(folder, "check.csv")
  )
  empty_sims_folder(folder, s, o$sims)
  if (o$sims == 1) {
    qs2::qs_save(o$runs[[1]], file.path(folder, "results.qs2"))
    write_private(o$private[[1]], file.path(folder, gitexclude_file))
    return(invisible(NULL))
  }
  delete_paths(file.path(folder, "results.qs2"))
  dir.create(file.path(folder, sims_folder), showWarnings = FALSE)
  for (sim in seq_len(o$sims)) {
    qs2::qs_save(o$runs[[sim]], file.path(folder, sim_file(sim)))
    write_private(o$private[[sim]], file.path(folder, sim_private_file(sim)))
  }
  stack_scenario(root, s, o$sims)
  delete_old_layout(folder)
  return(invisible(NULL))
}

# Empties the folder sims_gitexclude/ of the scenario `s` in `folder`, so
# that no file of an earlier run stays beside the new results: a simulation
# above `sims`, or every simulation when the scenario now has 1. For 1
# simulation, it deletes the folder. It prints one line when it deleted a
# file.
empty_sims_folder <- function(folder, s, sims) {
  sub <- file.path(folder, sims_folder)
  old <- list.files(sub, all.files = TRUE, no.. = TRUE, recursive = TRUE)
  delete_paths(sub, recursive = TRUE)
  if (length(old) == 0) {
    return(invisible(NULL))
  }
  cat(
    "pbn: ",
    if (sims == 1) "deleted " else "emptied ",
    sub,
    "/",
    if (sims == 1) {
      paste0(", because the scenario \"", s, "\" now has 1 simulation")
    } else {
      paste0(" before it wrote the scenario \"", s, "\"")
    },
    ": it held ",
    length(old),
    if (length(old) == 1) " file" else " files",
    " of an earlier run.\n",
    sep = ""
  )
  return(invisible(old))
}

# Deletes the folders sim_<kkkk>/ of the layout of pbn 2026.11.22 and older
# in the folder of a scenario, and prints one line. git tracked the results
# of each simulation there.
delete_old_layout <- function(folder) {
  old <- list.files(folder, pattern = "^sim_[0-9]{4}$")
  old <- old[dir.exists(file.path(folder, old))]
  if (length(old) == 0) {
    return(invisible(NULL))
  }
  delete_paths(file.path(folder, old), recursive = TRUE)
  cat(
    "pbn: deleted the ",
    length(old),
    " folders sim_<kkkk>/ of the old layout in ",
    folder,
    ". The results of each simulation are now in ",
    sims_folder,
    "/.\n",
    sep = ""
  )
  return(invisible(old))
}

# Prints each failed assertion of `checks` with cat(), and stops when there
# is one. R cuts an error message at 1000 bytes by default, and cat() does
# not, so the error gives the count, the scenarios and the files only.
stop_on_failed_checks <- function(checks, root) {
  failed <- checks[!checks$pass %in% TRUE, , drop = FALSE]
  if (nrow(failed) == 0) {
    return(invisible(NULL))
  }
  cat("pbn: these assertions failed:\n")
  cat(
    paste0(
      "  ",
      failed$scenario,
      ": ",
      failed$assertion,
      " (expected ",
      failed$expected,
      ", observed ",
      failed$observed,
      ")\n"
    ),
    sep = ""
  )
  utils::flush.console()
  scenarios <- unique(failed$scenario)
  stop(
    nrow(failed),
    " of ",
    nrow(checks),
    " assertions failed, in the scenarios ",
    paste(scenarios, collapse = ", "),
    ". pbn printed each failed assertion above. Each scenario has its ",
    "assertions in ",
    file.path(root, "<scenario>", "check.csv"),
    ".",
    call. = FALSE
  )
}

# The version of each package that is not a base package: the loaded and
# attached packages of this R session, and the packages of `workers`, a
# named vector of versions from the workers of run_plan(). A package in both
# has the version of this R session.
loaded_packages <- function(workers = NULL) {
  # qs2 writes the file, so it MUST be in the package list.
  loadNamespace("qs2")
  attached <- sub("^package:", "", grep("^package:", search(), value = TRUE))
  pkgs <- union(loadedNamespaces(), attached)
  base <- rownames(utils::installed.packages(
    lib.loc = .Library,
    priority = "base"
  ))
  pkgs <- setdiff(pkgs, base)
  versions <- vapply(
    pkgs,
    function(p) return(as.character(getNamespaceVersion(p))),
    character(1)
  )
  extra <- workers[!names(workers) %in% c(pkgs, base)]
  versions <- c(versions, extra)
  return(versions[order(names(versions))])
}
