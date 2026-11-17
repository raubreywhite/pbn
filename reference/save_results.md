# Save the results of a run

`save_results()` writes one results object for each scenario and
simulation of a run, and stacks the simulations. It checks the fake
scenarios with their `expect_` functions, and writes no results file
when an object fails
[`check_results()`](https://www.rwhite.no/pbn/reference/check_results.md).

## Usage

``` r
save_results(r, p, scenarios, gitexclude = NULL)
```

## Arguments

- r:

  The value of
  [`run_plan()`](https://www.rwhite.no/pbn/reference/run_plan.md) for
  the plan `p`.

- p:

  The plan, with data from
  [`add_data()`](https://www.rwhite.no/pbn/reference/add_data.md) and
  analyses from
  [`add_analyses()`](https://www.rwhite.no/pbn/reference/add_analyses.md).

- scenarios:

  The scenarios of the run, from
  [`scenarios()`](https://www.rwhite.no/pbn/reference/scenarios.md).
  They MUST be the scenarios of the analyses of `p`.

- gitexclude:

  `NULL`, or the names of the analysis kinds whose results hold data for
  each person. See the section Two files.

## Value

Invisibly, a data.frame of every assertion, with the columns `scenario`,
`assertion`, `expected`, `observed` and `pass`.

## Details

Call
[`initialize_project()`](https://www.rwhite.no/pbn/reference/initialize_project.md)
first. For each scenario and simulation, `save_results()` builds one
results object, a named list. Its first element is `manifest`. When a
data function returned a
[`cohort::CohortPipeline`](https://www.rwhite.no/cohort/reference/CohortPipeline.html),
the next element is `exclusions`, the exclusion tables of the cohorts
row-bound in the order of
[`add_data()`](https://www.rwhite.no/pbn/reference/add_data.md). The
column `data` names the data of each row. The section A cohort as the
data of [`add_data()`](https://www.rwhite.no/pbn/reference/add_data.md)
gives the columns. A plan without a cohort has no element `exclusions`.
Then comes one element for each analysis kind of
[`add_analyses()`](https://www.rwhite.no/pbn/reference/add_analyses.md),
under the kind name and in the order of the calls. The scenario `"real"`
has no element of a kind with `control = TRUE`. The element of a kind
holds the values of its analyses for that scenario and simulation, in
the order of the plan:

- When each value is a data.frame, the element is the values row-bound.
  Each extra dimension of the kind comes first as a column, unless the
  data.frame has a column of that name already. A data.table stays a
  data.table. A kind without extra dimensions has the data.frame of its
  one analysis.

- When each value is a list of data.frames with the same names, each
  name once, the element is a named list with these names. Each of its
  data.frames is the data.frames of that name row-bound, with the rule
  of a data.frame above. An example is `results$sem$estimates` and
  `results$sem$fit`.

- Otherwise, a kind without extra dimensions has the value of its one
  analysis.

- Otherwise, the element is a named list with one entry for each
  analysis. Each name is `<name>__<i>`, the analysis name without the
  scenario and the simulation. So the names are the same in each
  simulation.
  [`add_analyses()`](https://www.rwhite.no/pbn/reference/add_analyses.md)
  gives the order of `<i>`.

The `manifest` is a list with these elements:

- `pbn_version` and `r_version`.

- `packages`, a named character vector. The names are the attached and
  loaded packages of this R session that are not base packages, and the
  values are their versions. `qs2` is always in the list, because it
  writes the file. With 2 or more workers of
  [`run_plan()`](https://www.rwhite.no/pbn/reference/run_plan.md), the
  list also holds the packages loaded on a worker at the end of the run.
  An example is the package of a model. With 2 or more workers, this R
  session also loads doParallel, so the list then holds doParallel too.

- `run_date`, the date of the call as `YYYY-MM-DD`. It has no time, so
  two runs on one day with the same inputs and packages write the same
  bytes.

- `version`, `scenario`, `sim` and `sims`.

- `settings`, the named list of the setting columns of the scenario. It
  is an empty list when there are none.

- `source_md5`, the md5 of `<version>_run.R` and of each file in
  `<version>_functions/`, named by the path relative to `home`.
  [`initialize_project()`](https://www.rwhite.no/pbn/reference/initialize_project.md)
  computes it before it sources the files. The rule `manifest_source`
  needs the run script in it, so `save_results()` stops when
  `<version>_run.R` did not exist.

- For the scenario `"real"` only, `data_md5`, the md5 of each file
  directly in `data_raw` when
  [`run_plan()`](https://www.rwhite.no/pbn/reference/run_plan.md)
  started. So it names the data that the run read, also after a change
  of a file since
  [`initialize_project()`](https://www.rwhite.no/pbn/reference/initialize_project.md).
  `save_results()` stops for `"real"` when `data_raw` did not exist when
  [`initialize_project()`](https://www.rwhite.no/pbn/reference/initialize_project.md)
  ran.

- For a fake scenario only, `fake_md5`, one md5 of the code of the data
  functions of
  [`add_data()`](https://www.rwhite.no/pbn/reference/add_data.md),
  without their source references: each `fn`, and each step of the steps
  form. A change in a function that `fn` or a step calls does not change
  `fake_md5`. `source_md5` holds the md5 of the files of those
  functions.

- `n_rows_data`, a named integer vector with the number of rows of each
  data element. A list of data.frames with one number of rows, for
  example a list of imputed data sets, has the rows of one of them. Each
  other element that is not a data.frame has `NA`. The analysis
  `<scenario>__sim_<kkkk>__pbn_n_rows` of
  [`add_analyses()`](https://www.rwhite.no/pbn/reference/add_analyses.md)
  counts them where the data is, also on a worker. The rule
  `no_row_per_person` reads it.

- `n_rows_before_exclusions`, a named integer vector with the number of
  rows given to the cohort of each data element, or `NA` for data
  without a cohort. `no_row_per_person` reads it too.

- `n_rows_after_exclusions`, the same for the rows after the exclusions:
  `n_after` of the last step of the exclusion table. It has a number
  also when `n_rows_data` is `NA`, for example for a `mids` object.

- `analyses`, a data.frame with the columns `analysis`, `control` and
  `file`: the analysis kinds in the object, and the file of each,
  `"results.qs2"` or `"results_gitexclude.qs2"`.

- `data_cache`, a data.frame with one row for each data element of
  [`add_data()`](https://www.rwhite.no/pbn/reference/add_data.md) and
  the columns `data`, `cache`, `from_cache` and `cache_key`. `cache` is
  the argument of
  [`add_data()`](https://www.rwhite.no/pbn/reference/add_data.md).
  `from_cache` is `TRUE` when
  [`run_plan()`](https://www.rwhite.no/pbn/reference/run_plan.md) read
  the data from the cache. `cache_key` is the md5 of the cache key, or
  `NA` without a cache.

`save_results()` does these steps in this order. It stops at the first
step that fails:

1.  It checks its arguments. It stops when `r` has no stamp of
    [`run_plan()`](https://www.rwhite.no/pbn/reference/run_plan.md) for
    `p`. It stops when the settings of `scenarios` are not the settings
    in the argsets of `p`. It stops when a fake scenario has no function
    `expect_<scenario>(results)` in the environment of
    [`initialize_project()`](https://www.rwhite.no/pbn/reference/initialize_project.md),
    or when an analysis of `r` failed. In a git repository, it stops
    when git does not ignore each file that it would write into a folder
    `sims_gitexclude/`. The error names the first 3 of the files that
    git does not ignore. It writes no results file then, and it writes
    the run log with its error.

2.  It builds every results object, and the stacked object of each
    scenario with more than 1 simulation, and checks each with
    [`check_results()`](https://www.rwhite.no/pbn/reference/check_results.md).
    On a failure, it prints each failed rule with
    [`cat()`](https://rdrr.io/r/base/cat.html), writes no results file
    and stops.

3.  For each scenario, it replaces `check.csv` with one failed
    assertion, `not checked yet`, so that
    [`read_results()`](https://www.rwhite.no/pbn/reference/read_results.md)
    refuses the new results until step 4 checks them. Then it writes the
    files of the section The files. The files of the scenarios that are
    not in `scenarios` stay as they are.

4.  For each scenario with a function `expect_<scenario>()`, it calls
    that function on the results of the scenario. `expect_real()` is
    optional. The function MUST return a data.frame with at least one
    row and the columns `assertion`, `expected`, `observed` and `pass`.
    `save_results()` writes it to
    `<version>_results/<scenario>/check.csv` and prints it. For `"real"`
    without `expect_real()`, it deletes `check.csv`. An error, zero rows
    or another shape is one failed assertion. This rule is the same for
    `expect_real()` and for the fake scenarios. `save_results()` writes
    `check.csv` to a temporary file in the same folder and renames it,
    so a reader never sees a part of it.

5.  It writes the run log `<version>_results/run.md`, also after a stop
    in step 2, 3 or 4. The section The run log gives its content.

6.  After every scenario, it stops when a value of `pass` is not `TRUE`.
    It prints each failed assertion in full with
    [`cat()`](https://rdrr.io/r/base/cat.html), because R cuts an error
    message at 1000 bytes by default. The error gives the number of
    failed assertions, their scenarios and the path of `check.csv`. The
    files stay, so that you can find the fault, and
    [`read_results()`](https://www.rwhite.no/pbn/reference/read_results.md)
    stops on them until the assertions pass.

## The run log

[`run_plan()`](https://www.rwhite.no/pbn/reference/run_plan.md) replaces
the run log at its start with a short log that says that the run has not
finished. A run that ends before the next log, for example when R stops,
leaves that log.
[`run_plan()`](https://www.rwhite.no/pbn/reference/run_plan.md) also
writes the full log when it stops on failed analyses, and
[`make_outputs()`](https://www.rwhite.no/pbn/reference/make_outputs.md)
adds its files to it in the same R session. Each run replaces the file.
The log is GitHub-flavoured Markdown in ASCII, with these sections in
this order:

- `When`: the date and the time of the start of the run.

- `Run`: the result in one sentence and the scenarios of the run. Then
  the run date of each other scenario on the disk, from its manifest.
  Then the versions of R, pbn, plnr and cohort, and the number of
  workers.

- `Failures`, only after a failure: the failed analyses with their
  errors, the failed assertions, and the failed rules of
  [`check_results()`](https://www.rwhite.no/pbn/reference/check_results.md).

- `Scenarios`: the simulations and the analyses of each scenario.

- `Data`: the cache and the rows of each data element.

- `Exclusions`: the exclusion table of simulation 1 of each scenario.

- `Assertions`: the count for each scenario, and its assertions. For
  more than 30 assertions, the failed ones only.

- `Output` and `Files`: the files of
  [`make_outputs()`](https://www.rwhite.no/pbn/reference/make_outputs.md)
  and the results files, with their sizes.

- `Timings`: the seconds of each data step, of each analysis kind, and
  of each step of the main session.

The log holds counts, exclusion tables and the rows of `check.csv`. It
never holds the text of an error, because an error message can hold a
value of the data. These errors get a number in the log:

- the error of each failed analysis or data step, one number for each
  message,

- the error of an `expect_` function that stopped,

- the detail of each failed rule of
  [`check_results()`](https://www.rwhite.no/pbn/reference/check_results.md),
  which can quote a name of the project,

- the error of `save_results()` itself, also in the checks of its
  arguments,

- the error of the output function of
  [`make_outputs()`](https://www.rwhite.no/pbn/reference/make_outputs.md).

The file `<version>_results/run_errors_gitexclude.md` holds the full
text of each error under its number. In a git repository, pbn writes it
only when git ignores it, as the line `*gitexclude*` of `.gitignore`
does. Otherwise the log says that the file is missing. Each run deletes
the old file, and pbn writes it only when the run has an error.

A project string outside a table, for example a file name of
[`make_outputs()`](https://www.rwhite.no/pbn/reference/make_outputs.md),
is on one line as a code span. A string in a table cell is on one line
too. So a line end in a string cannot start a heading or a list item.

Only the sections `When` and `Timings` change in every run. Two runs of
the same code on one day, with the same data and cache, give the same
other lines. So put `run.md` in git, and `git diff` shows the real
changes of a run.
[`render_run()`](https://www.rwhite.no/pbn/reference/render_run.md)
renders the log to a PDF.

pbn measures each time with
[`proc.time()`](https://rdrr.io/r/base/proc.time.html) in the R session
that does the work, also on a worker. A data step gets the time of its
function. Data from the cache gets the time of the read. An analysis
gets the time of its action function. No timing goes to the value of
[`run_plan()`](https://www.rwhite.no/pbn/reference/run_plan.md), a
results file, `check.csv` or a cache key.

## The files

git gets the results of each scenario as one file, `results.qs2`, and
never the results of each simulation. These are the files of
`<version>_results/<scenario>/`:

    check.csv                  the assertions of expect_<scenario>()
    results.qs2                the results, stacked for more than 1 simulation
    results_gitexclude.qs2     the kinds of `gitexclude`, stacked the same way
    sims_gitexclude/           only for more than 1 simulation
      sim_<kkkk>.qs2           the results of simulation <kkkk>
      sim_<kkkk>_gitexclude.qs2  the kinds of `gitexclude` of simulation <kkkk>

A scenario with 1 simulation has no folder `sims_gitexclude/`. Its
`results.qs2` is the results object of its one simulation. For more
simulations, `results.qs2` holds the simulations stacked, with the rules
of the section Stacking. The name of `sims_gitexclude/` holds
`gitexclude`, so the line `*gitexclude*` of `.gitignore` keeps it out of
git. A clone of the repository thus has no results of one simulation.
[`read_results()`](https://www.rwhite.no/pbn/reference/read_results.md)
with `sim` reads them.

Before it writes a scenario, `save_results()` deletes each file in its
`sims_gitexclude/`, and prints one line when there was one. So no
simulation of an earlier run stays, for example after `sims` became
smaller. For a scenario that now has 1 simulation, it deletes the
folder.

`save_results()` stops when a file or a folder that it deletes is still
there afterwards, for example because another program holds it open on
Windows. The error names the file.

pbn 2026.11.22 and older wrote the results of each simulation to
`sim_<kkkk>/results.qs2`, and git tracked them. When `save_results()`
writes a scenario with more than 1 simulation, it deletes each folder
`sim_<kkkk>/` of that scenario, and prints one line.

## Stacking

The stacked results hold every element of the results of simulation 1,
in the same order. These rules apply at each depth:

- A data.frame gets a first column `sim`, and the rows of the
  simulations in order. It keeps the class of simulation 1, so a
  data.table stays a data.table.

- A list is stacked element by element, and pbn matches the elements by
  name. So each element of a list MUST have its own name.

- Each other element becomes a list with one entry for each simulation,
  named `sim_0001`, `sim_0002` and so on. This includes an atomic
  vector, a matrix, a factor, a date and `NULL`.

Stacking stops when two simulations differ in the structure at one path,
and the error names the path and the two simulations. It also stops on a
data.frame with a column `sim`, because pbn adds that column.

The `manifest` is that of simulation 1. In it, `sim` and `n_rows_data`
are `NA`, `sims` is the number of simulations, and `stacked` is `TRUE`.
The columns `from_cache` and `cache_key` of `data_cache` are `NA`. The
counts `n_rows_before_exclusions` and `n_rows_after_exclusions` are `NA`
too. The element `n_rows_by_sim` holds the counts of each simulation. It
is a data.frame with one row for each simulation and data element. Its
columns are `sim`, `data`, `n_rows_data`, `n_rows_before_exclusions` and
`n_rows_after_exclusions`. The rule `no_row_per_person` of
[`check_results()`](https://www.rwhite.no/pbn/reference/check_results.md)
reads it.

## Two files

`save_results()` writes the results of the kinds in `gitexclude` to
`results_gitexclude.qs2`, beside `results.qs2`, and to
`sim_<kkkk>_gitexclude.qs2`, beside each `sim_<kkkk>.qs2`. The section
The files shows them. `results.qs2` and `sim_<kkkk>.qs2` hold
`manifest`, `exclusions` and every other kind. Use `gitexclude` for a
kind with fitted models, imputations or one row for each person. The
project's `.gitignore` MUST hold the line `*gitexclude*`, so that git
never tracks that file.
[`read_results()`](https://www.rwhite.no/pbn/reference/read_results.md)
reads both files.

These rules apply:

- `save_results()` stops before it writes anything when a name in
  `gitexclude` is not an analysis kind of `p`.

- In a git repository, it also stops when git does not ignore
  `results_gitexclude.qs2`.

- [`check_results()`](https://www.rwhite.no/pbn/reference/check_results.md)
  checks `results.qs2` and `sim_<kkkk>.qs2`, and not the files of
  `gitexclude`.

- The simulations of the two files are stacked apart, with the rules of
  the section Stacking.

- Without kinds in `gitexclude`, `save_results()` deletes an old
  `results_gitexclude.qs2` of the scenario, and prints one line. The old
  `sim_<kkkk>_gitexclude.qs2` files go with the rest of
  `sims_gitexclude/`, see the section The files.

## See also

[`vignette("pbn")`](https://www.rwhite.no/pbn/articles/pbn.md), which
builds and runs a small fake project.

Other results:
[`check_results()`](https://www.rwhite.no/pbn/reference/check_results.md),
[`read_results()`](https://www.rwhite.no/pbn/reference/read_results.md),
[`render_run()`](https://www.rwhite.no/pbn/reference/render_run.md)

## Examples

``` r
home <- file.path(tempdir(), "pbn_save_example")
dir.create(file.path(home, "v01_functions"), recursive = TRUE)
dir.create(file.path(home, "data"))
writeLines("# The run script.", file.path(home, "v01_run.R"))
writeLines(
  c(
    "CFG <- list(scenarios = data.frame(scenario = c('real', 'null'), sims = 1))",
    "expect_null <- function(results) {",
    "  data.frame(assertion = 'one row', expected = 1,",
    "    observed = nrow(results$means), pass = nrow(results$means) == 1)",
    "}"
  ),
  file.path(home, "v01_functions", "_cfg.R")
)
env <- new.env()
pbn::initialize_project("v01", home, file.path(home, "data"), env = env)
#> You are NOT sourcing into .GlobalEnv. All functions will be sourced into an environment that is returned from this function.
#> Sourcing all code inside /tmp/RtmpZRWtob/pbn_save_example/v01_functions into 

scenarios <- pbn::scenarios(env$CFG)
p <- plnr::Plan$new()
pbn::add_data(p, "d", fn = function(scenario, sim) {
  return(data.frame(x = stats::rnorm(5)))
})
pbn::add_analyses(p, scenarios, name = "means", fn = function(data, argset) {
  return(data.frame(mean_x = mean(data$d$x)))
})
r <- pbn::run_plan(p)
pbn::save_results(r, p, scenarios)
#> Assertions of the scenario "null":
#>  assertion expected observed pass
#>    one row        1        1 TRUE
#> pbn: wrote the run log v01_results/run.md.
names(pbn::read_results("v01", home = home))
#> [1] "manifest" "means"   

unlink(home, recursive = TRUE)
```
