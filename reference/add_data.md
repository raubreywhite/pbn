# Add data to a plan

`add_data()` adds the data `name` to the plnr plan `p`, as the steps
`raw`, `clean`, `cohort` and `after_cohort`, or as one data function
`fn`. plnr calls it once for each scenario and simulation, and gives the
data to each analysis of that scenario and simulation.

## Usage

``` r
add_data(
  p,
  name,
  fn = NULL,
  raw = NULL,
  clean = NULL,
  cohort = NULL,
  after_cohort = NULL,
  cache = FALSE
)
```

## Arguments

- p:

  A plan from `plnr::Plan$new()`.

- name:

  The name of the data. Each action function gets it as `data[[name]]`.

- fn:

  The free form: one data function, for data that does not fit the
  steps. Give `fn` or `raw`, not both.

- raw:

  The first step, normally `data_raw`, a `function(scenario, sim)` that
  reads the real data or makes fake data.

- clean:

  Optional. The step that gets the value of `raw`, normally
  `data_clean`, for example for names and types. It MUST return a
  data.frame with the same number of rows.

- cohort:

  Optional. The step that gets the value of `clean`, or of `raw` without
  `clean`, normally `data_cohort`. It MUST return a
  [`cohort::CohortPipeline`](https://www.rwhite.no/cohort/reference/CohortPipeline.html)
  that starts with every row that it got.

- after_cohort:

  Optional, and only with `cohort`. The step that gets the
  [`cohort::CohortPipeline`](https://www.rwhite.no/cohort/reference/CohortPipeline.html),
  for example an imputation. It returns the data for the analyses, and
  pbn puts the exclusion table of the cohort on it. See the section A
  cohort as the data.

- cache:

  `TRUE` saves the data on disk for each scenario and simulation. The
  next run reads it while the cache key is the same. See the section
  Cache.

## Value

Invisibly, the name of the data.

## Details

In the steps form, each rule for `fn` in this section and in the section
Cache applies to `raw`.

plnr keys a data function on its arguments without a default. These are
the key fields. Each key field of `fn` MUST be `scenario`, `sim` or a
setting column of `CFG$scenarios`. Otherwise
[`run_plan()`](https://www.rwhite.no/pbn/reference/run_plan.md) stops
before it runs anything. plnr calls `fn` once for each set of values of
the key fields, and gives each analysis with those values the same data.

`add_data()` stops on these arguments of `fn`:

- `scenario` or `sim` with a default. plnr does not key on it, so every
  scenario or simulation would get the same data.

- `sim` without `scenario`. Every scenario would get the same data.

A data function without `scenario` and `sim`, for example one that reads
a lookup table, runs once for the whole run and sets no seed.

pbn adds `fn` in a wrapper. The wrapper has the key fields of `fn` as
its arguments, so plnr keys it in the same way. It calls `fn` with them
by name, so another argument of `fn` with a default keeps its default.
When `sim` is an argument, the wrapper calls `set.seed(sim)` before
`fn`. The data of simulation `k` is thus the same in each run, and does
not depend on the other scenarios of the run. A function that is called
with no `sim` sets no seed.

The wrapper needs no object of the R session that adds it.
[`run_plan()`](https://www.rwhite.no/pbn/reference/run_plan.md) gives
each worker the objects that the functions folder defines.

`name` MUST be one syntactic name that starts with a letter and holds
only letters, digits, `.` and `_`, for example `"d"`. pbn names each
step of the data `<name>$<step>` in the code that `fake_md5` hashes. A
`$` in a name could thus match a step of another data. `name` MUST NOT
be `"hash"`, because plnr adds `data$hash` to the data of each analysis.
Two data functions of one plan MUST NOT have the same name. Add data
with `add_data()`, not with `p$add_data()`.
[`save_results()`](https://www.rwhite.no/pbn/reference/save_results.md)
reads the data functions of `add_data()` only.

While [`run_plan()`](https://www.rwhite.no/pbn/reference/run_plan.md)
runs, an error in `fn` or in a step fails each analysis that gets the
data. When you call
[`get_data()`](https://www.rwhite.no/pbn/reference/get_data.md) or
`p$get_data()` by hand, the error stops the call.

## The steps

pbn runs the steps in the order `raw`, `clean`, `cohort` and
`after_cohort`, and gives the value of each step to the next one. Each
step after `raw` MUST take exactly one argument without a default, or
`add_data()` stops. pbn calls `set.seed(sim)` once, before `raw`. So
`raw = data_raw, clean = data_clean` draws the same numbers as
`fn = function(scenario, sim) data_clean(data_raw(scenario, sim))`.

pbn sees the value between the steps, so it checks the rule that the
cleaning drops no row. Each exclusion goes through the cohort, which
counts it. These values fail the data, as an error in `fn` does:

- `raw` returns a data.frame, and `clean` returns a value that is not a
  data.frame, or a data.frame with another number of rows.

- `cohort` returns a value that is not a
  [`cohort::CohortPipeline`](https://www.rwhite.no/cohort/reference/CohortPipeline.html).

- `cohort` gets a data.frame, and its pipeline starts with another
  number of rows, `cp$n_total()`.

The error gives both numbers of rows. The checks compare the numbers of
rows only, so the order of the rows and the columns MAY change. The
checks compare the number of rows, not which rows: a step that drops one
row and repeats another passes.

When `raw` returns a value that is not a data.frame, pbn does not check
`clean`. An example is a list of two tables that `clean` joins. That
list has no number of rows to compare, and it is a valid start. pbn does
not check the rows of `after_cohort`.

Use `fn` for data that does not fit the steps, for example data that
joins two cohorts. pbn then checks no rows inside `fn`.

An exclusion MAY apply to the rows of a reshape, for example one row for
each person and time point. Add these rows as a second data element,
with a second call of `add_data()`. Its `raw` builds the reshaped rows,
and its `cohort` applies the exclusion, so the cohort counts it. Its
`raw` MAY reshape the value of the `raw` of the first element. pbn sets
the seed to `sim` before each data element, so both draw the same
numbers.

## A cohort as the data

`fn` or the step `cohort` MAY return a
[`cohort::CohortPipeline`](https://www.rwhite.no/cohort/reference/CohortPipeline.html)
after its exclusions. The analyses then get `data[[name]]` as the
data.table of `$get_included(<cohort>)`. `<cohort>` is the one cohort of
the pipeline that no other cohort branches from. A pipeline with more
than one such cohort fails the data. Each other value goes to the
analyses as it is.

The step `after_cohort` reads the data of the cohort with
[`cohort_data()`](https://www.rwhite.no/pbn/reference/cohort_data.md),
and returns its value. The value MAY be any R object other than `NULL`,
for example a list of imputed data sets. pbn puts the exclusion table of
the cohort and its number of rows at the start on the value, as
[`keep_exclusions()`](https://www.rwhite.no/pbn/reference/cohort_data.md)
does. These replace the attributes that the step set. So a call of
[`keep_exclusions()`](https://www.rwhite.no/pbn/reference/cohort_data.md)
in the step changes nothing, and a table from another cohort does not
stay. The step MAY return the
[`cohort::CohortPipeline`](https://www.rwhite.no/cohort/reference/CohortPipeline.html)
that it got. Another
[`cohort::CohortPipeline`](https://www.rwhite.no/cohort/reference/CohortPipeline.html)
fails the data, because its exclusion table would replace that of the
step `cohort`.

A step after the cohort in `fn` returns its value through
[`keep_exclusions()`](https://www.rwhite.no/pbn/reference/cohort_data.md),
which puts the exclusion table on it.

pbn keeps the exclusion table of the cohort, and
[`save_results()`](https://www.rwhite.no/pbn/reference/save_results.md)
stores it in the element `exclusions` of the results. The table has one
row for each exclusion step of the cohort and of the cohorts above it,
in the order of the exclusion steps. It is a data.frame with these
columns:

- `data`, the name of the data.

- `step`, the reason of `$exclude_and_track()`.

- `n_before`, `n_excluded` and `n_after`, the number of rows before the
  step, excluded by the step, and after the step. `n_excluded` and
  `n_after` are `n_excluded` and `n_remaining` of `$consort()`.

The value carries the table in its attribute `pbn_exclusions`, and the
number of rows given to the pipeline in its attribute `pbn_n_before`.
[`save_results()`](https://www.rwhite.no/pbn/reference/save_results.md)
writes that number to `manifest$n_rows_before_exclusions`. The
attributes are how both come back from a worker: the R6 object stays on
the worker.

## Cache

With `cache = TRUE`, `fn` MUST have the arguments `scenario` and `sim`
without a default. Call
[`initialize_project()`](https://www.rwhite.no/pbn/reference/initialize_project.md)
first. For each scenario and simulation,
[`run_plan()`](https://www.rwhite.no/pbn/reference/run_plan.md) writes
the value to
`<home>/<version>_cache_gitexclude/<name>/<scenario>/sim_<kkkk>.qs2`.
The value is the data.table and its exclusion table for a cohort. The
cache key goes to `sim_<kkkk>_key.qs2` in the same folder. The cache key
is the md5 of these parts:

- the scenario, the simulation and the settings of the scenario.

- for `"real"` only, the fingerprint of the data at the time of the use.
  It holds the name `<folder name>/<path in the folder>`, the size and
  the modification time of each file under each folder of `folders` of
  [`initialize_project()`](https://www.rwhite.no/pbn/reference/initialize_project.md).
  pbn reads no content of a data file. The path of a folder is not in
  the key. So the same files in another place give the same key when
  they keep their modification times. A copy with new times looks like
  changed data, so the data is computed again. A file that changed in
  place, and kept its size and its modification time, is not seen.

- the md5 of each file of `<version>_functions/` whose name starts with
  `data_`, for example `data_raw.R`, `data_clean.R`, `data_cohort.R` and
  `data_impute.R`.
  [`initialize_project()`](https://www.rwhite.no/pbn/reference/initialize_project.md)
  computes them before it sources the files.

- the code of `fn`, or of each step, deparsed without its source
  references.

- `CFG$data`, when `CFG` has the element `data`. Put the config that the
  data code reads there. A change in another element of `CFG` does not
  change the key.

- the versions of R and pbn. The version of R covers the base packages.

- the three parts of [`RNGkind()`](https://rdrr.io/r/base/Random.html).

The key file also records the version of each package that was loaded in
the R session that computed the value, after `fn` returned. On a worker,
that is the worker. The record leaves out the base packages. It also
leaves out the packages that run the plan, tests and documents: pbn,
plnr, foreach, doParallel, doFuture, future, future.apply, globals,
listenv, parallelly, progressr, iterators, qs2, digest, uuid, progress,
testthat, pkgload, devtools, callr, processx, ps, knitr, rmarkdown and
evaluate. A later use compares each recorded version with
[`utils::packageVersion()`](https://rdrr.io/r/utils/packageDescription.html)
now. A package with another version, or one that is not installed now,
computes the value again.

Each use of the cache stops when a `data_*.R` file on disk differs from
the file that
[`initialize_project()`](https://www.rwhite.no/pbn/reference/initialize_project.md)
hashed and sourced: changed, added or removed. The error names the file.
Run
[`initialize_project()`](https://www.rwhite.no/pbn/reference/initialize_project.md)
again. A file with its recorded size and modification time is not hashed
again, so a change that keeps both is not found.

Keep the data code in the files `data_*.R`. The key holds no other file.
A function of the data code can be in another file. A change in it then
leaves the key the same, and the run reads the old data. This is the one
change of the data code that the cache does not find.

When the key is the same,
[`run_plan()`](https://www.rwhite.no/pbn/reference/run_plan.md) reads
the value from the cache and does not call `fn` or a step. Each analysis
sets its own seed, so its results are the same as with a new value.
Otherwise, or when a cache file cannot be read, it calls `fn` or the
steps and writes the cache. To compute every value again, delete the
folder `<version>_cache_gitexclude/`. No argument does that.

For each scenario and simulation,
[`run_plan()`](https://www.rwhite.no/pbn/reference/run_plan.md) prints
one line with [`cat()`](https://rdrr.io/r/base/cat.html). The line says
that the data came from the cache, with the date and time of the cache
file. Or it says that pbn computed it, and why. The reason names each
changed `data_*.R` file, `CFG$data`, or the settings. It names changed
raw data with its folders, for example "the raw data in the folder
lookup". It names a changed `fn` as "the data function of add_data()",
and each changed step as, for example, "the function given as `clean`".
It can also name the kind of random number generator, or each package
with another version. With `workers = 1`, the line comes before the data
function runs. With more workers,
[`run_plan()`](https://www.rwhite.no/pbn/reference/run_plan.md) prints
the lines after the run, in the order of the plan.

[`run_plan()`](https://www.rwhite.no/pbn/reference/run_plan.md) writes
each file to a temporary file in the same folder, and renames it. plnr
runs the data of one scenario and simulation in one task, so two workers
of one run never write the same file. When two runs write one file at
the same time, the last rename wins, and each file is whole. The cache
file holds its key too, so a cache file and a key file from two runs
give a new computation.

The cache folder holds data for each person. When the project is a git
repository, git MUST ignore it:
[`run_plan()`](https://www.rwhite.no/pbn/reference/run_plan.md) stops
before it runs anything when git does not ignore the cache files. Add
the line `*gitexclude*` to the `.gitignore` of the project.

`p$get_data()` and `p$run_one()` by hand use the cache too, and print
the same line. pbn computes the key at the time of the call, with the
same rules.

## See also

[`vignette("pbn")`](https://www.rwhite.no/pbn/articles/pbn.md), which
builds and runs a small fake project.

Other plan:
[`add_analyses()`](https://www.rwhite.no/pbn/reference/add_analyses.md),
[`cohort_data()`](https://www.rwhite.no/pbn/reference/cohort_data.md),
[`get_data()`](https://www.rwhite.no/pbn/reference/get_data.md),
[`progress()`](https://www.rwhite.no/pbn/reference/progress.md),
[`run_plan()`](https://www.rwhite.no/pbn/reference/run_plan.md)

## Examples

``` r
p <- plnr::Plan$new()
pbn::add_data(
  p,
  "d",
  raw = function(scenario, sim) return(data.frame(X = stats::rnorm(3))),
  clean = function(d) return(stats::setNames(d, tolower(names(d))))
)
```
