# Add a data function to a plan

`add_data()` adds the data function `fn` to the plnr plan `p` with
`p$add_data()`. plnr calls it once for each scenario and simulation, and
gives the data to each analysis of that scenario and simulation.

## Usage

``` r
add_data(p, name, fn, cache = FALSE)
```

## Arguments

- p:

  A plan from `plnr::Plan$new()`.

- name:

  The name of the data. Each action function gets it as `data[[name]]`.

- fn:

  The data function, normally
  `function(scenario, sim) data_raw(scenario, sim) |> data_clean() |> data_cohort()`.

- cache:

  `TRUE` saves the value of `fn` on disk for each scenario and
  simulation. The next run reads it while the cache key is the same. See
  the section Cache.

## Value

Invisibly, the name of the data.

## Details

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

`name` MUST be one non-empty string, and it MUST NOT be `"hash"`,
because plnr adds `data$hash` to the data of each analysis. Two data
functions of one plan MUST NOT have the same name. Add data with
`add_data()`, not with `p$add_data()`.
[`save_results()`](https://www.rwhite.no/pbn/reference/save_results.md)
reads the data functions of `add_data()` only.

While [`run_plan()`](https://www.rwhite.no/pbn/reference/run_plan.md)
runs, an error in `fn` fails each analysis that gets the data. When you
call `p$get_data()` by hand, the error stops the call.

## A cohort as the data

`fn` MAY return a
[`cohort::CohortPipeline`](https://www.rwhite.no/cohort/reference/CohortPipeline.html)
after its exclusions. The analyses then get `data[[name]]` as the
data.table of `$get_included(<cohort>)`. `<cohort>` is the one cohort of
the pipeline that no other cohort branches from. A pipeline with more
than one such cohort fails the data. Each other value goes to the
analyses as it is.

A step after the cohort, such as an imputation, reads the data of the
cohort with
[`cohort_data()`](https://www.rwhite.no/pbn/reference/cohort_data.md).
It returns its value through
[`keep_exclusions()`](https://www.rwhite.no/pbn/reference/cohort_data.md),
which puts the exclusion table on it. The value MAY be any R object, for
example a list of imputed data sets.

pbn keeps the exclusion table of the cohort, and
[`save_results()`](https://www.rwhite.no/pbn/reference/save_results.md)
stores it in the element `exclusions` of the results. The table has one
row for each exclusion step of the cohort and of the cohorts above it,
in the order of the steps. It is a data.frame with these columns:

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

- for `"real"` only, the md5 of each file directly in `data_raw` at the
  time of the use. pbn hashes a file again only when its size or its
  modification time differs from the last record.

- the md5 of each file of `<version>_functions/` whose name starts with
  `data_`, for example `data_raw.R`, `data_clean.R`, `data_cohort.R` and
  `data_impute.R`.
  [`initialize_project()`](https://www.rwhite.no/pbn/reference/initialize_project.md)
  computes them before it sources the files.

- the code of `fn`, deparsed without its source references.

- `CFG$data`, when `CFG` has the element `data`. Put the config that the
  data code reads there. A change in another element of `CFG` does not
  change the key.

- the versions of R and pbn. The version of R covers the base packages.

- the three parts of [`RNGkind()`](https://rdrr.io/r/base/Random.html).

The key file also records the version of each package that was loaded in
the R session that computed the value, after `fn` returned. On a worker,
that is the worker. The record leaves out the base packages. It also
leaves out the packages that run the plan, tests and documents: pbn,
plnr, foreach, doParallel, iterators, qs2, digest, uuid, progress,
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
the value from the cache and does not call `fn`. Each analysis sets its
own seed, so its results are the same as with a new value. Otherwise, or
when a cache file cannot be read, it calls `fn` and writes the cache. To
compute every value again, delete the folder
`<version>_cache_gitexclude/`. No argument does that.

For each scenario and simulation,
[`run_plan()`](https://www.rwhite.no/pbn/reference/run_plan.md) prints
one line with [`cat()`](https://rdrr.io/r/base/cat.html). The line says
that the data came from the cache, with the date and time of the cache
file. Or it says that pbn computed it, and why. The reason names each
changed `data_*.R` file, `CFG$data`, the raw data, the code of `fn` or
the settings. It can also name the kind of random number generator, or
each package with another version. With `workers = 1`, the line comes
before the data function runs. With more workers,
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
[`run_plan()`](https://www.rwhite.no/pbn/reference/run_plan.md)

## Examples

``` r
p <- plnr::Plan$new()
pbn::add_data(p, "d", fn = function(scenario, sim) {
  return(data.frame(x = stats::rnorm(3)))
})
```
