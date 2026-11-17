# Run a plan

`run_plan()` runs every analysis of the plnr plan `p`, in this R session
or on `workers` new R sessions. It stops when an analysis fails, and
then it prints each failed analysis with its error.

## Usage

``` r
run_plan(p, workers = 1)
```

## Arguments

- p:

  A plan from `plnr::Plan$new()`, with data from
  [`add_data()`](https://www.rwhite.no/pbn/reference/add_data.md) and
  analyses from
  [`add_analyses()`](https://www.rwhite.no/pbn/reference/add_analyses.md).

- workers:

  The number of R sessions that run the analyses, one whole number of 1
  or more.

## Value

Invisibly, a named list with one element for each analysis of the plan,
in the order of the plan. Element `i` is the value of the action
function of analysis `i`. Give it to
[`save_results()`](https://www.rwhite.no/pbn/reference/save_results.md).

## Details

With `workers = 1`, `run_plan()` calls `p$run_all()` in this R session.

With `workers` 2 or more, `run_plan()` needs the packages doParallel and
foreach. It starts a PSOCK cluster of `workers` new R sessions with
[`parallel::makePSOCKcluster()`](https://rdrr.io/r/parallel/makeCluster.html),
registers it with
[`doParallel::registerDoParallel()`](https://rdrr.io/pkg/doParallel/man/registerDoParallel.html),
and calls `p$run_all()` with the foreach path of plnr. A PSOCK cluster
does not fork, so it also works on Windows. Before the run, `run_plan()`
gives each worker these:

- the library paths of this R session, from
  [`.libPaths()`](https://rdrr.io/r/base/libPaths.html).

- the packages attached in this R session, in the order of
  [`search()`](https://rdrr.io/r/base/search.html), for example
  data.table after [`library(data.table)`](https://r-datatable.com) in
  the run script.

- the kind of random number generator of this R session, from
  [`RNGkind()`](https://rdrr.io/r/base/Random.html).

- every object of the environment that
  [`initialize_project()`](https://www.rwhite.no/pbn/reference/initialize_project.md)
  sourced the functions folder into, `.GlobalEnv` by default. So a data
  function and an action function find `CFG` and the other functions of
  the folder. A plnr plan and `.Random.seed` are left out. Each object
  is copied to each worker, so keep large objects out of that
  environment.

- the paths in
  [`org::project`](https://www.rwhite.no/org/reference/project.html),
  such as `org::project$data_raw`.

`run_plan()` does not copy the R options, for example
`options(digits = 3)`, and no other state of this R session.

After the run, `run_plan()` stops the cluster and registers the
sequential foreach backend, also after an error.

plnr calls each data function once for each scenario and simulation, see
[`add_data()`](https://www.rwhite.no/pbn/reference/add_data.md). Each
data function and each analysis sets its own seed before it runs, see
[`add_analyses()`](https://www.rwhite.no/pbn/reference/add_analyses.md).
Take a function that depends only on its arguments, the objects of the
functions folder such as `CFG`, the attached packages and the seed. Its
results with `workers = 1` and with `workers = 2` are
[`identical()`](https://rdrr.io/r/base/identical.html). A function that
reads an R option or another state of the main R session can give other
results on a worker. A data.table loses its self-reference on the way
back from a worker, and `run_plan()` gives it a new one with
[`data.table::setalloccol()`](https://rdrr.io/pkg/data.table/man/truelength.html).
It does this for each data.table in a value or in a plain list in a
value, at any depth.

`run_plan()` runs every analysis, also after an analysis fails. An error
in an action function fails that analysis, and an error in a data
function fails each analysis that gets the data. This is the same with 1
worker and with more. After the run, `run_plan()` prints each failed
analysis with its name and its full error message with
[`cat()`](https://rdrr.io/r/base/cat.html), and then it stops. R cuts an
error message at 1000 bytes by default, and
[`cat()`](https://rdrr.io/r/base/cat.html) does not. pbn then saves
nothing, because
[`save_results()`](https://www.rwhite.no/pbn/reference/save_results.md)
needs the value of `run_plan()`. The results files of an earlier run
stay as they are.

The value carries the attribute `pbn_run`, a stamp of the plan. It holds
no count of the runs, so two runs of one plan can be
[`identical()`](https://rdrr.io/r/base/identical.html).
[`save_results()`](https://www.rwhite.no/pbn/reference/save_results.md)
stops on a value without the stamp of its plan.

`run_plan()` sets `use_foreach` of the plan: `FALSE` for 1 worker and
`TRUE` for more.

After
[`initialize_project()`](https://www.rwhite.no/pbn/reference/initialize_project.md),
`run_plan()` first computes the md5 of each raw data file again when its
size or its modification time changed. The value carries the md5 in its
stamp, and
[`save_results()`](https://www.rwhite.no/pbn/reference/save_results.md)
writes it to `manifest$data_md5`.

For a data function with `cache = TRUE`, `run_plan()` needs
[`initialize_project()`](https://www.rwhite.no/pbn/reference/initialize_project.md)
first, and it prints one line for each scenario and simulation. The
section Cache of
[`add_data()`](https://www.rwhite.no/pbn/reference/add_data.md) gives
the rules. A data function that returns a
[`cohort::CohortPipeline`](https://www.rwhite.no/cohort/reference/CohortPipeline.html)
runs on the worker, and only its data.table and its exclusion table come
back. The section A cohort as the data of
[`add_data()`](https://www.rwhite.no/pbn/reference/add_data.md) gives
the rules.

## See also

[`vignette("pbn")`](https://www.rwhite.no/pbn/articles/pbn.md), which
builds and runs a small fake project.

Other plan:
[`add_analyses()`](https://www.rwhite.no/pbn/reference/add_analyses.md),
[`add_data()`](https://www.rwhite.no/pbn/reference/add_data.md),
[`cohort_data()`](https://www.rwhite.no/pbn/reference/cohort_data.md)

## Examples

``` r
CFG <- list(scenarios = data.frame(scenario = c("real", "null"), sims = 2))
p <- plnr::Plan$new()
pbn::add_data(p, "d", fn = function(scenario, sim) {
  return(data.frame(x = stats::rnorm(5)))
})
pbn::add_analyses(
  p,
  pbn::scenarios(CFG),
  name = "mean_x",
  fn = function(data, argset) return(data.frame(mean = mean(data$d$x)))
)
r <- pbn::run_plan(p)
r[["null__sim_0002__mean_x__1"]]
#>          mean
#> 1 -0.06696949
```
