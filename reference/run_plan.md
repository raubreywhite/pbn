# Run a plan

`run_plan()` runs every analysis of the plnr plan `p`, in this R session
or on `workers` new R sessions. It stops the run at the first failed
analysis, and then it prints each failed analysis with its error.

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

With `workers` 2 or more, `run_plan()` needs the packages future and
doFuture. It starts a PSOCK cluster of `workers` new R sessions with
[`parallel::makePSOCKcluster()`](https://rdrr.io/r/parallel/makeCluster.html).
It runs `p$run_all()` with the foreach path of plnr, on
[`doFuture::registerDoFuture()`](https://doFuture.futureverse.org/reference/registerDoFuture.html)
with the future plan
[future::cluster](https://future.futureverse.org/reference/cluster.html)
on that cluster. A PSOCK cluster does not fork, so it also works on
Windows. Each future runs a batch of tasks of plnr on a worker, and each
task holds the analyses that get the data of one key. Each worker gets
about 4 batches when each data function with arguments has both
`scenario` and `sim`, or when no data function has arguments. Otherwise
each task is a batch of its own.

Each worker starts with the library paths of this R session, from
[`.libPaths()`](https://rdrr.io/r/base/libPaths.html). While it starts
the cluster, `run_plan()` sets the environment variable `R_LIBS` to
these paths, and then it sets `R_LIBS` back. So a worker also finds a
library that this R session added with
[`.libPaths()`](https://rdrr.io/r/base/libPaths.html), for example the
temporary library of `R CMD build`. Each worker runs with 1 thread.
While `run_plan()` starts the cluster, it sets the environment variables
`OMP_NUM_THREADS`, `OPENBLAS_NUM_THREADS` and `MKL_NUM_THREADS` to 1, as
it sets `R_LIBS`. Then it sets them back in this R session. OpenMP and
the BLAS libraries OpenBLAS and MKL read them when they start their
threads, so the variables MUST be there when the worker starts. The
section One thread for each worker gives the rules.

Before the run, `run_plan()` gives each worker these:

- the library paths of this R session again, with
  [`.libPaths()`](https://rdrr.io/r/base/libPaths.html).

- 1 thread: the three variables again, for a library that the worker
  loads later, and `data.table::setDTthreads(1)` when data.table is
  installed.

- the packages attached in this R session, in the order of
  [`search()`](https://rdrr.io/r/base/search.html), for example
  data.table after [`library(data.table)`](https://r-datatable.com) in
  the run script.

- the kind of random number generator of this R session, from
  [`RNGkind()`](https://rdrr.io/r/base/Random.html).

- the paths in
  [`org::project`](https://www.rwhite.no/org/reference/project.html),
  such as `org::project$home`.

- the folders of
  [`initialize_project()`](https://www.rwhite.no/pbn/reference/initialize_project.md),
  so that [`folder()`](https://www.rwhite.no/pbn/reference/folder.md)
  works on the worker. A worker then loads pbn.

A future starts with an empty global environment.
[`initialize_project()`](https://www.rwhite.no/pbn/reference/initialize_project.md)
sources the functions folder into an environment, `.GlobalEnv` by
default. Each batch takes every object of that environment to the global
environment of its worker. A data function and an action function then
find `CFG` and the other functions of the folder. A plnr plan and
`.Random.seed` are left out. Each batch takes a copy of each object, so
keep large objects out of that environment.

`run_plan()` does not copy the R options, for example
`options(digits = 3)`, and no other state of this R session. A future
sets the option `mc.cores` to 1. `run_plan()` removes it on a worker, so
[`parallel::mclapply()`](https://rdrr.io/r/parallel/mclapply.html) in an
analysis uses its default number of cores. The output, the messages and
the warnings of a worker come to this R session at the end of each
batch.

After the run, `run_plan()` stops the cluster and registers the
sequential foreach backend, also after an error. It sets the future plan
and the option `doFuture.rng.onMisuse` back to their values before the
run. Each data function and each analysis sets its own seed, so
`run_plan()` turns off the warning of doFuture about random numbers for
the run. plnr calls each data function once for each scenario and
simulation, see
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
value, at any depth. It does not do this for a data.table in a list
column of a data.frame. Such a value MAY differ in
[`identical()`](https://rdrr.io/r/base/identical.html) between 1 worker
and more.

An error in an action function fails that analysis, and an error in a
data function fails each analysis that gets the data. pbn saves no
results of a run with a failed analysis, so the first failed analysis
stops the run. The section Stop at the first failure gives the rules.
After the run, `run_plan()` prints each failed analysis with its name
and its full error message with
[`cat()`](https://rdrr.io/r/base/cat.html), and then it stops. R cuts an
error message at 1000 bytes by default, and
[`cat()`](https://rdrr.io/r/base/cat.html) does not. The error message
of `run_plan()` gives the number of failed analyses and of the analyses
that did not run. It also names the first failed analysis in the order
of the plan, and holds its error. pbn cuts that error to keep the
message below 900 bytes. When 2 or more analyses fail and all have the
same error, the message says so. The results files of an earlier run
stay as they are. After
[`initialize_project()`](https://www.rwhite.no/pbn/reference/initialize_project.md),
`run_plan()` writes the run log `<version>_results/run.md` with the
failures before it stops. The section The run log of
[`save_results()`](https://www.rwhite.no/pbn/reference/save_results.md)
describes it.

After
[`initialize_project()`](https://www.rwhite.no/pbn/reference/initialize_project.md),
`run_plan()` first replaces the run log with one that says that the run
has not finished, and deletes the errors file
`<version>_results/run_errors_gitexclude.md`. A call of `run_plan()`
without
[`save_results()`](https://www.rwhite.no/pbn/reference/save_results.md)
after it leaves this log.

`run_plan()` measures the elapsed time of each data step and of each
analysis where it runs, also on a worker, and the time of the clock of
the computer at its end. The times go to the record of the plan, and
[`save_results()`](https://www.rwhite.no/pbn/reference/save_results.md)
writes them to the run log. They are not in the value, so two runs stay
[`identical()`](https://rdrr.io/r/base/identical.html).

The value carries the attribute `pbn_run`, a stamp of the plan. It holds
no count of the runs, so two runs of one plan can be
[`identical()`](https://rdrr.io/r/base/identical.html).
[`save_results()`](https://www.rwhite.no/pbn/reference/save_results.md)
stops on a value without the stamp of its plan.

`run_plan()` sets `use_foreach` of the plan: `FALSE` for 1 worker and
`TRUE` for more.

After
[`initialize_project()`](https://www.rwhite.no/pbn/reference/initialize_project.md),
`run_plan()` first lists the files of the folders of the project again,
with their sizes and modification times. It reads no content of a file.
The value carries this fingerprint in its stamp, and
[`save_results()`](https://www.rwhite.no/pbn/reference/save_results.md)
writes it to `manifest$data_files`.

An analysis that changes the shared data in place fails, also for a
change of values such as `data$d[1, x := 0]`. The error names the data
and each changed column. An analysis that changes an object of the
functions folder in place fails too. So does an analysis that leaves a
new object in the global environment.
[`add_analyses()`](https://www.rwhite.no/pbn/reference/add_analyses.md)
gives the rules.

An error outside the wrappers of pbn ends the run. Examples are a worker
that stops, and an error of plnr or of the backend of the workers.
`run_plan()` then prints the whole error, and writes the run log with it
after
[`initialize_project()`](https://www.rwhite.no/pbn/reference/initialize_project.md).
It stops with one message that holds the error, cut below 900 bytes, and
saves nothing. The future plan, the foreach backend and the options go
back as after any run, and the cluster stops.

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

## One thread for each worker

Some packages start threads of their own, for example data.table,
fixest, a threaded BLAS and OpenMP code. By default, such a package uses
many cores. data.table, for example, uses half of the cores of the
computer. With `workers` 2 or more, each worker would do that, and the
computer would run far more threads than cores. So `run_plan()` makes
each worker use 1 thread. There is no argument to change it. An analysis
that needs more threads in its worker sets them in its own code, for
example `data.table::setDTthreads(4)` at the top of its action function.

With `workers = 1`, `run_plan()` changes no thread setting, and the
analyses run with the settings of this R session.

The results of the test projects of pbn are
[`identical()`](https://rdrr.io/r/base/identical.html) with 1 and 2
workers. pbn does not check this for other code. A threaded BLAS MAY sum
in another order with another number of threads. So a matrix product of
such a BLAS MAY differ in its last digits between `workers = 1` and more
workers. With 1 worker it has the threads of this R session, and with
more it has 1 thread.

## Stop at the first failure

A failed data function makes each analysis of its scenario and
simulation fail, and
[`save_results()`](https://www.rwhite.no/pbn/reference/save_results.md)
needs a run without a failed analysis. So no analysis after the first
failure can give a result, and `run_plan()` stops the run there. There
is no option to run on. These are the rules:

- After the first failed analysis, no new data function and no new
  analysis starts. An analysis whose data failed fails at once, also
  after the stop, without a call of its action function.

- With 1 worker, the stop comes directly after the failed analysis.

- With workers, the failed analysis creates a folder in the temporary
  folder of this R session. Each worker looks for it before each data
  function, and at most every 0.5 seconds before an analysis. pbn does
  not stop an analysis that runs. So the longest delay is the rest of
  the longest analysis that runs at the time of the failure, plus 0.5
  seconds. The batches that did not start still go to the workers, and
  each of them ends at once.

- Each analysis that did not run counts as not run. It is not a failure.

The display shows the stop at once, and the run log says that the run
stopped early and how many analyses did not run.

## Progress

`run_plan()` shows one line for each scenario: its finished analyses of
all its analyses, the failed ones among them, the time since its start,
and a note. The note is one of these:

- `"waiting"` before the scenario starts, and `"done"` at its end.

- `"stopping"` after the first failed analysis of the run, and
  `"stopped early"` with the analyses that did not run at the end.

- the last text of
  [`progress()`](https://www.rwhite.no/pbn/reference/progress.md) from
  an analysis of the scenario, while that analysis runs.

- otherwise the time left, from the analyses of the scenario that ended
  without a failure, after at least 3 and at least 10% of them.

The analyses that count the rows do not count. The display replaces the
progress bar of plnr. The header of the display in a terminal gives the
time since the start of the run. So does the start of each line of a
log, for example
`pbn: [12m 05s] null 522/1200 analyses, 8m 53s, about 12m left`.

In an interactive terminal, the display draws its lines again in place.
In RStudio and in another console without ANSI codes, it draws one line
again in place. It shows the lines of each scenario at the end. While
the display is shown, progressr holds back other output, messages and
warnings, and shows them above it.

Without an interactive session, for example with `Rscript` into a log,
in knitr and in testthat, the display only adds lines:

- nothing in the first 60 seconds of the run, other than the line of the
  first failed analysis. A short run without a failure adds no line.

- one line at once when the first analysis fails, also in the first 60
  seconds.

- after 60 seconds, a header, and one line for each scenario that
  started.

- one line when a scenario finishes.

- one line for a running scenario at most every 60 seconds. The line
  comes only when 10% more of the scenario is done, in analyses or in
  its last text of
  [`progress()`](https://www.rwhite.no/pbn/reference/progress.md).

A worker sends the count of its finished analyses at most every 0.5
seconds, and at the end of each scenario and simulation. The display
uses progressr with a handler of its own, inside
[`progressr::with_progress()`](https://progressr.futureverse.org/reference/with_progress.html).
It sets no progressr handler of the R session, so the handlers that
another package sets stay as they are.

## See also

[`vignette("pbn")`](https://www.rwhite.no/pbn/articles/pbn.md), which
builds and runs a small fake project.

Other plan:
[`add_analyses()`](https://www.rwhite.no/pbn/reference/add_analyses.md),
[`add_data()`](https://www.rwhite.no/pbn/reference/add_data.md),
[`cohort_data()`](https://www.rwhite.no/pbn/reference/cohort_data.md),
[`get_data()`](https://www.rwhite.no/pbn/reference/get_data.md),
[`progress()`](https://www.rwhite.no/pbn/reference/progress.md)

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
