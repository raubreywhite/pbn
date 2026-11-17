# Show the progress inside one analysis

`progress()` shows how far a long analysis is, for example a bootstrap
loop, on the line of its scenario in the display of
[`run_plan()`](https://www.rwhite.no/pbn/reference/run_plan.md). Outside
a run of [`run_plan()`](https://www.rwhite.no/pbn/reference/run_plan.md)
it only checks its arguments.

## Usage

``` r
progress(done, total, label = NULL)
```

## Arguments

- done:

  The number of steps done, one number of 0 or more.

- total:

  The number of steps of the analysis, one number of 1 or more.

- label:

  Optional. A short text, for example `"bootstrap"`.

## Value

`NULL`, invisibly.

## Details

Call `progress()` in the loop of an action function. The line of the
scenario then shows the kind of the analysis, the values of its extra
dimensions, `label`, `done`/`total` and the percent. When at least 3
steps and at least 10% of the steps are done, it also shows the time
left. That time comes from the rate of the steps of this analysis. The
count of the finished analyses of the scenario does not change.

After the first failed analysis of the run, no new analysis starts, and
an analysis that runs goes on to its end. Its calls of `progress()`
still show.

`progress()` drops a call less than 0.5 seconds after the last one that
it showed. So a call in each pass of a fast loop costs little. It checks
its arguments at each call, also outside
[`run_plan()`](https://www.rwhite.no/pbn/reference/run_plan.md), so a
wrong argument stops the analysis however fast it runs.

Apart from the check of its arguments, `progress()` does nothing in
these cases:

- outside
  [`run_plan()`](https://www.rwhite.no/pbn/reference/run_plan.md), for
  example when you step through an analysis by hand, or call the action
  function yourself.

- in a data function.

- in a forked child process, for example inside
  [`parallel::mclapply()`](https://rdrr.io/r/parallel/mclapply.html).
  Several children would write to the one connection of the worker at
  the same time. Call `progress()` in the loop of the analysis itself,
  for example between chunks of the replicates, as the example shows.

## See also

[`vignette("pbn")`](https://www.rwhite.no/pbn/articles/pbn.md), whose
section Progress shows the display.

Other plan:
[`add_analyses()`](https://www.rwhite.no/pbn/reference/add_analyses.md),
[`add_data()`](https://www.rwhite.no/pbn/reference/add_data.md),
[`cohort_data()`](https://www.rwhite.no/pbn/reference/cohort_data.md),
[`get_data()`](https://www.rwhite.no/pbn/reference/get_data.md),
[`run_plan()`](https://www.rwhite.no/pbn/reference/run_plan.md)

## Examples

``` r
fit_boot <- function(data, argset) {
  n_boot <- 200
  chunks <- split(seq_len(n_boot), ceiling(seq_len(n_boot) / 50))
  est <- numeric(0)
  for (chunk in chunks) {
    # With forks, for example parallel::mclapply(chunk, ...), call
    # progress() here, between the chunks.
    est <- c(est, vapply(chunk, function(b) {
      return(mean(sample(data$d$x, replace = TRUE)))
    }, numeric(1)))
    pbn::progress(length(est), n_boot, "bootstrap")
  }
  return(data.frame(estimate = mean(data$d$x), se = stats::sd(est)))
}
# Outside run_plan(), progress() does nothing, so this call returns the
# value at once.
fit_boot(list(d = data.frame(x = stats::rnorm(20))), list())
#>    estimate        se
#> 1 0.2756986 0.2659695
```
