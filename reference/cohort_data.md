# Work on the data of a cohort and keep its exclusion table

`cohort_data()` gives the data of a
[`cohort::CohortPipeline`](https://www.rwhite.no/cohort/reference/CohortPipeline.html)
after its exclusions. `keep_exclusions()` puts the exclusion table of
the pipeline on the value of a step after the cohort, such as an
imputation.

## Usage

``` r
cohort_data(cp)

keep_exclusions(x, cp)
```

## Arguments

- cp:

  A
  [`cohort::CohortPipeline`](https://www.rwhite.no/cohort/reference/CohortPipeline.html)
  after its exclusions.

- x:

  The value of the step after the cohort, any R object other than
  `NULL`. For example a data.table, a list of imputed data sets or a
  `mids` object.

## Value

`cohort_data()`: a data.table. `keep_exclusions()`: `x` with the
attributes `pbn_exclusions` and `pbn_n_before`.

## Details

A step after the cohort reads the data with `cohort_data()`. As the step
`after_cohort` of
[`add_data()`](https://www.rwhite.no/pbn/reference/add_data.md), it
returns its value, and pbn puts the exclusion table on it:

    data_impute <- function(cp) {
      d <- pbn::cohort_data(cp)
      lapply(1:5, function(k) impute_once(d))
    }

At the end of a data function `fn`, the step returns
`pbn::keep_exclusions(imp, cp)`, so that the table stays.

`cohort_data()` returns `cp$get_included(<cohort>)`. `<cohort>` is the
one cohort of the pipeline that no other cohort branches from, the
cohort that
[`add_data()`](https://www.rwhite.no/pbn/reference/add_data.md) gives
the analyses. It stops on a pipeline with more than one such cohort.

`keep_exclusions()` returns `x` with two attributes. `pbn_exclusions`
holds the exclusion table of `cp`, and `pbn_n_before` holds
`cp$n_total()`, the number of rows given to the pipeline. The analyses
get `x`, and
[`save_results()`](https://www.rwhite.no/pbn/reference/save_results.md)
writes the table to the element `exclusions`. The section A cohort as
the data of
[`add_data()`](https://www.rwhite.no/pbn/reference/add_data.md) gives
the columns.

For a value that is not a data.frame, the check of
[`run_plan()`](https://www.rwhite.no/pbn/reference/run_plan.md) for an
analysis that changes the shared data in place checks nothing.
`manifest$n_rows_data` is the number of rows of one data set when `x` is
a list of data.frames with one number of rows, and `NA` otherwise.

A function that calls these two runs on each worker of
[`run_plan()`](https://www.rwhite.no/pbn/reference/run_plan.md), so each
worker needs pbn installed.

## See also

[`vignette("pbn")`](https://www.rwhite.no/pbn/articles/pbn.md), which
imputes after the cohort.

Other plan:
[`add_analyses()`](https://www.rwhite.no/pbn/reference/add_analyses.md),
[`add_data()`](https://www.rwhite.no/pbn/reference/add_data.md),
[`get_data()`](https://www.rwhite.no/pbn/reference/get_data.md),
[`progress()`](https://www.rwhite.no/pbn/reference/progress.md),
[`run_plan()`](https://www.rwhite.no/pbn/reference/run_plan.md)

## Examples

``` r
d <- data.table::data.table(id = 1:6, age = c(15, 30, 41, 17, 52, 60))
cp <- cohort::CohortPipeline$new(d)
cp$exclude_and_track("root", "age under 18", "age < 18")
adults <- pbn::cohort_data(cp)
imp <- list(adults, adults)
attr(pbn::keep_exclusions(imp, cp), "pbn_exclusions")
#>   data         step n_before n_excluded n_after
#> 1 <NA> age under 18        6          2       4
```
