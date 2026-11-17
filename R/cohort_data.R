#' Work on the data of a cohort and keep its exclusion table
#'
#' `cohort_data()` gives the data of a `cohort::CohortPipeline` after its
#' exclusions. `keep_exclusions()` puts the exclusion table of the pipeline on
#' the value of a step after the cohort, such as an imputation.
#'
#' @param cp A `cohort::CohortPipeline` after its exclusions.
#' @param x The value of the step after the cohort, any R object other than
#'   `NULL`. For example a data.table, a list of imputed data sets or a
#'   `mids` object.
#' @details
#' A step after the cohort reads the data with `cohort_data()`. As the step
#' `after_cohort` of [add_data()], it returns its value, and pbn puts the
#' exclusion table on it:
#'
#' ```r
#' data_impute <- function(cp) {
#'   d <- pbn::cohort_data(cp)
#'   lapply(1:5, function(k) impute_once(d))
#' }
#' ```
#'
#' At the end of a data function `fn`, the step returns
#' `pbn::keep_exclusions(imp, cp)`, so that the table stays.
#'
#' `cohort_data()` returns `cp$get_included(<cohort>)`. `<cohort>` is the one
#' cohort of the pipeline that no other cohort branches from, the cohort that
#' [add_data()] gives the analyses. It stops on a pipeline with more than one
#' such cohort.
#'
#' `keep_exclusions()` returns `x` with two attributes. `pbn_exclusions` holds
#' the exclusion table of `cp`, and `pbn_n_before` holds `cp$n_total()`, the
#' number of rows given to the pipeline. The analyses get `x`, and
#' [save_results()] writes the table to the element `exclusions`. The section
#' A cohort as the data of [add_data()] gives the columns.
#'
#' For a value that is not a data.frame, the check of [run_plan()] for an
#' analysis that changes the shared data in place checks nothing.
#' `manifest$n_rows_data` is the number of rows of one data set when `x` is a
#' list of data.frames with one number of rows, and `NA` otherwise.
#'
#' A function that calls these two runs on each worker of [run_plan()], so
#' each worker needs pbn installed.
#' @return `cohort_data()`: a data.table. `keep_exclusions()`: `x` with the
#'   attributes `pbn_exclusions` and `pbn_n_before`.
#' @examplesIf requireNamespace("cohort", quietly = TRUE)
#' d <- data.table::data.table(id = 1:6, age = c(15, 30, 41, 17, 52, 60))
#' cp <- cohort::CohortPipeline$new(d)
#' cp$exclude_and_track("root", "age under 18", "age < 18")
#' adults <- pbn::cohort_data(cp)
#' imp <- list(adults, adults)
#' attr(pbn::keep_exclusions(imp, cp), "pbn_exclusions")
#' @family plan
#' @seealso `vignette("pbn")`, which imputes after the cohort.
#' @export
cohort_data <- function(cp) {
  check_cohort(cp)
  return(cp$get_included(cohort_lineage(cp, NA_character_)$leaf))
}

#' @rdname cohort_data
#' @export
keep_exclusions <- function(x, cp) {
  check_cohort(cp)
  if (is.null(x)) {
    stop("`x` must not be NULL.", call. = FALSE)
  }
  lineage <- cohort_lineage(cp, NA_character_)$lineage
  x <- set_value_attr(
    x,
    "pbn_exclusions",
    cohort_exclusions(cp$consort(), lineage, NA_character_)
  )
  return(set_value_attr(x, "pbn_n_before", as.integer(cp$n_total())))
}

check_cohort <- function(cp) {
  if (!inherits(cp, "CohortPipeline")) {
    stop("`cp` must be a cohort::CohortPipeline.", call. = FALSE)
  }
  return(invisible(cp))
}
