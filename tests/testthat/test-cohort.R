two_scenarios_cohort <- function() {
  return(scenarios(list(
    scenarios = data.frame(scenario = c("real", "null"), sims = c(1, 2))
  )))
}

# A data function that returns a cohort with two exclusion steps, the second
# on a branch.
branched_cohort <- function(scenario, sim) {
  d <- data.table::data.table(id = 1:10, age = c(15, 16, 20:27), x = 1:10)
  cp <- cohort::CohortPipeline$new(d)
  cp$exclude_and_track("root", "age under 18", "age < 18")
  cp$new_cohort("low_x", from = "root")
  cp$exclude_and_track("low_x", "x of 8 or more", "x >= 8")
  return(cp)
}

test_that("the analyses get the data.table of a cohort after its exclusions, not the R6 object", {
  skip_if_not_installed("cohort")
  p <- plnr::Plan$new()
  fn <- branched_cohort
  environment(fn) <- globalenv()
  add_data(p, "d", fn = fn)
  add_analyses(p, two_scenarios_cohort(), "a", fn = function(data, argset) {
    return(data.frame(
      class = paste(class(data$d), collapse = "/"),
      ids = paste(data$d$id, collapse = " ")
    ))
  })
  r <- run_plan(p)
  expect_identical(
    r[["null__sim_0002__a__1"]],
    data.frame(class = "data.table/data.frame", ids = "3 4 5 6 7")
  )
  counted <- r[["null__sim_0002__pbn_n_rows"]]
  expect_identical(as.vector(counted), 5L)
  # The exclusion table holds the step of the root and that of the branch,
  # in order.
  expect_identical(
    attr(counted, "exclusions"),
    list(
      d = data.frame(
        data = "d",
        step = c("age under 18", "x of 8 or more"),
        n_before = c(10L, 8L),
        n_excluded = c(2L, 3L),
        n_after = c(8L, 5L)
      )
    )
  )
  # Another value goes to the analyses as it is, and the count has no
  # attribute.
  q <- plnr::Plan$new()
  add_data(q, "d", fn = function(scenario, sim) return(list(a = 1)))
  add_analyses(q, two_scenarios_cohort(), "a", fn = function(data, argset) {
    return(identical(data$d, list(a = 1)))
  })
  rq <- run_plan(q)
  expect_true(rq[["null__sim_0001__a__1"]])
  expect_identical(rq[["null__sim_0001__pbn_n_rows"]], c(d = NA_integer_))
})

test_that("a cohort with two cohorts without a child fails the data", {
  skip_if_not_installed("cohort")
  p <- plnr::Plan$new()
  add_data(p, "d", fn = function(scenario, sim) {
    cp <- cohort::CohortPipeline$new(data.table::data.table(x = 1:3))
    cp$new_cohort("b1", from = "root")
    cp$new_cohort("b2", from = "root")
    return(cp)
  })
  add_analyses(p, two_scenarios_cohort(), "a", fn = function(data, argset) 1)
  out <- utils::capture.output(
    expect_error(run_plan(p), "analyses failed")
  )
  expect_true(any(grepl(
    paste0(
      "returned a cohort::CohortPipeline with 2 cohorts that no other ",
      "cohort branches from: b1, b2"
    ),
    out,
    fixed = TRUE
  )))
})

test_that("save_results adds the element exclusions from the cohort, and a plan without a cohort has none", {
  skip_if_not_installed("cohort")
  p <- local_cohort_project()
  run <- cohort_run(p, cache = FALSE)
  utils::capture.output(save_results(run$r, run$plan, run$scenarios))
  real <- read_results("v01", home = p$home)
  expect_identical(names(real), c("manifest", "exclusions", "means"))
  expect_identical(
    real$exclusions,
    data.frame(
      data = "d",
      step = "age under 18",
      n_before = 8L,
      n_excluded = 2L,
      n_after = 6L
    )
  )
  expect_identical(real$manifest$n_rows_data, c(d = 6L))
  expect_identical(real$means$n, 6L)
  null <- read_results("v01", "null", home = p$home)
  expect_identical(
    names(null$exclusions),
    c("sim", "data", "step", "n_before", "n_excluded", "n_after")
  )
  expect_identical(null$exclusions$sim, 1:2)
  expect_identical(null$exclusions$n_before, c(30L, 30L))
  expect_identical(
    null$exclusions$n_after,
    null$means$n
  )

  # Without a cohort: no element exclusions.
  q <- local_project()
  sourced <- init(q)
  plan <- build_plan(sourced, scenarios(sourced$CFG))
  r <- run_plan(plan)
  utils::capture.output(save_results(r, plan, scenarios(sourced$CFG)))
  expect_false("exclusions" %in% names(read_results("v01", home = q$home)))
})

test_that("two cohorts on one plan give one exclusions table with the column data", {
  skip_if_not_installed("cohort")
  p <- plnr::Plan$new()
  for (nm in c("d1", "d2")) {
    fn <- branched_cohort
    environment(fn) <- globalenv()
    add_data(p, nm, fn = fn)
  }
  add_analyses(p, two_scenarios_cohort(), "a", fn = function(data, argset) 1)
  r <- run_plan(p)
  counted <- r[["real__sim_0001__pbn_n_rows"]]
  expect_identical(names(attr(counted, "exclusions")), c("d1", "d2"))
  table <- exclusions_element(attr(counted, "exclusions"))$exclusions
  expect_identical(table$data, c("d1", "d1", "d2", "d2"))
  expect_identical(rownames(table), as.character(1:4))
})

test_that("an analysis kind named exclusions stops add_analyses", {
  p <- plnr::Plan$new()
  expect_error(
    add_analyses(p, two_scenarios_cohort(), "exclusions", fn = identity),
    "must not be \"manifest\" or \"exclusions\"",
    fixed = TRUE
  )
  expect_length(p$analyses, 0)
})

test_that("a cohort built on 2 workers gives results identical to 1 worker, with the exclusion table", {
  skip_if_not_installed("cohort")
  skip_if_pbn_not_installed()
  p <- local_cohort_project()
  one <- cohort_run(p, cache = FALSE)
  two <- cohort_run(p, cache = FALSE, workers = 2)
  expect_identical(unstamped(two$r), unstamped(one$r))
  expect_identical(
    attr(two$r[["null__sim_0002__pbn_n_rows"]], "exclusions")$d$step,
    "age under 18"
  )
  expect_identical(two$r[["null__sim_0001__means__1"]]$class, "data.table")
  expect_identical(
    attr(two$r[["real__sim_0001__pbn_n_rows"]], "exclusions")$d$n_after,
    6L
  )
})

test_that("a step after the cohort keeps the exclusion table with keep_exclusions(), and a list of imputed data sets counts the rows of one", {
  skip_if_not_installed("cohort")
  p <- local_cohort_project()
  run <- cohort_run(p, cache = FALSE, fn = impute_data_fn, analysis = "fit_mean_imp")
  expect_identical(run$r[["real__sim_0001__means__1"]]$n_imp, 3L)
  expect_identical(run$r[["real__sim_0001__means__1"]]$n, 6L)
  counted <- run$r[["real__sim_0001__pbn_n_rows"]]
  expect_identical(as.vector(counted), 6L)
  expect_identical(attr(counted, "n_before"), list(d = 8L))
  expect_identical(
    attr(counted, "exclusions")$d,
    data.frame(
      data = "d",
      step = "age under 18",
      n_before = 8L,
      n_excluded = 2L,
      n_after = 6L
    )
  )
  utils::capture.output(save_results(run$r, run$plan, run$scenarios))
  real <- read_results("v01", home = p$home)
  expect_identical(real$exclusions$n_after, 6L)
  expect_identical(real$manifest$n_rows_data, c(d = 6L))
  expect_identical(real$manifest$n_rows_before_exclusions, c(d = 8L))
  null <- read_results("v01", "null", sim = 1, home = p$home)
  expect_identical(null$manifest$n_rows_before_exclusions, c(d = 30L))
})

test_that("count_rows counts one data set of a list of data.frames with one number of rows, and NA otherwise", {
  same <- list(data.frame(x = 1:4), data.table::data.table(x = 5:8))
  expect_identical(count_rows(list(d = same), list()), c(d = 4L))
  uneven <- list(data.frame(x = 1:4), data.frame(x = 1:3))
  expect_identical(count_rows(list(d = uneven), list()), c(d = NA_integer_))
  mixed <- list(data.frame(x = 1:4), 1:4)
  expect_identical(count_rows(list(d = mixed), list()), c(d = NA_integer_))
  expect_identical(count_rows(list(d = list()), list()), c(d = NA_integer_))
})

test_that("cohort_data and keep_exclusions stop on a value that is not a cohort, and keep_exclusions on NULL", {
  skip_if_not_installed("cohort")
  expect_error(cohort_data(data.frame(x = 1)), "`cp` must be a cohort::CohortPipeline.", fixed = TRUE)
  cp <- cohort::CohortPipeline$new(data.table::data.table(x = 1:3))
  expect_error(keep_exclusions(NULL, cp), "`x` must not be NULL.", fixed = TRUE)
  expect_error(keep_exclusions(1, list()), "`cp` must be a cohort::CohortPipeline.", fixed = TRUE)
  cp$new_cohort("b1", from = "root")
  cp$new_cohort("b2", from = "root")
  expect_error(
    cohort_data(cp),
    "The cohort::CohortPipeline has 2 cohorts that no other cohort branches from: b1, b2.",
    fixed = TRUE
  )
})

test_that("the imputation after the cohort works with the cache and with 2 workers, identical to 1 worker", {
  skip_if_not_installed("cohort")
  skip_if_pbn_not_installed()
  p <- local_cohort_project()
  cache <- file.path(p$home, "v01_cache_gitexclude")
  args <- list(fn = impute_data_fn, analysis = "fit_mean_imp")
  one <- do.call(cohort_run, c(list(p), args))
  unlink(cache, recursive = TRUE)
  two <- do.call(cohort_run, c(list(p, workers = 2), args))
  expect_identical(unstamped(two$r), unstamped(one$r))
  expect_identical(two$out, one$out)
  again <- do.call(cohort_run, c(list(p, workers = 2), args))
  expect_identical(again$out, cache_lines(p, rep(NA, 3)))
  expect_identical(without_counts(again$r), without_counts(one$r))
  expect_identical(
    attr(again$r[["null__sim_0002__pbn_n_rows"]], "exclusions"),
    attr(one$r[["null__sim_0002__pbn_n_rows"]], "exclusions")
  )
  expect_identical(
    attr(again$r[["null__sim_0002__pbn_n_rows"]], "n_before"),
    list(d = 30L)
  )
})

test_that("a table with one row for each row given to the cohort fails no_row_per_person", {
  skip_if_not_installed("cohort")
  p <- local_cohort_project()
  run <- cohort_run(p, cache = FALSE)
  add_analyses(run$plan, run$scenarios, "raw_rows", fn = function(data, argset) {
    return(data.frame(i = seq_len(attr(data$d, "pbn_n_before"))))
  })
  r <- run_plan(run$plan)
  out <- utils::capture.output(expect_error(
    save_results(r, run$plan, run$scenarios),
    "failures of check_results()"
  ))
  expect_true(any(grepl(
    "real: no_row_per_person: x$raw_rows (a data.frame with 8 rows)",
    out,
    fixed = TRUE
  )))
})

test_that("with a value that is not a data.frame, a table with one row for each person after the exclusions fails no_row_per_person", {
  skip_if_not_installed("cohort")
  p <- local_cohort_project()
  sourced <- init(p)
  sc <- scenarios(sourced$CFG)
  plan <- plnr::Plan$new()
  fn <- function(scenario, sim) {
    cp <- data_cohort(data_clean(data_raw(scenario, sim)))
    d <- pbn::cohort_data(cp)
    return(pbn::keep_exclusions(list(d, d[-1]), cp))
  }
  environment(fn) <- sourced
  add_data(plan, "d", fn = fn)
  add_analyses(plan, sc, "rows", fn = function(data, argset) {
    return(data.frame(i = seq_len(nrow(data$d[[1]]))))
  })
  r <- run_plan(plan)
  expect_identical(r[["real__sim_0001__pbn_n_rows"]][["d"]], NA_integer_)
  out <- utils::capture.output(expect_error(
    save_results(r, plan, sc),
    "failures of check_results()"
  ))
  expect_true(any(grepl(
    "real: no_row_per_person: x$rows (a data.frame with 6 rows)",
    out,
    fixed = TRUE
  )))
  counts <- cohort_counts(r[["real__sim_0001__pbn_n_rows"]])
  expect_identical(counts, list(before = c(d = 8L), after = c(d = 6L)))
})

test_that("a cohort with two exclusion steps and two people left saves, and a two-row analysis table still fails", {
  skip_if_not_installed("cohort")
  p <- local_cohort_project()
  sourced <- init(p)
  sc <- scenarios(list(scenarios = data.frame(scenario = "real", sims = 1)))
  plan <- plnr::Plan$new()
  fn <- function(scenario, sim) {
    cp <- cohort::CohortPipeline$new(data_clean(data_raw(scenario, sim)))
    cp$exclude_and_track("root", "age under 18", "age < 18")
    cp$exclude_and_track("root", "age over 26", "age > 26")
    return(cp)
  }
  environment(fn) <- sourced
  add_data(plan, "d", fn = fn)
  add_analyses(plan, sc, "means", fn = sourced$fit_mean)
  r <- run_plan(plan)
  utils::capture.output(save_results(r, plan, sc))
  x <- read_results("v01", home = p$home)
  expect_identical(x$manifest$n_rows_data, c(d = 2L))
  expect_identical(nrow(x$exclusions), 2L)

  add_analyses(plan, sc, "rows", fn = function(data, argset) {
    return(data.frame(i = seq_len(nrow(data$d))))
  })
  r <- run_plan(plan)
  out <- utils::capture.output(expect_error(
    save_results(r, plan, sc),
    "failures of check_results()"
  ))
  expect_true(any(grepl("real: no_row_per_person: x$rows (a data.frame with 2 rows)", out, fixed = TRUE)))
  expect_false(any(grepl("x$exclusions", out, fixed = TRUE)))
})
