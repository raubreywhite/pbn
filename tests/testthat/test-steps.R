# The steps form of add_data(): raw, clean, cohort and after_cohort.

steps_scenarios <- function() {
  return(scenarios(list(
    scenarios = data.frame(scenario = c("real", "null"), sims = c(1, 2))
  )))
}

# Ten rows, two of them under 18.
ten_rows <- function(scenario, sim) {
  return(data.frame(
    id = 1:10,
    age = c(15, 16, 20:27),
    y = stats::rnorm(10)
  ))
}

as_table <- function(d) {
  return(data.table::as.data.table(d))
}

adults <- function(d) {
  cp <- cohort::CohortPipeline$new(data.table::as.data.table(d))
  cp$exclude_and_track("root", "age under 18", "age < 18")
  return(cp)
}

# As in a project, a function with data.table syntax lives in the global
# environment, where data.table's [ and := work. `...` are its objects.
in_global <- function(f, ...) {
  environment(f) <- list2env(list(...), parent = globalenv())
  return(f)
}

# A plan with the data "d" from the arguments `...` of add_data(), and one
# analysis that gives the rows and the ids.
steps_plan <- function(...) {
  p <- plnr::Plan$new()
  add_data(p, "d", ...)
  add_analyses(p, steps_scenarios(), "a", fn = function(data, argset) {
    return(data.frame(n = nrow(data$d), ids = paste(data$d$id, collapse = " ")))
  })
  return(p)
}

# The printed lines of a run_plan() that MUST fail. The run stops at its
# first failed analysis, so a data that fails for each scenario fails the 2
# analyses of real only: the one that counts the rows and "a".
failed_lines <- function(p) {
  return(utils::capture.output(
    testthat::expect_error(run_plan(p), "analyses failed")
  ))
}

test_that("add_data stops on fn together with a step, and on no fn and no raw, and adds nothing", {
  p <- plnr::Plan$new()
  f <- function(scenario, sim) return(1)
  expect_error(
    add_data(p, "d", fn = f, raw = f),
    "Give `fn` or the steps, not both. add_data() got `fn` and `raw`.",
    fixed = TRUE
  )
  expect_error(
    add_data(p, "d", fn = f, clean = identity, cohort = identity),
    "add_data() got `fn` and `clean`, `cohort`.",
    fixed = TRUE
  )
  expect_error(
    add_data(p, "d"),
    "Give the data function as `fn`, or give the steps, starting with `raw`.",
    fixed = TRUE
  )
  expect_error(
    add_data(p, "d", fn = 1),
    "`fn` must be a function.",
    fixed = TRUE
  )
  expect_length(plan_info(p)$data, 0)
})

test_that("add_data stops on steps without raw, on after_cohort without cohort, and on a step that is not a function", {
  p <- plnr::Plan$new()
  f <- function(scenario, sim) return(1)
  expect_error(
    add_data(p, "d", clean = identity),
    "The steps need the step `raw`, the function(scenario, sim) that reads or makes the data.",
    fixed = TRUE
  )
  expect_error(
    add_data(p, "d", raw = f, clean = identity, after_cohort = identity),
    paste0(
      "The step `after_cohort` needs the step `cohort`, because it gets the ",
      "cohort::CohortPipeline of that step."
    ),
    fixed = TRUE
  )
  expect_error(
    add_data(p, "d", raw = f, clean = 1),
    "`clean` must be a function.",
    fixed = TRUE
  )
  expect_error(
    add_data(p, "d", raw = "data_raw"),
    "`raw` must be a function.",
    fixed = TRUE
  )
  expect_length(plan_info(p)$data, 0)
  # raw alone, and raw with cohort but no clean, are steps forms.
  expect_identical(add_data(p, "d", raw = f), "d")
  expect_identical(add_data(p, "e", raw = f, cohort = identity), "e")
})

test_that("each step after raw takes exactly one argument without a default, and raw keeps the rules of fn", {
  p <- plnr::Plan$new()
  f <- function(scenario, sim) return(1)
  expect_error(
    add_data(p, "d", raw = f, clean = function() 1),
    paste0(
      "The step `clean` must take exactly one argument without a default, ",
      "the value of the step before it. It takes none."
    ),
    fixed = TRUE
  )
  expect_error(
    add_data(p, "d", raw = f, cohort = function(d, e) 1),
    "The step `cohort` must take exactly one argument without a default, the value of the step before it. It takes d, e.",
    fixed = TRUE
  )
  expect_error(
    add_data(p, "d", raw = f, cohort = identity, after_cohort = function(...) {
      1
    }),
    "The step `after_cohort` must take exactly one argument without a default",
    fixed = TRUE
  )
  expect_error(
    add_data(p, "d", raw = function(scenario, sim = 1) 1),
    "The data function has sim with a default.",
    fixed = TRUE
  )
  expect_error(
    add_data(p, "d", raw = function(sim) 1),
    "The data function has the argument sim but not scenario",
    fixed = TRUE
  )
  expect_error(
    add_data(p, "d", raw = function(scenario) 1, cache = TRUE),
    "With cache = TRUE, the data function needs the arguments scenario and sim",
    fixed = TRUE
  )
  expect_length(plan_info(p)$data, 0)

  # The key fields come from raw. A step gets its value by the name of its
  # argument without a default, also after an argument with a default.
  add_data(
    p,
    "d",
    raw = function(scenario, sim, n_boot) return(data.frame(n = n_boot)),
    clean = function(k = 0, d, ...) return(data.frame(n = d$n + k))
  )
  expect_identical(
    key_fields(plan_info(p)$data[["d"]]),
    c("scenario", "sim", "n_boot")
  )
  sc <- scenarios(list(
    scenarios = data.frame(
      scenario = c("real", "null"),
      sims = 1,
      n_boot = c(7, 3)
    )
  ))
  add_analyses(p, sc, "a", fn = function(data, argset) return(data$d$n))
  expect_identical(p$get_data("null__sim_0001__a__1")$d, data.frame(n = 3))
  expect_identical(run_plan(p)[["real__sim_0001__a__1"]], 7)
})

test_that("a clean step that drops a row, or returns no data.frame, fails the data with both counts, in run_plan and by hand", {
  skip_if_not_installed("cohort")
  p <- steps_plan(
    raw = ten_rows,
    clean = function(d) return(d[-1, ]),
    cohort = adults
  )
  msg <- paste0(
    "The step clean of the data \"d\" returned 9 rows, but the step raw ",
    "returned 10 rows. The step clean MUST return a data.frame with the same ",
    "number of rows. Put each exclusion in the step cohort, with ",
    "$exclude_and_track(), so that pbn counts it."
  )
  out <- failed_lines(p)
  expect_identical(
    out[match("pbn: the analysis real__sim_0001__a__1 failed:", out) + 1],
    paste0("The data function 'd' failed: ", msg)
  )
  expect_identical(
    sum(out == paste0("The data function 'd' failed: ", msg)),
    2L
  )
  expect_error(p$get_data("null__sim_0001__a__1"), msg, fixed = TRUE)

  q <- steps_plan(raw = ten_rows, clean = function(d) return(as.list(d)))
  expect_error(
    q$get_data("real__sim_0001__a__1"),
    paste0(
      "The step clean of the data \"d\" returned an object of class list, ",
      "not a data.frame, but the step raw returned 10 rows."
    ),
    fixed = TRUE
  )
})

test_that("a cohort step that drops rows before its first tracked exclusion, or returns no cohort, fails the data", {
  skip_if_not_installed("cohort")
  p <- steps_plan(
    raw = ten_rows,
    clean = as_table,
    cohort = in_global(function(d) return(adults(d[age > 15])), adults = adults)
  )
  msg <- paste0(
    "The cohort of the step cohort of the data \"d\" starts with 9 rows, but ",
    "the step got 10 rows. The step cohort MUST give ",
    "cohort::CohortPipeline$new() every row that it gets. Put each exclusion ",
    "in $exclude_and_track(), so that pbn counts it."
  )
  out <- failed_lines(p)
  expect_identical(
    sum(out == paste0("The data function 'd' failed: ", msg)),
    2L
  )
  expect_error(p$get_data("null__sim_0001__a__1"), msg, fixed = TRUE)

  # Without clean, the cohort gets the rows of raw.
  q <- steps_plan(raw = ten_rows, cohort = function(d) {
    return(adults(d[-(1:3), ]))
  })
  expect_error(
    q$get_data("real__sim_0001__a__1"),
    "starts with 7 rows, but the step got 10 rows.",
    fixed = TRUE
  )
  r <- steps_plan(raw = ten_rows, cohort = as_table)
  expect_error(
    r$get_data("real__sim_0001__a__1"),
    paste0(
      "The step cohort of the data \"d\" returned an object of class ",
      "data.table. It MUST return a cohort::CohortPipeline."
    ),
    fixed = TRUE
  )
})

test_that("steps that keep every row pass, also with new row order and new columns, and a raw value that is not a data.frame leaves clean unchecked", {
  skip_if_not_installed("cohort")
  p <- steps_plan(
    raw = ten_rows,
    clean = in_global(
      function(d) {
        d <- as_table(d)[order(-id)]
        d[, older := age > 21]
        return(d[, .(id, age, older)])
      },
      as_table = as_table
    ),
    cohort = adults
  )
  r <- run_plan(p)
  expect_identical(
    r[["null__sim_0002__a__1"]],
    data.frame(n = 8L, ids = "10 9 8 7 6 5 4 3")
  )
  counted <- r[["null__sim_0002__pbn_n_rows"]]
  expect_identical(attr(counted, "n_before"), list(d = 10L))
  expect_identical(attr(counted, "exclusions")$d$n_after, 8L)

  # raw gives a list of two tables, and clean joins them and drops a row.
  q <- steps_plan(
    raw = function(scenario, sim) {
      return(list(a = ten_rows(scenario, sim), b = data.frame(id = 2:10)))
    },
    clean = function(x) return(merge(x$a, x$b, by = "id")),
    cohort = adults
  )
  rq <- run_plan(q)
  expect_identical(
    rq[["real__sim_0001__a__1"]],
    data.frame(n = 8L, ids = "3 4 5 6 7 8 9 10")
  )
  expect_identical(
    attr(rq[["real__sim_0001__pbn_n_rows"]], "n_before"),
    list(d = 9L)
  )

  # raw alone gives its value as it is.
  s <- steps_plan(raw = ten_rows)
  expect_identical(run_plan(s)[["real__sim_0001__a__1"]]$n, 10L)
})

test_that("the steps form gives results, exclusions, counts and manifests identical to the equivalent fn, by hand too", {
  skip_if_not_installed("cohort")
  p <- local_cohort_project()
  for (form in list(
    list(fn = cohort_data_fn, steps = cohort_steps, analysis = "fit_mean"),
    list(fn = impute_data_fn, steps = impute_steps, analysis = "fit_mean_imp"),
    # The step without keep_exclusions(): pbn keeps the table of the cohort.
    list(fn = impute_data_fn, steps = plain_steps, analysis = "fit_mean_imp")
  )) {
    free <- cohort_run(p, cache = FALSE, fn = form$fn, analysis = form$analysis)
    utils::capture.output(save_results(free$r, free$plan, free$scenarios))
    free_real <- read_results("v01", home = p$home)
    free_null <- lapply(1:2, function(k) {
      read_results("v01", "null", sim = k, home = p$home)
    })
    hand_free <- free$plan$get_data("null__sim_0002__means__1")

    steps <- cohort_run(
      p,
      cache = FALSE,
      steps = form$steps,
      analysis = form$analysis
    )
    utils::capture.output(save_results(steps$r, steps$plan, steps$scenarios))
    steps_real <- read_results("v01", home = p$home)
    steps_null <- lapply(1:2, function(k) {
      read_results("v01", "null", sim = k, home = p$home)
    })
    hand_steps <- steps$plan$get_data("null__sim_0002__means__1")

    expect_identical(unstamped(steps$r), unstamped(free$r))
    expect_identical(hand_steps, hand_free)
    # The real manifest holds data_files and no fake_md5, so it is the same.
    expect_identical(steps_real, free_real)
    expect_identical(steps_real$manifest$n_rows_before_exclusions, c(d = 8L))
    expect_identical(steps_real$manifest$n_rows_after_exclusions, c(d = 6L))
    # A fake manifest differs in fake_md5 only: the code is other code.
    for (k in 1:2) {
      a <- steps_null[[k]]
      b <- free_null[[k]]
      expect_false(identical(a$manifest$fake_md5, b$manifest$fake_md5))
      a$manifest$fake_md5 <- NULL
      b$manifest$fake_md5 <- NULL
      expect_identical(a, b)
    }
    expect_identical(
      steps_null[[1]]$manifest$fake_md5,
      function_md5(data_code(plan_info(steps$plan)))
    )
  }
})

test_that("pbn puts the exclusion table of the cohort on the value of after_cohort, also in place of a table from another cohort", {
  skip_if_not_installed("cohort")
  sc <- steps_scenarios()
  table <- data.frame(
    data = "d",
    step = "age under 18",
    n_before = 10L,
    n_excluded = 2L,
    n_after = 8L
  )
  other <- cohort::CohortPipeline$new(data.table::data.table(x = 1:4))
  other$exclude_and_track("root", "x of 3 or more", "x >= 3")
  ids <- function(cp) return(cohort_data(cp)$id)
  steps <- list(
    plain = ids,
    kept = function(cp) return(keep_exclusions(ids(cp), cp)),
    other = function(cp) return(keep_exclusions(ids(cp), other))
  )
  for (nm in names(steps)) {
    p <- plnr::Plan$new()
    add_data(
      p,
      "d",
      raw = ten_rows,
      cohort = adults,
      after_cohort = in_global(steps[[nm]], ids = ids, other = other)
    )
    add_analyses(p, sc, "a", fn = function(data, argset) {
      return(paste(data$d, collapse = " "))
    })
    r <- run_plan(p)
    expect_identical(r[["null__sim_0002__a__1"]], "3 4 5 6 7 8 9 10", info = nm)
    counted <- r[["null__sim_0002__pbn_n_rows"]]
    expect_identical(attr(counted, "exclusions"), list(d = table), info = nm)
    expect_identical(attr(counted, "n_before"), list(d = 10L), info = nm)
    hand <- p$get_data("real__sim_0001__a__1")$d
    expect_identical(attr(hand, "pbn_exclusions"), table, info = nm)
  }
  q <- plnr::Plan$new()
  add_data(
    q,
    "d",
    raw = ten_rows,
    cohort = adults,
    after_cohort = function(cp) NULL
  )
  add_analyses(q, sc, "a", fn = function(data, argset) return(1))
  expect_error(
    q$get_data("real__sim_0001__a__1"),
    "The step after_cohort of the data \"d\" returned NULL. It MUST return the data for the analyses.",
    fixed = TRUE
  )
})

test_that("an after_cohort step MAY return the cohort that it got, and a new cohort fails the data", {
  skip_if_not_installed("cohort")
  sc <- steps_scenarios()
  same <- steps_plan(
    raw = ten_rows,
    cohort = adults,
    after_cohort = function(cp) return(cp)
  )
  r <- run_plan(same)
  expect_identical(
    r[["null__sim_0002__a__1"]],
    data.frame(n = 8L, ids = "3 4 5 6 7 8 9 10")
  )
  expect_identical(
    attr(r[["null__sim_0002__pbn_n_rows"]], "exclusions")$d$n_after,
    8L
  )
  expect_identical(
    attr(r[["null__sim_0002__pbn_n_rows"]], "n_before"),
    list(d = 10L)
  )

  # A new pipeline from the data of the cohort would replace its table and
  # its count at the start.
  fresh <- steps_plan(
    raw = ten_rows,
    cohort = adults,
    after_cohort = function(cp) {
      return(cohort::CohortPipeline$new(cohort_data(cp)))
    }
  )
  msg <- paste0(
    "The step after_cohort of the data \"d\" returned another ",
    "cohort::CohortPipeline than the one it got. It MUST return the data for ",
    "the analyses, or the same cohort, so that the exclusion table is that of ",
    "the step cohort."
  )
  out <- failed_lines(fresh)
  expect_identical(
    sum(out == paste0("The data function 'd' failed: ", msg)),
    2L
  )
  expect_error(fresh$get_data("real__sim_0001__a__1"), msg, fixed = TRUE)
})

test_that("add_data needs a syntactic data name, so a name with $ cannot match a step of another data", {
  p <- plnr::Plan$new()
  f <- function(scenario, sim) return(1)
  msg <- paste0(
    "`name` must be a syntactic name that starts with a letter and holds ",
    "only letters, digits, \".\" and \"_\", for example \"d\"."
  )
  for (bad in c("x$raw", "1d", ".d", "_d", "a b", "if", "d-1")) {
    expect_error(add_data(p, bad, fn = f), msg, fixed = TRUE, info = bad)
    expect_error(add_data(p, bad, raw = f), msg, fixed = TRUE, info = bad)
  }
  expect_length(plan_info(p)$data, 0)
  for (good in c("d", "d1", "d.1", "d_1", "D")) {
    expect_identical(add_data(p, good, raw = f), good)
  }
  expect_error(
    add_data(p, "hash", fn = f),
    "because plnr uses that name",
    fixed = TRUE
  )
})

test_that("an analysis that changes the data of the steps in place fails, as with fn", {
  skip_if_not_installed("cohort")
  change <- in_global(function(data, argset) {
    data$d[, z := 1]
    return(1)
  })
  free <- function(scenario, sim) {
    return(adults(as_table(ten_rows(scenario, sim))))
  }
  plans <- list(
    steps = steps_plan(raw = ten_rows, clean = as_table, cohort = adults),
    fn = steps_plan(fn = free)
  )
  failed <- lapply(plans, function(p) {
    add_analyses(p, steps_scenarios(), "change", fn = change)
    out <- failed_lines(p)
    return(out[grepl("in place", out, fixed = TRUE)])
  })
  # The first analysis that changes the data stops the run.
  expect_length(failed$steps, 1)
  expect_true(all(startsWith(
    failed$steps,
    "The analysis changed the data 'd' in place"
  )))
  expect_identical(failed$steps, failed$fn)
})

test_that("fake_md5 covers each step and a step that is left out, and fn keeps its md5", {
  f <- function(scenario, sim) return(data.frame(x = 1))
  g <- function(d) return(d)
  md5 <- function(...) {
    p <- plnr::Plan$new()
    add_data(p, "d", ...)
    return(function_md5(data_code(plan_info(p))))
  }
  base <- md5(raw = f, clean = g, cohort = g, after_cohort = g)
  expect_identical(md5(raw = f, clean = g, cohort = g, after_cohort = g), base)
  h <- function(d) return(d[1])
  expect_false(identical(
    md5(
      raw = function(scenario, sim) 2,
      clean = g,
      cohort = g,
      after_cohort = g
    ),
    base
  ))
  expect_false(identical(
    md5(raw = f, clean = h, cohort = g, after_cohort = g),
    base
  ))
  expect_false(identical(
    md5(raw = f, clean = g, cohort = h, after_cohort = g),
    base
  ))
  expect_false(identical(
    md5(raw = f, clean = g, cohort = g, after_cohort = h),
    base
  ))
  expect_false(identical(md5(raw = f, clean = g, cohort = g), base))
  # The free form keeps its md5: the function named by the data.
  expect_identical(md5(fn = f), function_md5(list(d = f)))
})

test_that("with a cache, a changed step computes the data again, and the line names that step", {
  skip_if_not_installed("cohort")
  p <- local_cohort_project()
  step <- function(why, steps) {
    run <- cohort_run(p, steps = steps, analysis = "fit_mean_imp")
    expect_identical(run$out, cache_lines(p, why))
    return(run)
  }
  first <- step(rep("there is no cache file", 3), impute_steps)
  again <- step(rep(NA, 3), impute_steps)
  expect_identical(without_counts(again$r), without_counts(first$r))

  other <- list(
    raw = function(scenario, sim) {
      d <- data_raw(scenario, sim)
      return(d)
    },
    clean = function(d) {
      d <- data_clean(d)
      return(d)
    },
    cohort = function(d) {
      cp <- data_cohort(d)
      return(cp)
    },
    after_cohort = function(cp) {
      x <- data_impute(cp)
      return(x)
    }
  )
  # Each new step calls the function that was the step. That function is
  # now a function that the data calls, so the line names it too.
  called <- c(
    raw = "data_raw",
    clean = "data_clean",
    cohort = "data_cohort",
    after_cohort = "data_impute"
  )
  steps <- impute_steps
  for (s in names(other)) {
    steps[[s]] <- other[[s]]
    step(
      rep(
        paste0(
          "the function given as `",
          s,
          "`, the function `",
          called[[s]],
          "` changed"
        ),
        3
      ),
      steps
    )
  }
  # Two steps at once are both named, in the order of the steps.
  step(
    rep(
      paste0(
        "the function given as `raw`, the function given as `clean`, the ",
        "function `data_clean`, the function `data_raw` changed"
      ),
      3
    ),
    c(impute_steps[c("raw", "clean")], steps[c("cohort", "after_cohort")])
  )
  step(
    rep(
      paste0(
        "the function given as `cohort`, the function given as ",
        "`after_cohort`, the function `data_cohort`, the function ",
        "`data_impute` changed"
      ),
      3
    ),
    impute_steps
  )
  # A step that is left out is named too.
  run <- cohort_run(p, steps = cohort_steps)
  expect_identical(
    run$out,
    cache_lines(
      p,
      rep(
        "the function given as `after_cohort`, the function `impute_draws` changed",
        3
      )
    )
  )
  # From the steps to fn: the data function of add_data() changed.
  run <- cohort_run(p)
  expect_identical(
    run$out,
    cache_lines(
      p,
      rep(
        paste0(
          "the data function of add_data(), the function `data_clean`, the ",
          "function `data_cohort`, the function `data_raw` changed"
        ),
        3
      )
    )
  )
})

test_that("the steps on 2 workers give results and cache lines identical to 1 worker, and identical to fn", {
  skip_if_not_installed("cohort")
  skip_if_pbn_not_installed()
  p <- local_cohort_project()
  cache <- file.path(p$home, "v01_cache_gitexclude")
  args <- list(steps = plain_steps, analysis = "fit_mean_imp")
  one <- do.call(cohort_run, c(list(p), args))
  unlink(cache, recursive = TRUE)
  two <- do.call(cohort_run, c(list(p, workers = 2), args))
  expect_identical(two$out, cache_lines(p, rep("there is no cache file", 3)))
  expect_identical(two$out, one$out)
  expect_identical(unstamped(two$r), unstamped(one$r))
  again <- do.call(cohort_run, c(list(p, workers = 2), args))
  expect_identical(again$out, cache_lines(p, rep(NA, 3)))
  expect_identical(without_counts(again$r), without_counts(one$r))

  free <- cohort_run(
    p,
    cache = FALSE,
    workers = 2,
    fn = impute_data_fn,
    analysis = "fit_mean_imp"
  )
  steps <- cohort_run(
    p,
    cache = FALSE,
    workers = 2,
    steps = plain_steps,
    analysis = "fit_mean_imp"
  )
  expect_identical(unstamped(steps$r), unstamped(free$r))
  expect_identical(
    attr(steps$r[["null__sim_0002__pbn_n_rows"]], "exclusions")$d$step,
    "age under 18"
  )
})
