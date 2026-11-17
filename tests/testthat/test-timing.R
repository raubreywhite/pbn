# The timings of run_plan(): measured where the work runs, kept in the
# registry of the plan, and never in the value of run_plan().

# A plan with data in the steps form, `d`, data in the free form, `e`, and
# one analysis kind with two values of an extra dimension. The functions
# live in the global environment, as in a project.
timed_plan <- function() {
  raw <- function(scenario, sim) return(data.frame(x = stats::rnorm(4)))
  clean <- function(d) return(d)
  free <- function(scenario, sim) return(data.frame(y = stats::runif(2)))
  action <- function(data, argset) return(data.frame(m = mean(data$d$x)))
  for (f in c("raw", "clean", "free", "action")) {
    fn <- get(f)
    environment(fn) <- globalenv()
    assign(f, fn)
  }
  p <- plnr::Plan$new()
  add_data(p, "d", raw = raw, clean = clean)
  add_data(p, "e", fn = free)
  sc <- scenarios(list(
    scenarios = data.frame(scenario = c("real", "null"), sims = c(1, 3))
  ))
  add_analyses(p, sc, "a", fn = action, k = 1:2)
  return(p)
}

# The run record of the plan `p`, without its fields that change in every
# run.
timing_shape <- function(p) {
  run <- plan_info(p)$run
  steps <- run$steps[
    order(run$steps$data, run$steps$scenario, run$steps$step),
  ]
  rownames(steps) <- NULL
  return(list(
    data = sort(paste(run$data$data, run$data$scenario, run$data$sim)),
    steps = steps[, c("data", "scenario", "step")],
    analyses = run$analyses[, c("scenario", "kind")],
    timed = !is.na(run$analyses$seconds) & run$analyses$seconds >= 0,
    data_timed = all(run$data$seconds >= 0)
  ))
}

expected_shape <- function() {
  where <- c("null 1", "null 2", "null 3", "real 1")
  # The steps raw and clean of d, and the one step fn of e, for each
  # scenario and simulation, sorted by data, scenario and step.
  steps <- data.frame(
    data = rep(c("d", "e"), c(8, 4)),
    scenario = c(rep("null", 6), "real", "real", rep("null", 3), "real"),
    step = c(rep(c("clean", "raw"), each = 3), "clean", "raw", rep("fn", 4))
  )
  return(list(
    data = sort(c(paste("d", where), paste("e", where))),
    steps = steps,
    analyses = data.frame(
      scenario = rep(c("real", "null"), c(2, 6)),
      kind = "a"
    ),
    timed = rep(TRUE, 8),
    data_timed = TRUE
  ))
}

test_that("run_plan measures each data step and each analysis, and its value stays identical between two runs", {
  local_clean_state()
  p <- timed_plan()
  one <- run_plan(p)
  first <- timing_shape(p)
  two <- run_plan(p)
  expect_identical(two, one)
  expect_identical(timing_shape(p), first)
  expect_identical(first, expected_shape())
  # The value holds no timing: no pbn_timed and no attribute but the stamp.
  expect_false(any(vapply(one, inherits, logical(1), what = "pbn_timed")))
  expect_identical(names(attributes(one)), c("names", "pbn_run"))
  expect_identical(
    one[["null__sim_0002__a__2"]],
    p$run_one("null__sim_0002__a__2")
  )
})

test_that("with 2 workers the timings come back from the workers, and the value is identical to 1 worker", {
  skip_if_pbn_not_installed()
  local_clean_state()
  p <- timed_plan()
  one <- run_plan(p)
  two <- run_plan(p, workers = 2)
  expect_identical(two, one)
  expect_identical(timing_shape(p), expected_shape())
  expect_identical(plan_info(p)$run$workers, 2)
})

test_that("a data record from the cache is marked as reused, with its read time as the one step", {
  p <- local_project()
  sourced <- init(p)
  sc <- scenarios(sourced$CFG)
  plan <- plnr::Plan$new()
  fn <- function(scenario, sim) return(sourced$get_data(scenario, sim))
  environment(fn) <- list2env(list(sourced = sourced), parent = globalenv())
  add_data(plan, "d", fn = fn, cache = TRUE)
  add_analyses(plan, sc, "m", fn = function(data, argset) return(nrow(data$d)))
  utils::capture.output(run_plan(plan))
  first <- plan_info(plan)$run
  expect_false(any(first$data$reused))
  expect_identical(unique(first$steps$step), "fn")
  utils::capture.output(run_plan(plan))
  second <- plan_info(plan)$run
  expect_true(all(second$data$reused))
  expect_identical(unique(second$steps$step), "read the cache")
  expect_true(all(grepl(
    ": reused the cache of ",
    second$data$line,
    fixed = TRUE
  )))
})

test_that("unwrap_timed keeps one copy of a record that two workers got with the log", {
  rec <- function(id) return(list(id = id, data = "d"))
  timed <- function(value, data) {
    return(structure(
      list(value = value, seconds = 0.5, data = data),
      class = "pbn_timed"
    ))
  }
  log <- list2env(list(pending = list(rec("1_1"))), parent = emptyenv())
  got <- unwrap_timed(
    list(timed(1, list(rec("1_1"), rec("9_2"))), timed(NULL, NULL), "plain"),
    log
  )
  expect_identical(got$r, list(1, NULL, "plain"))
  expect_identical(got$seconds, c(0.5, 0.5, NA))
  expect_identical(
    vapply(got$data, function(d) return(d$id), ""),
    c("1_1", "9_2")
  )
})
