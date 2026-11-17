small_scenarios <- function() {
  return(scenarios(list(
    scenarios = data.frame(
      scenario = c("real", "null"),
      sims = c(1, 2),
      n_boot = c(200, 5)
    )
  )))
}

data_fn <- function(scenario, sim) {
  return(data.frame(x = stats::rnorm(4)))
}

test_that("add_analyses names each analysis <scenario>__sim_<kkkk>__<name>__<i>, with the last dimension fastest", {
  p <- plnr::Plan$new()
  add_data(p, "d", fn = data_fn)
  added <- add_analyses(
    p,
    small_scenarios(),
    name = "models",
    fn = function(data, argset) return(NULL),
    outcome = c("y", "z"),
    adjust = c("full", "none")
  )
  expect_identical(
    added,
    c(
      paste0("real__sim_0001__models__", 1:4),
      paste0("null__sim_0001__models__", 1:4),
      paste0("null__sim_0002__models__", 1:4)
    )
  )
  expect_identical(
    names(p$analyses),
    c(
      "real__sim_0001__pbn_n_rows",
      "null__sim_0001__pbn_n_rows",
      "null__sim_0002__pbn_n_rows",
      added
    )
  )
  expect_identical(
    p$get_argset("null__sim_0002__models__3"),
    list(
      scenario = "null",
      sim = 2L,
      analysis = "models",
      seed = 1270170988L,
      outcome = "z",
      adjust = "full",
      n_boot = 5
    )
  )
  expect_identical(p$get_argset("real__sim_0001__models__2")$adjust, "none")
  expect_identical(p$get_argset("real__sim_0001__models__2")$outcome, "y")
  expect_identical(
    p$get_argset("real__sim_0001__pbn_n_rows"),
    list(
      scenario = "real",
      sim = 1L,
      analysis = "pbn_n_rows",
      seed = 497968153L,
      n_boot = 200
    )
  )
  # A second kind adds no second analysis that counts the rows.
  add_analyses(p, small_scenarios(), "means", fn = function(data, argset) 1)
  expect_identical(sum(grepl("pbn_n_rows", names(p$analyses))), 3L)
  expect_identical(
    tail(names(p$analyses), 3),
    c(
      "real__sim_0001__means__1",
      "null__sim_0001__means__1",
      "null__sim_0002__means__1"
    )
  )
})

test_that("a control analysis is never added for real", {
  p <- plnr::Plan$new()
  add_data(p, "d", fn = data_fn)
  added <- add_analyses(
    p,
    small_scenarios(),
    name = "crude",
    fn = function(data, argset) return(1),
    control = TRUE
  )
  expect_identical(
    added,
    c("null__sim_0001__crude__1", "null__sim_0002__crude__1")
  )
  scenario_of <- vapply(
    p$analyses,
    function(a) return(a$argset$scenario),
    character(1)
  )
  expect_false("real" %in% scenario_of)
  info <- plan_info(p)
  expect_identical(
    info$kinds,
    data.frame(analysis = "crude", control = TRUE)
  )
})

test_that("add_analyses and add_data stop on invalid input and on a name that is taken, and add nothing", {
  p <- plnr::Plan$new()
  add_data(p, "d", fn = data_fn)
  sc <- small_scenarios()
  fn <- function(data, argset) return(1)
  add_analyses(p, sc, "models", fn = fn)
  before <- names(p$analyses)
  stops <- list(
    "`name` must be one syntactic name" = list(name = "my models"),
    "`name` must be one syntactic name " = list(name = "manifest"),
    "`name` must be one syntactic name  " = list(name = "pbn_rows"),
    "`name` must be one syntactic name   " = list(name = ".hidden"),
    "already has the analysis kind \"models\"" = list(name = "models"),
    "`fn` must be a function" = list(name = "a", fn = "f"),
    "`control` must be TRUE or FALSE" = list(name = "a", control = NA),
    "need another name: sim" = list(name = "a", sim = 1:2),
    "need another name: fn_name" = list(name = "a", fn_name = "x"),
    "have the name of a setting column of CFG$scenarios: n_boot" = list(
      name = "a",
      n_boot = c(1, 2)
    ),
    "has a value twice" = list(name = "a", outcome = c("y", "y")),
    "must be an atomic vector with at least one value and no NA" = list(
      name = "a",
      outcome = c("y", NA)
    ),
    "must be an atomic vector with at least one value and no NA " = list(
      name = "a",
      outcome = list("y")
    ),
    "Each extra dimension in `...` needs its own name" = list(
      name = "a",
      c("y", "z")
    )
  )
  for (msg in names(stops)) {
    args <- c(list(p = p, scenarios = sc), stops[[msg]])
    if (!"fn" %in% names(args)) {
      args$fn <- fn
    }
    expect_error(
      do.call(add_analyses, args),
      trimws(msg),
      fixed = TRUE,
      info = msg
    )
  }
  expect_identical(names(p$analyses), before)
  expect_error(add_analyses(list(), sc, "a", fn), "`p` must be a plan")
  expect_error(
    add_analyses(p, data.frame(x = 1), "a", fn),
    "`scenarios` must be a data.frame with the columns scenario and sims"
  )

  # A name that plnr has already, from p$add_analysis().
  p$add_analysis(name = "null__sim_0001__other__1", fn = fn, sim = 1)
  expect_error(
    add_analyses(p, sc, "other", fn),
    "The plan already has analyses with these names, so pbn added none: null__sim_0001__other__1",
    fixed = TRUE
  )
  expect_false("real__sim_0001__other__1" %in% names(p$analyses))

  expect_error(add_data(p, "d", fn = data_fn), "already has the data \"d\"")
  expect_error(add_data(p, "hash", fn = data_fn), "plnr uses that name")
  expect_error(add_data(p, "", fn = data_fn), "one non-empty string")
  expect_error(add_data(p, "e", fn = 1), "`fn` must be a function")
  expect_error(add_data(list(), "e", fn = data_fn), "`p` must be a plan")
})

test_that("the data wrapper keeps the key fields of the function, and sets the seed only when sim is one", {
  keys <- function(fn) return(key_fields(data_wrapper(fn, "d")))
  expect_identical(keys(function(scenario, sim) 1), c("scenario", "sim"))
  expect_identical(keys(function(sim) 1), "sim")
  expect_identical(
    keys(function(scenario, sim, n = 3, ...) 1),
    c("scenario", "sim")
  )
  expect_identical(
    keys(function(scenario, sim, n_boot) 1),
    c("scenario", "sim", "n_boot")
  )
  expect_identical(keys(function() 1), character(0))

  w <- data_wrapper(
    function(scenario, sim, n = 3) {
      return(list(scenario, stats::runif(1), n))
    },
    "d"
  )
  set.seed(99)
  first <- w(scenario = "null", sim = 4)
  set.seed(1)
  expect_identical(w(scenario = "null", sim = 4), first)
  set.seed(4)
  expect_identical(first, list("null", stats::runif(1), 3))

  # Without sim, the wrapper sets no seed.
  w <- data_wrapper(function(scenario) return(stats::runif(1)), "d")
  set.seed(5)
  a <- w("null")
  set.seed(5)
  expect_identical(w("null"), a)
  expect_false(identical(w("null"), a))
})

test_that("an action sets the seed to its simulation, and is_run_directly() is FALSE in it", {
  p <- plnr::Plan$new()
  add_data(p, "d", fn = data_fn)
  add_analyses(p, small_scenarios(), "draws", fn = function(data, argset) {
    return(list(direct = plnr::is_run_directly(), draw = stats::runif(1)))
  })
  r <- run_plan(p)
  draws <- r[c(
    "real__sim_0001__draws__1",
    "null__sim_0001__draws__1",
    "null__sim_0002__draws__1"
  )]
  expect_identical(
    vapply(draws, function(x) return(x$direct), logical(1)),
    stats::setNames(rep(FALSE, 3), names(draws))
  )
  draw_of <- function(seed) {
    set.seed(seed)
    return(stats::runif(1))
  }
  one <- draw_of(p$get_argset("null__sim_0001__draws__1")$seed)
  two <- draw_of(p$get_argset("null__sim_0002__draws__1")$seed)
  expect_identical(
    unname(vapply(draws, function(x) return(x$draw), numeric(1))),
    c(one, one, two)
  )
})

test_that("the data function runs once for each scenario and simulation, however many analyses use it", {
  calls <- character(0)
  p <- plnr::Plan$new()
  add_data(p, "d", fn = function(scenario, sim) {
    calls <<- c(calls, paste(scenario, sim))
    return(data.frame(x = 1:3))
  })
  sc <- small_scenarios()
  add_analyses(p, sc, "a", fn = function(data, argset) return(1))
  add_analyses(p, sc, "b", fn = function(data, argset) return(2), k = 1:3)
  add_analyses(
    p,
    sc,
    "c",
    fn = function(data, argset) return(3),
    control = TRUE
  )
  # 3 scenarios and simulations, with 6, 6 and 5 analyses.
  expect_identical(length(p$analyses), 3L + 3L + 9L + 2L)
  run_plan(p)
  expect_identical(calls, c("real 1", "null 1", "null 2"))
})

test_that("simulation k gives the same fake data whichever scenarios are selected", {
  p <- local_project()
  sourced <- init(p)
  all <- build_plan(sourced, scenarios(sourced$CFG))
  one <- build_plan(sourced, scenarios(sourced$CFG, only = "null"))
  for (sim in 1:3) {
    nm <- paste0("null__sim_000", sim, "__models__1")
    set.seed(100 + sim)
    a <- all$get_data(nm)$d
    set.seed(200 + sim)
    b <- one$get_data(nm)$d
    expect_identical(a, b, info = nm)
  }
  expect_false(identical(
    all$get_data("null__sim_0001__table1__1")$d,
    all$get_data("null__sim_0002__table1__1")$d
  ))
})

test_that("p$get_argset() and p$get_data() step through one named analysis by hand", {
  p <- local_project()
  sourced <- init(p)
  plan <- build_plan(sourced, scenarios(sourced$CFG))
  argset <- plan$get_argset("null__sim_0003__models__2")
  expect_identical(argset$outcome, "z")
  expect_identical(argset$scenario, "null")
  data <- plan$get_data("null__sim_0003__models__2")
  expect_identical(names(data), c("d", "hash"))
  expect_identical(nrow(data$d), 40L)
  # The action function by hand gives what the plan gives.
  by_hand <- {
    set.seed(argset$seed)
    sourced$fit_models(data, argset)
  }
  expect_identical(plan$run_one("null__sim_0003__models__2"), by_hand)
  # Outside run_plan(), an error in the data function stops the call.
  bad <- plnr::Plan$new()
  add_data(bad, "d", fn = function(scenario, sim) stop("planted data error"))
  add_analyses(bad, small_scenarios(), "a", fn = function(data, argset) 1)
  expect_error(bad$get_data("null__sim_0001__a__1"), "planted data error")
  expect_error(bad$run_one("null__sim_0001__a__1"), "planted data error")
})

test_that("the entry of a plan goes when R frees the plan, and a new plan starts empty", {
  make <- function() {
    p <- plnr::Plan$new()
    add_data(p, "d", fn = data_fn)
    return(data.table::address(p))
  }
  key <- make()
  gc()
  gc()
  expect_false(exists(key, envir = plans, inherits = FALSE))
  q <- plnr::Plan$new()
  expect_null(plan_info(q))
  add_data(q, "d", fn = data_fn)
  expect_identical(names(plan_info(q)$data), "d")
})

test_that("the seed of an analysis is the documented hash of kind, counter and sim, and is 10000 or more", {
  expect_identical(analysis_seed("pbn_n_rows", 0, 1), 497968153L)
  expect_identical(analysis_seed("models", 3, 2), 1270170988L)
  # The polynomial hash of "<name>__<k>__<sim>", computed here once more.
  h <- 0
  for (code in utf8ToInt("draws__1__7")) {
    h <- (h * 257 + code) %% 2147483629
  }
  expect_identical(analysis_seed("draws", 1, 7), as.integer(10000 + h %% (2147483629 - 10000)))
  for (kind in c("a", "models", "table1", "pbn_n_rows")) {
    seeds <- vapply(1:9999, function(s) return(analysis_seed(kind, 1, s)), 1L)
    expect_true(all(seeds != 1:9999), info = kind)
    expect_gte(min(seeds), 10000L)
    expect_identical(anyDuplicated(seeds), 0L)
  }
})

test_that("an analysis draws other numbers than its data, two kinds of one simulation draw other numbers, and one kind draws the same in each scenario", {
  p <- plnr::Plan$new()
  add_data(p, "d", fn = function(scenario, sim) {
    return(data.frame(x = stats::rnorm(1)))
  })
  sc <- small_scenarios()
  draw <- function(data, argset) return(stats::rnorm(1))
  add_analyses(p, sc, "a", fn = draw)
  add_analyses(p, sc, "b", fn = draw)
  add_analyses(p, sc, "c", fn = draw, k = 1:2)
  r <- run_plan(p)
  first_data <- p$get_data("null__sim_0001__a__1")$d$x
  expect_false(identical(r[["null__sim_0001__a__1"]], first_data))
  expect_false(identical(r[["null__sim_0001__a__1"]], r[["null__sim_0001__b__1"]]))
  expect_false(identical(r[["null__sim_0001__c__1"]], r[["null__sim_0001__c__2"]]))
  expect_identical(r[["real__sim_0001__a__1"]], r[["null__sim_0001__a__1"]])
  expect_identical(r[["real__sim_0001__c__2"]], r[["null__sim_0001__c__2"]])
  expect_false(identical(r[["null__sim_0001__a__1"]], r[["null__sim_0002__a__1"]]))
})

test_that("two analyses of one simulation with one seed stop add_analyses before it adds anything", {
  p <- plnr::Plan$new()
  add_data(p, "d", fn = data_fn)
  add_analyses(p, small_scenarios(), "a", fn = function(data, argset) 1)
  before <- names(p$analyses)
  seed_a <- p$get_argset("real__sim_0001__a__1")$seed
  local_mocked_bindings(analysis_seed = function(name, k, sim) return(seed_a))
  expect_error(
    add_analyses(p, small_scenarios(), "b", fn = function(data, argset) 1),
    paste0(
      "Two analyses of simulation 1 get the seed ",
      seed_a,
      ": the kinds a and b. Give one of the kinds another name."
    ),
    fixed = TRUE
  )
  expect_identical(names(p$analyses), before)
  # Also against the analysis that counts the rows, in a new plan.
  q <- plnr::Plan$new()
  add_data(q, "d", fn = data_fn)
  expect_error(
    add_analyses(q, small_scenarios(), "a", fn = function(data, argset) 1, k = 1:2),
    paste0("get the seed ", seed_a, ": the kinds pbn_n_rows and a."),
    fixed = TRUE
  )
  # Within one kind.
  local_mocked_bindings(analysis_seed = function(name, k, sim) {
    return(if (name == "pbn_n_rows") 99999L else seed_a)
  })
  expect_error(
    add_analyses(q, small_scenarios(), "a", fn = function(data, argset) 1, k = 1:2),
    paste0("get the seed ", seed_a, ": the kinds a and a."),
    fixed = TRUE
  )
  expect_length(q$analyses, 0)
  expect_length(q$analyses, 0)
})

test_that("add_data stops on a scenario or sim with a default and on sim without scenario, and allows neither", {
  p <- plnr::Plan$new()
  expect_error(
    add_data(p, "d", fn = function(scenario, sim = 1) 1),
    "The data function has sim with a default",
    fixed = TRUE
  )
  expect_error(
    add_data(p, "d", fn = function(scenario = "real", sim) 1),
    "The data function has scenario with a default",
    fixed = TRUE
  )
  expect_error(
    add_data(p, "d", fn = function(sim) 1),
    "has the argument sim but not scenario",
    fixed = TRUE
  )
  expect_null(plan_info(p))
  # A lookup table, without scenario and sim, and a function of the
  # scenario only.
  expect_identical(add_data(p, "lookup", fn = function() data.frame(k = 1)), "lookup")
  expect_identical(add_data(p, "by_scenario", fn = function(scenario) 1), "by_scenario")
  expect_identical(add_data(p, "d", fn = function(scenario, sim, n = 2) 1), "d")
})

test_that("an analysis that changes the shared data in place fails, and one that copies it first passes", {
  sc <- small_scenarios()
  # As in a project, the functions live in the global environment, where
  # data.table's := works.
  in_global <- function(f, ...) {
    environment(f) <- list2env(list(...), parent = globalenv())
    return(f)
  }
  shared <- in_global(function(scenario, sim) {
    return(data.table::data.table(x = 1:3, y = c(2, 4, 6)))
  })
  changes <- list(
    add = function(d) d[, z := 1],
    replace = function(d) d[, x := x * 2L],
    remove = function(d) d[, y := NULL],
    rename = function(d) data.table::setnames(d, "x", "x2"),
    key = function(d) data.table::setkey(d, y)
  )
  for (nm in names(changes)) {
    p <- plnr::Plan$new()
    add_data(p, "d", fn = shared)
    add_analyses(p, sc, "a", fn = in_global(function(data, argset) {
      change(data$d)
      return(1)
    }, change = in_global(changes[[nm]])))
    out <- utils::capture.output(
      err <- tryCatch(run_plan(p), error = function(e) e)
    )
    expect_match(conditionMessage(err), "3 of 6 analyses failed", info = nm)
    expect_true(any(grepl(
      "The analysis changed the data 'd' in place",
      out,
      fixed = TRUE
    )), info = nm)
    expect_true(any(grepl("data.table::copy(data$d)", out, fixed = TRUE)))
  }
  p <- plnr::Plan$new()
  add_data(p, "d", fn = shared)
  add_analyses(p, sc, "a", fn = in_global(function(data, argset) {
    d <- data.table::copy(data$d)
    d[, z := 1]
    return(ncol(d))
  }))
  add_analyses(p, sc, "b", fn = in_global(function(data, argset) {
    return(ncol(data$d))
  }))
  r <- run_plan(p)
  expect_identical(r[["null__sim_0002__a__1"]], 3L)
  expect_identical(r[["null__sim_0002__b__1"]], 2L)

  # The documented limit: a change of values in place is not found, and the
  # next analysis gets the changed data.
  p <- plnr::Plan$new()
  add_data(p, "d", fn = shared)
  add_analyses(p, sc, "a", fn = in_global(function(data, argset) {
    data$d[1, x := 0L]
    return(1)
  }))
  add_analyses(p, sc, "b", fn = in_global(function(data, argset) {
    return(data$d$x[1])
  }))
  r <- run_plan(p)
  expect_identical(r[["null__sim_0002__b__1"]], 0L)
})
