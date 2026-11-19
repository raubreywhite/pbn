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
  expect_identical(
    analysis_seed("draws", 1, 7),
    as.integer(10000 + h %% (2147483629 - 10000))
  )
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
  expect_false(identical(
    r[["null__sim_0001__a__1"]],
    r[["null__sim_0001__b__1"]]
  ))
  expect_false(identical(
    r[["null__sim_0001__c__1"]],
    r[["null__sim_0001__c__2"]]
  ))
  expect_identical(r[["real__sim_0001__a__1"]], r[["null__sim_0001__a__1"]])
  expect_identical(r[["real__sim_0001__c__2"]], r[["null__sim_0001__c__2"]])
  expect_false(identical(
    r[["null__sim_0001__a__1"]],
    r[["null__sim_0002__a__1"]]
  ))
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
    add_analyses(
      q,
      small_scenarios(),
      "a",
      fn = function(data, argset) 1,
      k = 1:2
    ),
    paste0("get the seed ", seed_a, ": the kinds pbn_n_rows and a."),
    fixed = TRUE
  )
  # Within one kind.
  local_mocked_bindings(analysis_seed = function(name, k, sim) {
    return(if (name == "pbn_n_rows") 99999L else seed_a)
  })
  expect_error(
    add_analyses(
      q,
      small_scenarios(),
      "a",
      fn = function(data, argset) 1,
      k = 1:2
    ),
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
  expect_identical(
    add_data(p, "lookup", fn = function() data.frame(k = 1)),
    "lookup"
  )
  expect_identical(
    add_data(p, "by_scenario", fn = function(scenario) 1),
    "by_scenario"
  )
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
    add_analyses(
      p,
      sc,
      "a",
      fn = in_global(
        function(data, argset) {
          change(data$d)
          return(1)
        },
        change = in_global(changes[[nm]])
      )
    )
    out <- utils::capture.output(
      err <- tryCatch(run_plan(p), error = function(e) e)
    )
    # The first failed analysis stops the run.
    expect_match(
      conditionMessage(err),
      "^1 of 6 analyses failed, and the run stopped early: 2 analyses did not run\\. ",
      info = nm
    )
    expect_true(
      any(grepl(
        "The analysis changed the data 'd' in place",
        out,
        fixed = TRUE
      )),
      info = nm
    )
    expect_true(any(grepl("data.table::copy(data$d)", out, fixed = TRUE)))
  }
  p <- plnr::Plan$new()
  add_data(p, "d", fn = shared)
  add_analyses(
    p,
    sc,
    "a",
    fn = in_global(function(data, argset) {
      d <- data.table::copy(data$d)
      d[, z := 1]
      return(ncol(d))
    })
  )
  add_analyses(
    p,
    sc,
    "b",
    fn = in_global(function(data, argset) {
      return(ncol(data$d))
    })
  )
  r <- run_plan(p)
  expect_identical(r[["null__sim_0002__a__1"]], 3L)
  expect_identical(r[["null__sim_0002__b__1"]], 2L)
})

test_that("an analysis that changes values of the shared data in place fails and names the data and the columns, for a data.table, a data.frame and a list of data.tables", {
  sc <- small_scenarios()
  in_global <- function(f, ...) {
    environment(f) <- list2env(list(...), parent = globalenv())
    return(f)
  }
  table_fn <- in_global(function(scenario, sim) {
    return(data.table::data.table(
      x = 1:3,
      g = c("a", "b", "b"),
      y = c(2, 4, 6),
      f = factor(c("p", "q", "p")),
      l = c(TRUE, FALSE, NA),
      day = as.Date("2026-01-01") + 0:2
    ))
  })
  # data.frame() keeps 1:3 as a compact sequence, whose serialize() version
  # 3 holds its description only.
  frame_fn <- in_global(function(scenario, sim) {
    return(data.frame(x = 1:3, y = c(2, 4, 6), s = c("a", "b", "c")))
  })
  vector_fn <- in_global(function(scenario, sim) {
    return(c(1, 2, 3))
  })
  model_fn <- in_global(function(scenario, sim) {
    return(stats::lm(y ~ x, data = data.frame(x = 1:5, y = c(2, 4, 5, 8, 9))))
  })
  list_fn <- in_global(function(scenario, sim) {
    return(lapply(1:2, function(i) data.table::data.table(x = 1:3 + i)))
  })
  cases <- list(
    list(table_fn, function(d) d[1, x := 0L], "in the column 'x'"),
    list(table_fn, function(d) d[g == "b", y := NA], "in the column 'y'"),
    list(
      table_fn,
      function(d) data.table::set(d, 2L, "g", "z"),
      "in the column 'g'"
    ),
    list(
      table_fn,
      function(d) data.table::setattr(d$f, "levels", c("q", "p")),
      "in the column 'f'"
    ),
    list(table_fn, function(d) d[3, l := TRUE], "in the column 'l'"),
    list(table_fn, function(d) d[1, day := day + 1], "in the column 'day'"),
    # The column f reads p, q, p in both orders, so it does not change.
    list(
      table_fn,
      function(d) data.table::setorder(d, -x),
      "in the columns 'x', 'g', 'y', 'l', 'day'"
    ),
    list(
      table_fn,
      function(d) data.table::setattr(d, "label", "new"),
      "in the attributes of the table"
    ),
    list(
      frame_fn,
      function(d) data.table::set(d, 1L, "x", 0L),
      "in the column 'x'"
    ),
    list(
      frame_fn,
      function(d) data.table::set(d, 1L, "y", 0),
      "in the column 'y'"
    ),
    list(
      frame_fn,
      function(d) data.table::set(d, 3L, "s", "z"),
      "in the column 's'"
    ),
    list(list_fn, function(d) d[[2]][1, x := 0L], "in the column '[[2]]$x'"),
    list(
      list_fn,
      function(d) data.table::setattr(d, "names", c("p", "q")),
      "in the attributes of the list"
    ),
    list(
      vector_fn,
      function(d) data.table::setattr(d, "label", "new"),
      "in its value"
    ),
    list(
      model_fn,
      function(d) data.table::setattr(d$coefficients, "names", c("a", "b")),
      "in its value"
    )
  )
  for (i in seq_along(cases)) {
    p <- plnr::Plan$new()
    add_data(p, "d", fn = cases[[i]][[1]])
    add_analyses(
      p,
      sc,
      "a",
      fn = in_global(
        function(data, argset) {
          change(data$d)
          return(1)
        },
        change = in_global(cases[[i]][[2]])
      )
    )
    # expect_error() fails when the run does not stop.
    out <- utils::capture.output(
      expect_error(
        run_plan(p),
        "^1 of 6 analyses failed, and the run stopped early: 2 analyses did not run\\. ",
        info = paste("case", i)
      )
    )
    expect_identical(
      out[2:3],
      c(
        "pbn: the analysis real__sim_0001__a__1 failed:",
        paste0(
          "The analysis changed values of the shared data in place: the ",
          "data 'd', ",
          cases[[i]][[3]],
          ". plnr gives every analysis of one scenario and simulation the ",
          "same data object, so each later analysis in the same R session ",
          "would get the changed data, and a result would depend on the ",
          "order of the analyses. Copy the data first, for example ",
          "d <- data.table::copy(data$d), or make the change in a data step."
        )
      ),
      info = paste("case", i)
    )
  }
})

test_that("an analysis that reads the shared data without a change passes, also with a data.table subset that adds an index, and the next analysis gets the data as it was", {
  sc <- small_scenarios()
  in_global <- function(f) {
    environment(f) <- globalenv()
    return(f)
  }
  p <- plnr::Plan$new()
  add_data(
    p,
    "d",
    fn = in_global(function(scenario, sim) {
      return(data.table::data.table(
        x = c(3L, 1L, 2L),
        g = c("a", "b", "b"),
        y = c(2, 4, 6)
      ))
    })
  )
  add_data(
    p,
    "e",
    fn = in_global(function(scenario, sim) {
      return(data.frame(x = 1:3, y = c(2, 4, 6)))
    })
  )
  add_analyses(
    p,
    sc,
    "a",
    fn = in_global(function(data, argset) {
      d <- data$d
      e <- data$e
      return(c(
        nrow(d[g == "b"]),
        d[order(x)]$x[1],
        d[, .N, by = g]$N[2],
        d[g == "a", sum(y)],
        unname(stats::coef(stats::lm(y ~ x, data = e))[2]),
        sum(e$x + 0L),
        nrow(merge(d, e, by = "x"))
      ))
    })
  )
  add_analyses(
    p,
    sc,
    "b",
    fn = in_global(function(data, argset) {
      return(c(data$d$x, data$e$x))
    })
  )
  # Elements that are not data.frames: a vector, a model, a named list of
  # data.tables and an environment, which pbn does not check.
  add_data(p, "v", fn = in_global(function(scenario, sim) return(c(1, 2, 3))))
  add_data(
    p,
    "m",
    fn = in_global(function(scenario, sim) {
      return(stats::lm(y ~ x, data = data.frame(x = 1:5, y = c(2, 4, 5, 8, 9))))
    })
  )
  add_data(
    p,
    "l",
    fn = in_global(function(scenario, sim) {
      return(list(
        p = data.table::data.table(x = 1:2),
        q = data.table::data.table(x = 3:4)
      ))
    })
  )
  add_data(
    p,
    "en",
    fn = in_global(function(scenario, sim) {
      e <- new.env()
      e$k <- 5
      return(e)
    })
  )
  add_analyses(
    p,
    sc,
    "c",
    fn = in_global(function(data, argset) {
      return(c(
        sum(data$v),
        round(unname(stats::predict(data$m)[1]), 6),
        round(unname(stats::coef(summary(data$m))[2, 1]), 6),
        nrow(data$l$q[x > 3]),
        length(names(data$l)),
        get("k", envir = data$en)
      ))
    })
  )
  r <- run_plan(p)
  expect_identical(r[["null__sim_0002__c__1"]], c(6, 2, 1.8, 1, 2, 5))
  # The subset added an index to the shared table, which changes no value.
  expect_identical(
    r[["null__sim_0002__a__1"]],
    c(2, 1, 2, 2, 2, 6, 3)
  )
  expect_identical(r[["null__sim_0002__b__1"]], c(3L, 1L, 2L, 1:3))
})

test_that("an analysis that changes values in place after an analysis that read the same data fails, also with the hashes kept from the first analysis", {
  sc <- small_scenarios()
  in_global <- function(f) {
    environment(f) <- globalenv()
    return(f)
  }
  p <- plnr::Plan$new()
  add_data(
    p,
    "d",
    fn = in_global(function(scenario, sim) {
      return(data.table::data.table(x = 1:3, g = c("a", "b", "b")))
    })
  )
  add_analyses(
    p,
    sc,
    "a",
    fn = in_global(function(data, argset) {
      return(nrow(data$d[g == "b"]))
    })
  )
  add_analyses(
    p,
    sc,
    "b",
    fn = in_global(function(data, argset) {
      data$d[1, x := 0L]
      return(1)
    })
  )
  # expect_error() fails when the run does not stop.
  out <- utils::capture.output(
    expect_error(
      run_plan(p),
      "^1 of 9 analyses failed, and the run stopped early: 2 analyses did not run\\. "
    )
  )
  expect_identical(
    out[2:3],
    c(
      "pbn: the analysis real__sim_0001__b__1 failed:",
      paste0(
        "The analysis changed values of the shared data in place: the data ",
        "'d', in the column 'x'. plnr gives every analysis of one scenario ",
        "and simulation the same data object, so each later analysis in the ",
        "same R session would get the changed data, and a result would ",
        "depend on the order of the analyses. Copy the data first, for ",
        "example d <- data.table::copy(data$d), or make the change in a data ",
        "step."
      )
    )
  )
})

test_that("an analysis on a worker that changes the shared data in place fails too", {
  skip_if_pbn_not_installed()
  shared <- function(scenario, sim) {
    return(data.table::data.table(x = 1:3))
  }
  adds <- function(data, argset) {
    data$d[, z := 1]
    return(1)
  }
  environment(shared) <- globalenv()
  environment(adds) <- globalenv()
  p <- plnr::Plan$new()
  add_data(p, "d", fn = shared)
  add_analyses(p, small_scenarios(), "a", fn = adds)
  out <- utils::capture.output(
    err <- tryCatch(run_plan(p, workers = 2), error = function(e) e)
  )
  # A worker that does not know of the stop yet MAY fail its analysis too.
  expect_match(
    conditionMessage(err),
    "^[1-3] of 6 analyses failed, and the run stopped early: [0-5] analyses did not run\\. "
  )
  n <- as.integer(sub(" of 6 .*", "", conditionMessage(err)))
  expect_identical(
    sum(grepl("The analysis changed the data 'd' in place", out, fixed = TRUE)),
    n
  )
})

test_that("on a worker, an analysis that changes values of the shared data in place fails, and one that reads them passes", {
  skip_if_pbn_not_installed()
  shared <- function(scenario, sim) {
    return(data.table::data.table(x = 1:3, g = c("a", "b", "b")))
  }
  sets <- function(data, argset) {
    data$d[g == "b", x := 0L]
    return(1)
  }
  reads <- function(data, argset) {
    return(nrow(data$d[g == "b"]))
  }
  environment(shared) <- globalenv()
  environment(sets) <- globalenv()
  environment(reads) <- globalenv()
  p <- plnr::Plan$new()
  add_data(p, "d", fn = shared)
  add_analyses(p, small_scenarios(), "a", fn = sets)
  out <- utils::capture.output(
    err <- expect_error(
      run_plan(p, workers = 2),
      "^[1-3] of 6 analyses failed, and the run stopped early: [0-5] analyses did not run\\. "
    )
  )
  if (is.null(err)) {
    return(invisible(NULL))
  }
  n <- as.integer(sub(" of 6 .*", "", conditionMessage(err)))
  expect_identical(
    sum(grepl(
      "changed values of the shared data in place: the data 'd', in the column 'x'.",
      out,
      fixed = TRUE
    )),
    n
  )
  p <- plnr::Plan$new()
  add_data(p, "d", fn = shared)
  add_analyses(p, small_scenarios(), "a", fn = reads)
  r <- run_plan(p, workers = 2)
  expect_identical(r[["null__sim_0002__a__1"]], 2L)
})

test_that("the seed of the data does not depend on the scenario: two scenarios with the same data code and another setting draw the same numbers in one simulation, with 1 and 2 workers, and an analysis gets the same seed in both", {
  skip_if_pbn_not_installed()
  local_clean_state()
  data_fn <- function(scenario, sim, b) {
    return(data.frame(x = stats::rnorm(5), b = b))
  }
  environment(data_fn) <- globalenv()
  fn <- function(data, argset) {
    return(data.frame(
      x = data$d$x,
      b = data$d$b,
      draw = stats::runif(1),
      seed = argset$seed
    ))
  }
  environment(fn) <- globalenv()
  sc <- scenarios(list(
    scenarios = data.frame(
      scenario = c("real", "aa", "bbbbbb"),
      sims = c(1, 4, 4),
      b = c(0, 1, 2)
    )
  ))
  p <- plnr::Plan$new()
  add_data(p, "d", fn = data_fn)
  add_analyses(p, sc, "a", fn = fn)
  for (workers in 1:2) {
    r <- run_plan(p, workers = workers)
    aa <- r[["aa__sim_0003__a__1"]]
    bb <- r[["bbbbbb__sim_0003__a__1"]]
    expect_identical(aa$b, rep(1, 5), info = paste("workers", workers))
    expect_identical(bb$b, rep(2, 5), info = paste("workers", workers))
    expect_identical(aa$x, bb$x, info = paste("workers", workers))
    expect_identical(aa$draw, bb$draw, info = paste("workers", workers))
    expect_identical(aa$seed, bb$seed, info = paste("workers", workers))
    expect_identical(
      aa$seed[1],
      analysis_seed("a", 1, 3),
      info = paste("workers", workers)
    )
    four <- r[["aa__sim_0004__a__1"]]
    expect_false(identical(aa$x, four$x), info = paste("workers", workers))
    expect_false(
      identical(aa$seed, four$seed),
      info = paste("workers", workers)
    )
    # The data of simulation 3 is set.seed(3) and then the draws.
    set.seed(3)
    expect_identical(aa$x, stats::rnorm(5), info = paste("workers", workers))
  }
})
