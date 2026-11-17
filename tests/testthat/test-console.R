# get_data() and get_argset(): the data and the argset of one analysis
# without its name.

console_plan <- function() {
  p <- plnr::Plan$new()
  add_data(p, "d", fn = function(scenario, sim) {
    return(data.frame(scenario = scenario, sim = sim, x = stats::rnorm(3)))
  })
  sc <- scenarios(list(
    scenarios = data.frame(scenario = c("real", "null"), sims = c(1, 3))
  ))
  add_analyses(
    p,
    sc,
    "models",
    fn = function(data, argset) return(data.frame(m = mean(data$d$x))),
    outcome = c("y", "z")
  )
  add_analyses(
    p,
    sc,
    "crude",
    fn = function(data, argset) return(1),
    control = TRUE
  )
  return(p)
}

test_that("get_data gives the data that each analysis of the scenario and simulation gets, before and after run_plan", {
  p <- console_plan()
  got <- get_data(p, "null", 3)
  expect_identical(got, p$get_data("null__sim_0003__models__2"))
  expect_identical(got$d$scenario, rep("null", 3))
  expect_identical(got$d$sim, rep(3L, 3))
  # The seed is sim, so a second call gives the same data.
  expect_identical(get_data(p, "null", 3), got)
  expect_identical(get_data(p)$d, p$get_data("real__sim_0001__models__1")$d)
  r <- run_plan(p)
  expect_identical(get_data(p, "null", 3), got)
  expect_identical(
    r[["null__sim_0003__models__2"]],
    data.frame(m = mean(got$d$x))
  )
})

test_that("get_argset gives the argset of one analysis from its scenario, simulation, kind and counter", {
  p <- console_plan()
  expect_identical(
    get_argset(p, "null", 2, "models", 2),
    p$get_argset("null__sim_0002__models__2")
  )
  expect_identical(get_argset(p, "null", 2, "models", 2)$outcome, "z")
  expect_identical(
    get_argset(p, "null", 2, "models"),
    p$get_argset("null__sim_0002__models__1")
  )
  expect_identical(
    get_argset(p, name = "models"),
    p$get_argset("real__sim_0001__models__1")
  )
})

test_that("get_data and get_argset stop on a scenario, a simulation, a kind or a counter that the plan does not hold, and name the values it holds", {
  p <- console_plan()
  stops <- list(
    "`scenario` must be one of the scenarios of the plan: real, null." = quote(get_data(p, "nul")),
    "`scenario` must be one of the scenarios of the plan: real, null." = quote(get_data(p, c("real", "null"))),
    "`sim` must be a whole number from 1 to 3, the simulations of the scenario \"null\"." = quote(get_data(p, "null", 4)),
    "`sim` must be a whole number from 1 to 3, the simulations of the scenario \"null\"." = quote(get_data(p, "null", 0)),
    "`sim` must be a whole number from 1 to 1, the simulations of the scenario \"real\"." = quote(get_data(p, "real", 2)),
    "`sim` must be a whole number from 1 to 3, the simulations of the scenario \"null\"." = quote(get_argset(p, "null", 1.5, "models")),
    "`name` must be one of the analysis kinds of the plan: models, crude." = quote(get_argset(p, "null", 1, "model")),
    "`name` must be one analysis kind, for example \"models\"." = quote(get_argset(p, "null", 1)),
    "The kind \"crude\" has no analysis for the scenario \"real\". A kind with control = TRUE has none for \"real\"." = quote(get_argset(p, "real", 1, "crude")),
    "`i` must be a whole number from 1 to 2, the counters of the kind \"models\"." = quote(get_argset(p, "null", 1, "models", 3))
  )
  for (i in seq_along(stops)) {
    expect_error(
      eval(stops[[i]]),
      names(stops)[i],
      fixed = TRUE,
      info = deparse(stops[[i]])
    )
  }
  # A plan without data and without analyses.
  expect_error(
    get_data(plnr::Plan$new()),
    "The plan has no analysis from pbn::add_analyses().",
    fixed = TRUE
  )
  q <- plnr::Plan$new()
  add_data(q, "d", fn = function(scenario, sim) return(data.frame(x = 1)))
  expect_error(
    get_argset(q, name = "models"),
    "The plan has no analysis from pbn::add_analyses().",
    fixed = TRUE
  )
  expect_error(get_data(list()), "`p` must be a plan", fixed = TRUE)
})

test_that("get_data works right after add_data, with the scenarios of the scenarios() call before add_data, and gives the data of the plan after add_analyses", {
  local_clean_state()
  raw <- function(scenario, sim) {
    return(data.frame(scenario = scenario, sim = sim, x = stats::rnorm(3)))
  }
  cfg <- list(scenarios = data.frame(
    scenario = c("real", "null"),
    sims = c(1, 3),
    n_boot = c(200, 5)
  ))
  # Without a call of scenarios() before add_data().
  q <- plnr::Plan$new()
  add_data(q, "d", raw = raw)
  sc <- scenarios(cfg)
  expect_error(
    get_data(q, "null", 2),
    paste0(
      "The plan has no analysis yet, so pbn::get_data() needs the scenarios ",
      "of the run. Call pbn::scenarios(CFG) before pbn::add_data()."
    ),
    fixed = TRUE
  )
  p <- plnr::Plan$new()
  add_data(p, "d", raw = raw, clean = function(d) return(d[, c("x", "scenario", "sim")]))
  add_data(p, "boot", fn = function(n_boot) return(n_boot))
  early <- get_data(p, "null", 2)
  expect_identical(names(early), c("d", "boot"))
  expect_identical(early$boot, 5)
  expect_identical(get_data(p)$d$scenario, rep("real", 3))
  expect_error(
    get_data(p, "null", 4),
    "`sim` must be a whole number from 1 to 3, the simulations of the scenario \"null\".",
    fixed = TRUE
  )
  expect_error(
    get_data(p, "known"),
    "`scenario` must be one of the scenarios of the plan: real, null.",
    fixed = TRUE
  )
  # A later call of scenarios() for another plan does not change the
  # scenarios of `p`.
  scenarios(cfg, only = "null")
  expect_identical(get_data(p)$d$scenario, rep("real", 3))
  # The same data as after add_analyses().
  add_analyses(p, sc, "m", fn = function(data, argset) return(1))
  late <- get_data(p, "null", 2)
  expect_identical(late[c("d", "boot")], early)

  # initialize_project() forgets the scenarios. It is called directly here:
  # init() of the helpers empties the state of pbn itself first.
  pr <- local_project()
  sourced <- new.env(parent = globalenv())
  suppressMessages(initialize_project("v01", pr$home, pr$data_default, env = sourced))
  r <- plnr::Plan$new()
  add_data(r, "d", fn = function(scenario, sim) return(data.frame(x = sim)))
  expect_error(get_data(r), "Call pbn::scenarios(CFG) before pbn::add_data().", fixed = TRUE)
})

test_that("get_data gives the same random data before add_analyses, after it and through p$get_data(), for two simulations", {
  local_clean_state()
  sc <- scenarios(list(
    scenarios = data.frame(scenario = c("real", "null"), sims = c(1, 3))
  ))
  p <- plnr::Plan$new()
  add_data(p, "d", fn = function(scenario, sim) return(data.frame(x = stats::rnorm(4))))
  add_data(
    p,
    "e",
    raw = function(scenario, sim) return(data.frame(y = stats::runif(2))),
    clean = function(d) return(d)
  )
  set.seed(999)
  before <- list(get_data(p, "null", 2), get_data(p, "null", 3))
  add_analyses(p, sc, "a", fn = function(data, argset) return(1))
  set.seed(1)
  after <- list(get_data(p, "null", 2), get_data(p, "null", 3))
  by_plnr <- list(
    p$get_data("null__sim_0002__a__1"),
    p$get_data("null__sim_0003__a__1")
  )
  for (k in 1:2) {
    expect_identical(after[[k]][c("d", "e")], before[[k]])
    expect_identical(by_plnr[[k]][c("d", "e")], before[[k]])
  }
  expect_false(identical(before[[1]]$d, before[[2]]$d))
})

test_that("get_data right after add_data uses the cache with its one line", {
  p <- local_project()
  sourced <- init(p)
  sc <- scenarios(sourced$CFG)
  plan <- plnr::Plan$new()
  fn <- function(scenario, sim) return(sourced$get_data(scenario, sim))
  environment(fn) <- list2env(list(sourced = sourced), parent = globalenv())
  add_data(plan, "d", fn = fn, cache = TRUE)
  first <- utils::capture.output(d1 <- get_data(plan, "null", 2)$d)
  expect_identical(
    first,
    "pbn: data d, null sim_0002: computed, because there is no cache file."
  )
  second <- utils::capture.output(d2 <- get_data(plan, "null", 2)$d)
  expect_match(second, "^pbn: data d, null sim_0002: reused the cache of ")
  expect_length(second, 1)
  # The record of the cache differs: computed, then reused.
  attr(d1, "pbn_cache") <- NULL
  attr(d2, "pbn_cache") <- NULL
  expect_identical(d2, d1)
})
