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
  q <- plnr::Plan$new()
  add_data(q, "d", fn = function(scenario, sim) return(data.frame(x = 1)))
  expect_error(
    get_data(q),
    "The plan has no analysis from pbn::add_analyses().",
    fixed = TRUE
  )
  expect_error(get_data(list()), "`p` must be a plan", fixed = TRUE)
})
