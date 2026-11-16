init <- function(p, scenario = "real", data_raw = p$data_default) {
  return(suppressMessages(initialize_project(
    "v01",
    p$home,
    data_raw,
    scenario = scenario,
    env = new.env()
  )))
}

results_files <- function(p) {
  return(sort(list.files(
    file.path(p$home, "v01_results"),
    recursive = TRUE,
    all.files = TRUE
  )))
}

test_that("run_arg, choose_scenario and choose_sim read the trailing arguments", {
  expect_identical(choose_scenario("real", character(0)), "real")
  expect_identical(choose_scenario("real", c("--x", "scenario=null")), "null")
  expect_identical(choose_sim(character(0)), 1L)
  expect_identical(choose_sim(c("scenario=null", "sim=2")), 2L)
  expect_error(choose_sim(c("sim=1", "sim=2")), "sim= more than once")
  expect_error(
    choose_scenario("real", c("scenario=a", "scenario=b")),
    "scenario= more than once"
  )
  for (bad in c("sim=0", "sim=1.5", "sim=x", "sim=-1", "sim=")) {
    expect_error(choose_sim(bad), "sim=", info = bad)
  }
  expect_error(choose_sim("sim=0"), "whole number of 1 or more")
  expect_error(choose_scenario("Real", character(0)), "lower-case")
})

test_that("initialize_project starts the real scenario in v01_results/real/", {
  p <- local_project()
  env <- new.env()
  out <- suppressMessages(
    initialize_project("v01", p$home, p$data_default, env = env)
  )
  expect_identical(
    names(out),
    c("version", "home", "scenario", "sim", "sims", "results", "data_raw")
  )
  expect_identical(out$scenario, "real")
  expect_identical(out$sim, 1L)
  expect_identical(out$sims, 1L)
  expect_identical(out$data_raw, p$data_default)
  expect_identical(out$results, file.path(out$home, "v01_results", "real"))
  expect_identical(
    normalizePath(org::project$results_today),
    normalizePath(out$results)
  )
  expect_true(exists("CFG", envir = env, inherits = FALSE))
  expect_true(exists("double_it", envir = env, inherits = FALSE))
  # No folder with today's date, and nothing in the results folder.
  expect_identical(
    list.files(file.path(p$home, "v01_results"), all.files = TRUE, no.. = TRUE),
    "real"
  )
  expect_identical(results_files(p), character(0))
})

test_that("a made-up scenario writes its data into the results folder and does not read data_raw", {
  p <- local_project()
  out <- init(p, "known", data_raw = file.path(p$root, "absent"))
  folder <- file.path(out$home, "v01_results", "known")
  expect_identical(out$results, folder)
  expect_identical(out$data_raw, normalizePath(file.path(folder, "data")))
  expect_identical(results_files(p), "known/data/d.csv")
  expect_identical(
    normalizePath(org::project$results_today),
    normalizePath(folder)
  )
  expect_identical(
    state$data_md5,
    c(d.csv = md5_of(file.path(folder, "data", "d.csv")))
  )

  # A file in the data folder from an earlier run is deleted.
  writeLines("x", file.path(folder, "data", "old.txt"))
  init(p, "known", data_raw = file.path(p$root, "absent"))
  expect_identical(results_files(p), "known/data/d.csv")
})

test_that("a scenario with 3 simulations writes simulation 1 to sim_0001/", {
  p <- local_project()
  out <- init(p, "null")
  folder <- file.path(out$home, "v01_results", "null", "sim_0001")
  expect_identical(out$results, folder)
  expect_identical(out$sims, 3L)
  expect_identical(
    normalizePath(org::project$results_today),
    normalizePath(folder)
  )
  expect_identical(results_files(p), "null/sim_0001/data/d.csv")
})

test_that("initialize_project sets the seed to the simulation", {
  p <- local_project()
  init(p, "known")
  after_init <- stats::runif(1)
  set.seed(1)
  expect_identical(after_init, stats::runif(1))
})

test_that("initialize_project sets the seed before org sources the functions folder", {
  p <- local_project()
  write_lines(
    p$home,
    "v01_functions/draw.R",
    "source_draw <- stats::runif(1)"
  )
  env <- new.env()
  set.seed(99)
  suppressMessages(initialize_project(
    "v01",
    p$home,
    p$data_default,
    scenario = "known",
    env = env
  ))
  set.seed(1)
  expect_identical(env$source_draw, stats::runif(1))
})

test_that("initialize_project stops on an unknown scenario and leaves no folder", {
  p <- local_project()
  expect_error(
    init(p, "absent"),
    "The scenario \"absent\" is not in CFG\\$scenarios"
  )
  expect_false(dir.exists(file.path(p$home, "v01_results")))

  dir.create(file.path(p$home, "v01_results", "real"), recursive = TRUE)
  expect_error(init(p, "absent"), "is not in CFG")
  expect_identical(
    list.files(file.path(p$home, "v01_results")),
    "real"
  )
})

test_that("initialize_project stops on a missing fake_<scenario>() and names it", {
  p <- local_project()
  write_lines(
    p$home,
    "v01_functions/_cfg.R",
    sub("\"null\")", "\"other\")", cfg_lines(), fixed = TRUE)
  )
  expect_error(init(p, "other"), "needs the function fake_other\\(dir\\)")
  expect_false(dir.exists(file.path(p$home, "v01_results")))
})

test_that("initialize_project stops on a scenario table without real", {
  p <- local_project()
  write_lines(
    p$home,
    "v01_functions/_cfg.R",
    sub("\"real\", ", "\"other\", ", cfg_lines(), fixed = TRUE)
  )
  expect_error(init(p, "known"), "must have a scenario named \"real\"")
  expect_false(dir.exists(file.path(p$home, "v01_results")))
})

test_that("initialize_project stops on a bad version or a missing real data folder", {
  p <- local_project()
  expect_error(initialize_project("V01", p$home, p$data_default), "two digits")
  expect_error(
    init(p, data_raw = file.path(p$root, "absent")),
    "data_raw folder does not exist"
  )
  expect_false(dir.exists(file.path(p$home, "v01_results")))
})

test_that("save_results stops on an object that fails check_results", {
  p <- local_project()
  init(p)
  expect_error(
    save_results(
      exclusions = data.frame(
        step = "a",
        n_before = 10,
        n_excluded = 1,
        n_after = 10
      ),
      estimates = data.frame(term = "x", estimate = 1),
      tables = list()
    ),
    "exclusions_table"
  )
  expect_identical(results_files(p), character(0))
})

test_that("save_results stops after an initialize_project() that stopped", {
  p <- local_project()
  init(p)
  expect_error(init(p, "absent"), "is not in CFG")
  expect_error(
    save_results(
      exclusions = data.frame(
        step = character(0),
        n_before = numeric(0),
        n_excluded = numeric(0),
        n_after = numeric(0)
      ),
      estimates = data.frame(term = "x", estimate = 1),
      tables = list()
    ),
    "Call pbn::initialize_project\\(\\) before"
  )
  expect_identical(results_files(p), character(0))
})

test_that("read_results reads the direct file, the stacked file or one simulation", {
  p <- local_project()
  for (rel in c("real", "null", "null/sim_0002")) {
    folder <- file.path(p$home, "v01_results", rel)
    dir.create(folder, recursive = TRUE, showWarnings = FALSE)
    qs2::qs_save(list(from = rel), file.path(folder, "results.qs2"))
  }
  expect_identical(read_results("v01", home = p$home)$from, "real")
  expect_identical(
    read_results("v01", scenario = "null", home = p$home)$from,
    "null"
  )
  expect_identical(
    read_results("v01", scenario = "null", sim = 2, home = p$home)$from,
    "null/sim_0002"
  )
  expect_error(
    read_results("v01", scenario = "null", sim = 3, home = p$home),
    "sim_0003/results.qs2"
  )
  expect_error(read_results("v01", sim = 0, home = p$home), "whole number")
  expect_error(
    read_results("v01", scenario = "../x", home = p$home),
    "lower-case"
  )
})

# The tests below run v01_run.R in a new R session.

test_that("each scenario writes only to its own folder, and the real results stay byte-identical", {
  skip_if_pbn_not_installed()
  p <- local_project()
  results <- file.path(p$home, "v01_results")

  res <- run_script(p$home)
  expect_identical(res$status, 0L, info = res$stdout)
  expect_identical(results_files(p), "real/results.qs2")
  real_before <- file_states(file.path(results, "real"))

  res <- run_script(p$home, "scenario=known")
  expect_identical(res$status, 0L, info = res$stdout)
  expect_identical(
    results_files(p),
    c("known/data/d.csv", "known/results.qs2", "real/results.qs2")
  )
  expect_identical(file_states(file.path(results, "real")), real_before)

  before <- results_files(p)
  res <- run_script(p$home, c("scenario=null", "sim=2"))
  expect_identical(res$status, 0L, info = res$stdout)
  expect_identical(
    setdiff(results_files(p), before),
    c("null/sim_0002/data/d.csv", "null/sim_0002/results.qs2")
  )
  expect_identical(file_states(file.path(results, "real")), real_before)

  real <- read_results("v01", home = p$home)
  known <- read_results("v01", scenario = "known", home = p$home)
  null2 <- read_results("v01", scenario = "null", sim = 2, home = p$home)
  expect_identical(check_results(null2)$status, rep("PASS", 5))
  expect_identical(
    real$manifest[c("scenario", "sim", "sims", "data_raw")],
    list(scenario = "real", sim = 1L, sims = 1L, data_raw = p$data_default)
  )
  expect_identical(
    null2$manifest[c("scenario", "sim", "sims")],
    list(scenario = "null", sim = 2L, sims = 3L)
  )
  expect_identical(
    null2$manifest$data_raw,
    normalizePath(
      file.path(results, "null", "sim_0002", "data"),
      winslash = "/"
    )
  )
  expect_false("data_raw_source" %in% names(real$manifest))
  expect_identical(names(null2$manifest$data_md5), "d.csv")
  # Planted: 3 ones of x in group a of the real data, 4 in known.
  expect_equal(real$estimates$estimate, 0.4)
  expect_equal(known$estimates$estimate, 0.5)
  t1 <- known$tables$table1
  expect_identical(t1$events[t1$var == "x" & t1$grp == "a"], 4L)
  expect_identical(null2$manifest$n_rows_data, 40L)
})

test_that("a simulation run twice gives the same bytes, and two simulations differ", {
  skip_if_pbn_not_installed()
  p <- local_project()
  folder <- file.path(p$home, "v01_results", "null")
  run_script(p$home, c("scenario=null", "sim=2"))
  first <- md5_of(file.path(folder, "sim_0002", "results.qs2"))
  first_data <- md5_of(file.path(folder, "sim_0002", "data", "d.csv"))

  res <- run_script(p$home, c("scenario=null", "sim=2"))
  expect_identical(res$status, 0L, info = res$stdout)
  expect_identical(md5_of(file.path(folder, "sim_0002", "results.qs2")), first)
  expect_identical(
    md5_of(file.path(folder, "sim_0002", "data", "d.csv")),
    first_data
  )

  run_script(p$home, c("scenario=null", "sim=3"))
  e2 <- read_results("v01", scenario = "null", sim = 2, home = p$home)$estimates
  e3 <- read_results("v01", scenario = "null", sim = 3, home = p$home)$estimates
  expect_false(identical(e2$estimate, e3$estimate))
  expect_false(identical(
    md5_of(file.path(folder, "sim_0003", "data", "d.csv")),
    first_data
  ))
})

test_that("the run script stops on a bad scenario or simulation and names the problem", {
  skip_if_pbn_not_installed()
  p <- local_project()
  bad <- list(
    list(
      args = "scenario=absent",
      msg = "The scenario \"absent\" is not in CFG$scenarios"
    ),
    list(
      args = c("scenario=known", "sim=2"),
      msg = "The simulation 2 is more than the 1 simulations of the scenario \"known\""
    ),
    list(
      args = c("scenario=null", "sim=4"),
      msg = "The simulation 4 is more than the 3 simulations"
    ),
    list(
      args = c("sim=1", "sim=2"),
      msg = "The command line gives sim= more than once"
    ),
    list(args = "sim=0", msg = "sim= must be a whole number of 1 or more")
  )
  for (b in bad) {
    res <- run_script(p$home, b$args)
    info <- paste(b$args, collapse = " ")
    expect_identical(res$status, 1L, info = info)
    expect_match(res$stdout, b$msg, fixed = TRUE, info = info)
  }
  expect_false(dir.exists(file.path(p$home, "v01_results")))

  write_lines(
    p$home,
    "v01_functions/_cfg.R",
    sub("\"null\")", "\"other\")", cfg_lines(), fixed = TRUE)
  )
  res <- run_script(p$home, "scenario=other")
  expect_identical(res$status, 1L)
  expect_match(res$stdout, "needs the function fake_other(dir)", fixed = TRUE)
  expect_false(dir.exists(file.path(p$home, "v01_results")))
})

test_that("data_md5 lists only the files that existed at initialize_project()", {
  skip_if_pbn_not_installed()
  p <- local_project()
  # The run writes into the results folder and into the data folder before
  # save_results().
  lines <- append(
    run_script_lines("v01", p$data_default),
    c(
      "writeLines(\"x\", file.path(p$results, \"fig.txt\"))",
      "writeLines(\"x\", file.path(p$data_raw, \"made_by_run.txt\"))"
    ),
    after = 1
  )
  writeLines(lines, file.path(p$home, "v01_run.R"))
  run_script(p$home, "scenario=known")
  folder <- file.path(p$home, "v01_results", "known")
  r <- read_results("v01", scenario = "known", home = p$home)
  expect_identical(names(r$manifest$data_md5), "d.csv")
  expect_identical(
    unname(r$manifest$data_md5),
    md5_of(file.path(folder, "data", "d.csv"))
  )
  expect_true(file.exists(file.path(folder, "data", "made_by_run.txt")))
  expect_true(file.exists(file.path(folder, "fig.txt")))
})

test_that("the manifest packages name qs2, and run_date is the date only", {
  skip_if_pbn_not_installed()
  p <- local_project()
  run_script(p$home)
  r <- read_results("v01", home = p$home)
  expect_true(all(
    c("data.table", "org", "pbn", "qs2") %in% names(r$manifest$packages)
  ))
  expect_identical(
    unname(r$manifest$packages["qs2"]),
    as.character(utils::packageVersion("qs2"))
  )
  expect_identical(
    r$manifest$packages[["pbn"]],
    as.character(utils::packageVersion("pbn"))
  )
  expect_false("stats" %in% names(r$manifest$packages))
  expect_match(r$manifest$run_date, "^[0-9]{4}-[0-9]{2}-[0-9]{2}$")
})
