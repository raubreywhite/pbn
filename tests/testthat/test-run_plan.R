two_scenarios <- function() {
  return(scenarios(list(
    scenarios = data.frame(scenario = c("real", "null"), sims = c(1, 2))
  )))
}

# A plan where the action fails for "real", and the data fails for
# simulation 2 with a message of more than 1000 bytes.
failing_plan <- function() {
  p <- plnr::Plan$new()
  data_fn <- function(scenario, sim) {
    if (sim == 2) {
      stop("planted data error ", strrep("x", 1200), " end of the message")
    }
    return(data.frame(x = 1:3))
  }
  action_fn <- function(data, argset) {
    if (argset$scenario == "real") {
      stop("planted action error")
    }
    return(data.frame(m = mean(data$d$x)))
  }
  # As in a project, the functions live in the global environment.
  environment(data_fn) <- globalenv()
  environment(action_fn) <- globalenv()
  add_data(p, "d", fn = data_fn)
  add_analyses(p, two_scenarios(), "a", fn = action_fn)
  return(p)
}

test_that("run_plan stops on a bad plan, bad workers and a data function with another key", {
  expect_error(run_plan(list()), "`p` must be a plan")
  p <- plnr::Plan$new()
  expect_error(
    run_plan(p),
    "no analysis from pbn::add_analyses()",
    fixed = TRUE
  )
  add_data(p, "d", fn = function(scenario, sim) return(data.frame(x = 1)))
  add_analyses(p, two_scenarios(), "a", fn = function(data, argset) 1)
  for (bad in list(0, 1.5, NA, Inf, "2", c(1, 2))) {
    expect_error(
      run_plan(p, workers = bad),
      "`workers` must be one whole number of 1 or more",
      info = format(bad)
    )
  }
  q <- plnr::Plan$new()
  add_data(q, "d", fn = function(scenario, outcome) return(data.frame(x = 1)))
  add_analyses(
    q,
    two_scenarios(),
    "a",
    fn = function(data, argset) 1,
    outcome = "y"
  )
  expect_error(
    run_plan(q),
    "The data function \"d\" has the arguments outcome without a default",
    fixed = TRUE
  )
})

test_that("run_plan with workers > 1 stops with a message when doParallel or foreach is missing", {
  p <- plnr::Plan$new()
  add_data(p, "d", fn = function(scenario, sim) return(data.frame(x = 1)))
  add_analyses(p, two_scenarios(), "a", fn = function(data, argset) 1)
  local_mocked_bindings(has_package = function(pkg) return(pkg != "doParallel"))
  expect_error(
    run_plan(p, workers = 2),
    "run_plan() with workers > 1 needs the packages doParallel. Install them",
    fixed = TRUE
  )
  local_mocked_bindings(has_package = function(pkg) return(FALSE))
  expect_error(
    run_plan(p, workers = 2),
    "needs the packages doParallel and foreach",
    fixed = TRUE
  )
  # workers = 1 needs neither.
  expect_identical(run_plan(p)[["null__sim_0002__a__1"]], 1)
})

test_that("a failing analysis: run_plan prints every failed analysis in full and stops, the same with 1 and 2 workers", {
  skip_if_pbn_not_installed()
  failed_output <- function(workers) {
    p <- failing_plan()
    out <- utils::capture.output(
      err <- tryCatch(run_plan(p, workers = workers), error = function(e) e)
    )
    return(list(out = out, err = conditionMessage(err)))
  }
  one <- failed_output(1)
  two <- failed_output(2)
  expect_identical(two, one)
  # The message names the first failed analysis and holds the start of its
  # error, cut below 900 bytes. The errors differ, so no "All" sentence.
  expect_true(startsWith(
    one$err,
    paste0(
      "3 of 6 analyses failed. pbn printed each failed analysis and its ",
      "error above, and saved nothing. The results files of an earlier run ",
      "stay as they are. The first failed analysis is ",
      "null__sim_0002__pbn_n_rows, with this error: \"The data function 'd' ",
      "failed: planted data error xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
    )
  ))
  expect_true(endsWith(one$err, "xxxx [...]\"."))
  expect_lt(nchar(one$err, "bytes"), 900)
  expect_no_match(one$err, "failed with this error", fixed = TRUE)
  text <- paste(one$out, collapse = "\n")
  expect_identical(
    grep("^pbn: the analysis", one$out, value = TRUE),
    c(
      "pbn: the analysis null__sim_0002__pbn_n_rows failed:",
      "pbn: the analysis real__sim_0001__a__1 failed:",
      "pbn: the analysis null__sim_0002__a__1 failed:"
    )
  )
  expect_match(text, "Error: planted action error", fixed = TRUE)
  # The whole message of more than 1000 bytes, with its end.
  expect_match(
    text,
    paste0(
      "The data function 'd' failed: planted data error ",
      strrep("x", 1200),
      " end of the message"
    ),
    fixed = TRUE
  )
})

test_that("every analysis fails with one long error: the message holds it cut below 900 bytes and says all failed with it, with 1 and 2 workers", {
  skip_if_pbn_not_installed()
  data_fn <- function(scenario, sim) {
    stop("one error ", strrep("z", 5000))
  }
  environment(data_fn) <- globalenv()
  p <- plnr::Plan$new()
  add_data(p, "d", fn = data_fn)
  add_analyses(p, two_scenarios(), "a", fn = function(data, argset) 1)
  for (workers in 1:2) {
    utils::capture.output(
      err <- tryCatch(run_plan(p, workers = workers), error = function(e) e)
    )
    msg <- conditionMessage(err)
    expect_lt(nchar(msg, "bytes"), 900)
    expect_match(
      msg,
      paste0(
        "6 of 6 analyses failed. pbn printed each failed analysis and its ",
        "error above, and saved nothing. The results files of an earlier ",
        "run stay as they are. The first failed analysis is ",
        "real__sim_0001__pbn_n_rows, with this error: \"The data function ",
        "'d' failed: one error zzzz"
      ),
      fixed = TRUE
    )
    expect_true(endsWith(msg, "zzzz [...]\". All 6 failed with this error."))
  }
})

test_that("failed_message cuts the first error at a whole character and adds the All sentence only for equal errors", {
  # A name of 1 and of 2 characters, so that one of the two cuts falls
  # inside a character of 2 bytes unless the cut keeps whole characters.
  for (nm in c("a", "ab")) {
    failed <- stats::setNames(paste(rep("\u00e9", 2000), collapse = ""), nm)
    msg <- failed_message(failed, 9)
    expect_true(validUTF8(msg), info = nm)
    expect_lt(nchar(msg, "bytes"), 900)
    expect_true(endsWith(msg, "\u00e9 [...]\"."), info = nm)
    expect_no_match(msg, "failed with this error", fixed = TRUE)
  }
  expect_true(endsWith(
    failed_message(c(a = "e1", b = "e1"), 2),
    "is a, with this error: \"e1\". All 2 failed with this error."
  ))
  expect_true(endsWith(
    failed_message(c(a = "e1", b = "e2"), 2),
    "is a, with this error: \"e1\"."
  ))
})

test_that("run_plan with workers = 2 gives results identical to workers = 1, on a PSOCK cluster that it stops", {
  skip_if_pbn_not_installed()
  p <- local_project()
  sourced <- init(p)
  plan <- build_plan(sourced, scenarios(sourced$CFG))
  one <- run_plan(plan)
  seen <- new.env()
  real_prepare <- prepare_workers
  local_mocked_bindings(prepare_workers = function(cl) {
    seen$cl <- cl
    return(real_prepare(cl))
  })
  two <- run_plan(plan, workers = 2)
  expect_s3_class(seen$cl, "SOCKcluster")
  expect_length(seen$cl, 2)
  expect_identical(two, one)
  expect_identical(names(one), names(plan$analyses))
  # A data.table from a worker has a valid self-reference again.
  expect_s3_class(two[["null__sim_0002__table1__1"]], "data.table")
  expect_gt(data.table::truelength(two[["null__sim_0002__table1__1"]]), 0L)
  # The cluster is stopped, and the sequential backend is registered.
  expect_error(parallel::clusterCall(seen$cl, function() 1))
  expect_identical(foreach::getDoParName(), "doSEQ")

  # Also after a failed run.
  seen$cl <- NULL
  expect_error(
    utils::capture.output(run_plan(failing_plan(), workers = 2)),
    "analyses failed"
  )
  expect_s3_class(seen$cl, "SOCKcluster")
  expect_error(parallel::clusterCall(seen$cl, function() 1))
  expect_identical(foreach::getDoParName(), "doSEQ")
})

test_that("the wrappers run on a PSOCK worker that has no object of this session", {
  skip_if_pbn_not_installed()
  user_data <- function(scenario, sim) {
    return(data.frame(scenario = scenario, draw = stats::runif(2)))
  }
  user_action <- function(data, argset) {
    return(data.frame(n = nrow(data$d), draw = stats::runif(1)))
  }
  environment(user_data) <- globalenv()
  environment(user_action) <- globalenv()
  data_w <- data_wrapper(user_data, "d")
  action_w <- action_wrapper(user_action)
  on_worker <- function(data_w, action_w) {
    d <- data_w(scenario = "null", sim = 3L)
    a <- action_w(list(d = d), list(sim = 3L, seed = 12345L))
    return(list(d = d, a = a))
  }
  environment(on_worker) <- globalenv()
  cl <- parallel::makePSOCKcluster(1)
  withr::defer(parallel::stopCluster(cl))
  got <- parallel::clusterCall(cl, on_worker, data_w, action_w)[[1]]
  d <- data_w(scenario = "null", sim = 3L)
  a <- action_w(list(d = d), list(sim = 3L, seed = 12345L))
  expect_identical(got, list(d = d, a = a))
})

test_that("an analysis that returns NULL keeps its place in the results", {
  p <- plnr::Plan$new()
  add_data(p, "d", fn = function(scenario, sim) return(data.frame(x = 1)))
  add_analyses(p, two_scenarios(), "a", fn = function(data, argset) {
    return(NULL)
  })
  r <- run_plan(p)
  expect_identical(names(r), names(p$analyses))
  expect_null(r[["null__sim_0002__a__1"]])
  expect_identical(r[["null__sim_0002__pbn_n_rows"]], c(d = 1L))
})

# A library with the package pbnprobe, whose function probe_value() returns
# "probe". The library is in .libPaths() of this R session only: no
# environment variable names it.
local_probe_library <- function(env = parent.frame()) {
  root <- tempfile("pbnprobe")
  src <- file.path(root, "pbnprobe")
  lib <- file.path(root, "lib")
  dir.create(file.path(src, "R"), recursive = TRUE)
  dir.create(lib)
  withr::defer(unlink(root, recursive = TRUE), envir = env)
  writeLines(
    c(
      "Package: pbnprobe",
      "Version: 0.0.1",
      "Title: A Probe",
      "Description: A probe for the tests of pbn.",
      "License: MIT",
      "Authors@R: person('A', 'B', role = c('aut', 'cre'), email = 'a@b.no')"
    ),
    file.path(src, "DESCRIPTION")
  )
  writeLines("export(probe_value)", file.path(src, "NAMESPACE"))
  writeLines(
    "probe_value <- function() return(\"probe\")",
    file.path(src, "R", "probe.R")
  )
  status <- system2(
    file.path(R.home("bin"), "R"),
    c("CMD", "INSTALL", "--no-test-load", "-l", shQuote(lib), shQuote(src)),
    stdout = FALSE,
    stderr = FALSE
  )
  testthat::expect_identical(status, 0L)
  lib <- normalizePath(lib, winslash = "/")
  withr::local_libpaths(lib, action = "prefix", .local_envir = env)
  return(lib)
}

test_that("a worker finds a package in a library that this R session added with .libPaths()", {
  skip_if_pbn_not_installed()
  lib <- local_probe_library()
  expect_false(grepl(lib, Sys.getenv("R_LIBS"), fixed = TRUE))
  p <- plnr::Plan$new()
  data_fn <- function(scenario, sim) {
    return(data.frame(v = pbnprobe::probe_value()))
  }
  action_fn <- function(data, argset) return(data$d)
  environment(data_fn) <- globalenv()
  environment(action_fn) <- globalenv()
  add_data(p, "d", fn = data_fn)
  add_analyses(p, two_scenarios(), "a", fn = action_fn)
  r <- run_plan(p, workers = 2)
  expect_identical(r[["null__sim_0002__a__1"]], data.frame(v = "probe"))
  # R_LIBS is back to its value before the run.
  expect_false(grepl(lib, Sys.getenv("R_LIBS"), fixed = TRUE))
  # pbnprobe was loaded on the workers only, and the manifest names it.
  expect_false("pbnprobe" %in% loadedNamespaces())
  expect_identical(
    unname(plan_info(p)$worker_packages["pbnprobe"]),
    "0.0.1"
  )
  expect_identical(
    unname(loaded_packages(plan_info(p)$worker_packages)["pbnprobe"]),
    "0.0.1"
  )
  # With 1 worker, this R session is the worker.
  run_plan(p)
  expect_null(plan_info(p)$worker_packages)
  unloadNamespace("pbnprobe")
})

test_that("a new worker gets the library paths of this R session in R_LIBS", {
  lib <- local_probe_library()
  cl <- make_cluster(1)
  withr::defer(parallel::stopCluster(cl))
  first_call <- function() {
    return(list(paths = .libPaths(), r_libs = Sys.getenv("R_LIBS")))
  }
  environment(first_call) <- baseenv()
  seen <- parallel::clusterCall(cl, first_call)[[1]]
  # R_LIBS reaches the worker. Its .libPaths() at start is not asserted: under
  # R CMD check on Windows the start-up of R sets other paths, and
  # prepare_workers() then sets the paths with set_lib_paths().
  expect_true(
    lib %in% strsplit(seen$r_libs, .Platform$path.sep, fixed = TRUE)[[1]],
    info = paste0(
      "lib: ", lib,
      "\nR_LIBS of the worker: ", seen$r_libs,
      "\n.libPaths() of the worker: ", paste(seen$paths, collapse = "; ")
    )
  )
})

test_that("prepare_workers() gives a worker the library paths of this R session", {
  skip_if_pbn_not_installed()
  lib <- local_probe_library()
  # A cluster that starts without the library paths of this R session.
  cl <- parallel::makePSOCKcluster(1)
  withr::defer(parallel::stopCluster(cl))
  ask <- function() return(.libPaths())
  environment(ask) <- baseenv()
  expect_false(lib %in% parallel::clusterCall(cl, ask)[[1]])
  prepare_workers(cl)
  expect_true(lib %in% parallel::clusterCall(cl, ask)[[1]])
})
