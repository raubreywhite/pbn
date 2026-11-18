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

test_that("run_plan with workers > 1 stops with a message when future or doFuture is missing", {
  p <- plnr::Plan$new()
  add_data(p, "d", fn = function(scenario, sim) return(data.frame(x = 1)))
  add_analyses(p, two_scenarios(), "a", fn = function(data, argset) 1)
  local_mocked_bindings(has_package = function(pkg) return(pkg != "doFuture"))
  expect_error(
    run_plan(p, workers = 2),
    paste0(
      "run_plan() with workers > 1 needs the packages doFuture: install ",
      "them, or use workers = 1."
    ),
    fixed = TRUE
  )
  local_mocked_bindings(has_package = function(pkg) return(FALSE))
  expect_error(
    run_plan(p, workers = 2),
    "needs the packages future and doFuture: install them",
    fixed = TRUE
  )
  # workers = 1 needs neither.
  expect_identical(run_plan(p)[["null__sim_0002__a__1"]], 1)
})

test_that("a failing analysis: run_plan prints each failed analysis in full and stops early, with 1 and 2 workers", {
  skip_if_pbn_not_installed()
  failed_output <- function(workers) {
    p <- failing_plan()
    out <- utils::capture.output(
      err <- tryCatch(run_plan(p, workers = workers), error = function(e) e)
    )
    return(list(out = out, err = conditionMessage(err)))
  }
  one <- failed_output(1)
  # With 1 worker the analyses that count the rows come first. The data of
  # null 2 fails, its analyses fail with it, and the run stops. The message
  # names the first failed analysis and holds the start of its error, cut
  # below 900 bytes. The analysis of real, with another error, did not run.
  expect_true(startsWith(
    one$err,
    paste0(
      "2 of 6 analyses failed, and the run stopped early: 2 analyses did not ",
      "run. pbn printed each failed analysis and its ",
      "error above, and saved nothing. The results files of an earlier run ",
      "stay as they are. The first failed analysis is ",
      "null__sim_0002__pbn_n_rows, with this error: \"The data function 'd' ",
      "failed: planted data error xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
    )
  ))
  expect_true(endsWith(one$err, "xxxx [...]\". All 2 failed with this error."))
  expect_lt(nchar(one$err, "bytes"), 900)
  text <- paste(one$out, collapse = "\n")
  expect_identical(
    grep("^pbn: the analysis", one$out, value = TRUE),
    c(
      "pbn: the analysis null__sim_0002__pbn_n_rows failed:",
      "pbn: the analysis null__sim_0002__a__1 failed:"
    )
  )
  expect_no_match(text, "planted action error", fixed = TRUE)
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
  # With 2 workers, each worker stops at its next analysis after the first
  # failure, so a worker MAY fail an analysis before it sees the stop. Each
  # printed failure is one of the 3 analyses that fail, in full.
  two <- failed_output(2)
  expect_match(
    two$err,
    "^[1-3] of 6 analyses failed, and the run stopped early: [0-4] analyses did not run\\. "
  )
  printed <- grep("^pbn: the analysis", two$out, value = TRUE)
  expect_true(length(printed) >= 1)
  expect_true(all(
    printed %in%
      c(
        "pbn: the analysis null__sim_0002__pbn_n_rows failed:",
        "pbn: the analysis real__sim_0001__a__1 failed:",
        "pbn: the analysis null__sim_0002__a__1 failed:"
      )
  ))
  n <- as.integer(sub(" of 6 .*", "", two$err))
  expect_identical(length(printed), n)
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
    # With 1 worker the 2 analyses of real fail, and the run stops. With 2
    # workers, each worker MAY fail its analyses before it sees the stop.
    expect_match(
      msg,
      if (workers == 1) {
        "^2 of 6 analyses failed, and the run stopped early: 4 analyses did not run\\. "
      } else {
        "^[2-6] of 6 analyses failed(, and the run stopped early: [0-4] analyses did not run)?\\. "
      }
    )
    expect_match(
      msg,
      paste0(
        "pbn printed each failed analysis and its ",
        "error above, and saved nothing. The results files of an earlier ",
        "run stay as they are. The first failed analysis is "
      ),
      fixed = TRUE
    )
    # With 2 workers, a worker that started later MAY fail first.
    expect_match(
      msg,
      paste0(
        if (workers == 1) "real__sim_0001" else "[a-z]+__sim_000[12]",
        "__pbn_n_rows, with this error: \"The data function 'd' failed: ",
        "one error zzzz"
      )
    )
    n <- sub(" of 6 .*", "", msg)
    expect_true(endsWith(
      msg,
      paste0("zzzz [...]\". All ", n, " failed with this error.")
    ))
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
      "lib: ",
      lib,
      "\nR_LIBS of the worker: ",
      seen$r_libs,
      "\n.libPaths() of the worker: ",
      paste(seen$paths, collapse = "; ")
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

test_that("a worker uses the kind of random number generator of this R session, and the results stay identical to 1 worker", {
  skip_if_pbn_not_installed()
  old <- RNGkind()
  withr::defer(RNGkind(old[1], old[2], old[3]))
  RNGkind("L'Ecuyer-CMRG")
  data_fn <- function(scenario, sim) return(data.frame(x = stats::runif(2)))
  action_fn <- function(data, argset) {
    return(list(kind = RNGkind()[1], draw = stats::runif(1), x = data$d$x))
  }
  environment(data_fn) <- globalenv()
  environment(action_fn) <- globalenv()
  p <- plnr::Plan$new()
  add_data(p, "d", fn = data_fn)
  add_analyses(p, two_scenarios(), "a", fn = action_fn)
  one <- run_plan(p)
  two <- run_plan(p, workers = 2)
  expect_identical(two, one)
  expect_identical(two[["null__sim_0002__a__1"]]$kind, "L'Ecuyer-CMRG")
})

# Runs run_plan(), and returns the error, the output, the run log and the
# errors file, and what the run left: the future plan, the foreach backend,
# the option of doFuture and the cluster.
run_outside <- function(plan, p, workers) {
  seen <- new.env()
  real_make <- make_cluster
  local_mocked_bindings(make_cluster = function(workers) {
    seen$cl <- real_make(workers)
    return(seen$cl)
  })
  before <- future::plan()
  out <- utils::capture.output(
    err <- tryCatch(run_plan(plan, workers = workers), error = function(e) e)
  )
  results <- file.path(p$home, "v01_results")
  return(list(
    err = err,
    out = out,
    log = readLines(file.path(results, "run.md")),
    errors = readLines(file.path(results, "run_errors_gitexclude.md")),
    plan_back = identical(future::plan(), before),
    backend = foreach::getDoParName(),
    rng_option = getOption("doFuture.rng.onMisuse"),
    cluster = seen$cl
  ))
}

expect_run_error <- function(got, text) {
  msg <- conditionMessage(got$err)
  expect_true(startsWith(
    msg,
    paste0(
      "run_plan() stopped outside an analysis: a worker stopped, or plnr or ",
      "the backend of the workers failed. pbn printed the whole error above, ",
      "and saved nothing."
    )
  ))
  expect_lt(nchar(msg, "bytes"), 900)
  expect_match(msg, text, fixed = TRUE)
  expect_identical(
    got$out[1:2],
    c(
      "pbn: wrote the run log v01_results/run.md.",
      "pbn: run_plan() stopped outside an analysis:"
    )
  )
  expect_true(any(grepl(text, got$errors, fixed = TRUE)))
  expect_true(any(grepl(
    "run_plan() stopped outside an analysis, so save_results() did not run.",
    got$log,
    fixed = TRUE
  )))
  expect_false(any(grepl(text, got$log, fixed = TRUE)))
  expect_true(got$plan_back)
  expect_identical(got$backend, "doSEQ")
  expect_null(got$rng_option)
  expect_null(getOption("pbn.run"))
  return(invisible(NULL))
}

test_that("a worker that stops ends run_plan with one message and a failed run log, and the backend goes back and the cluster stops", {
  skip_if_pbn_not_installed()
  main <- Sys.getpid()
  # The analysis stops its R session when it runs on a worker, never here.
  stops <- function(data, argset) {
    if (Sys.getpid() != main && argset$sim == 2) {
      quit(save = "no", status = 1)
    }
    return(1)
  }
  environment(stops) <- list2env(list(main = main), parent = globalenv())
  # Two plans, because future reports the stop in two ways: as an interrupt
  # of a running future, or as an error when it launches the next one. The
  # second one used to be lost: the closed connection of the stopped worker
  # made stopCluster() fail, and its error replaced the one of the run.
  for (small in c(FALSE, TRUE)) {
    p <- local_project()
    sourced <- init(p)
    if (small) {
      data_fn <- function(scenario, sim) return(data.frame(x = 1))
      environment(data_fn) <- globalenv()
      plan <- plnr::Plan$new()
      add_data(plan, "d", fn = data_fn)
      sc <- scenarios(list(
        scenarios = data.frame(scenario = c("real", "null"), sims = c(1, 4))
      ))
      add_analyses(plan, sc, "stops", fn = stops, k = 1:3)
    } else {
      plan <- build_plan(sourced, scenarios(sourced$CFG))
      add_analyses(plan, scenarios(sourced$CFG), "stops", fn = stops)
    }
    # doFuture warns that it cancels the other futures.
    got <- suppressWarnings(run_outside(plan, p, 2))
    expect_run_error(got, "Future")
    expect_no_match(
      conditionMessage(got$err),
      "invalid connection",
      fixed = TRUE
    )
    expect_s3_class(got$cluster, "SOCKcluster")
    expect_error(parallel::clusterCall(got$cluster, function() 1))
  }
})

test_that("an error of plnr outside the wrappers of pbn ends run_plan with one message and a failed run log, with 1 and 2 workers", {
  skip_if_pbn_not_installed()
  for (workers in 1:2) {
    p <- local_project()
    sourced <- init(p)
    plan <- build_plan(sourced, scenarios(sourced$CFG))
    # A data function of plnr itself, without the wrapper of add_data().
    plan$add_data("outside", fn = function() {
      stop("planted outside the wrappers")
    })
    got <- run_outside(plan, p, workers)
    expect_run_error(got, "planted outside the wrappers")
  }
})

test_that("run_error_message cuts a long error at a whole character below 900 bytes", {
  msg <- run_error_message(paste(rep("\u00e9", 3000), collapse = ""))
  expect_true(validUTF8(msg))
  expect_lt(nchar(msg, "bytes"), 900)
  expect_true(endsWith(msg, "\u00e9 [...]\"."))
  expect_true(endsWith(run_error_message("short"), "The error is: \"short\"."))
})

test_that("a run stops at its first failed analysis: no later data function or analysis runs, with 1 worker, and with 2 workers fewer than all", {
  skip_if_pbn_not_installed()
  marks <- withr::local_tempdir()
  data_fn <- function(scenario, sim) {
    if (scenario == "real") {
      stop("the raw data is missing")
    }
    file.create(file.path(marks, paste0("data_", sim)))
    return(data.frame(x = 1))
  }
  action_fn <- function(data, argset) {
    file.create(file.path(marks, paste0("analysis_", argset$sim)))
    Sys.sleep(0.2)
    return(1)
  }
  environment(data_fn) <- list2env(list(marks = marks), parent = globalenv())
  environment(action_fn) <- environment(data_fn)
  sc <- scenarios(list(
    scenarios = data.frame(scenario = c("real", "null"), sims = c(1, 20))
  ))
  for (workers in 1:2) {
    unlink(list.files(marks, full.names = TRUE))
    p <- plnr::Plan$new()
    add_data(p, "d", fn = data_fn)
    add_analyses(p, sc, "a", fn = action_fn)
    out <- utils::capture.output(
      err <- tryCatch(run_plan(p, workers = workers), error = function(e) e)
    )
    ran <- list.files(marks, pattern = "^analysis_")
    if (workers == 1) {
      # The analyses that count the rows come first, so the data of real
      # fails before any data of null.
      expect_match(
        conditionMessage(err),
        "^2 of 42 analyses failed, and the run stopped early: 40 analyses did not run\\. "
      )
      expect_identical(list.files(marks), character(0))
    } else {
      expect_match(
        conditionMessage(err),
        "^2 of 42 analyses failed, and the run stopped early: [0-9]+ analyses did not run\\. "
      )
      expect_lt(length(ran), 20)
    }
    expect_identical(
      grep("^pbn: the analysis", out, value = TRUE),
      c(
        "pbn: the analysis real__sim_0001__pbn_n_rows failed:",
        "pbn: the analysis real__sim_0001__a__1 failed:"
      ),
      info = paste("workers:", workers)
    )
  }
})

test_that("run_stopping sees the folder of a stop that another R session made, before each data function and at most every 0.5 seconds before an analysis", {
  run <- new.env()
  run$stop_path <- file.path(withr::local_tempdir(), "stop")
  run$every <- 0.5
  run$progressor <- NULL
  run_reset(run)
  expect_false(run_stopping(run))
  # Another R session fails an analysis.
  dir.create(run$stop_path)
  # Within 0.5 seconds of the last look, an analysis does not look again.
  expect_false(run_stopping(run))
  expect_true(run_stopping(run, force = TRUE))
  expect_true(run_stopping(run))
  # A new future of a worker forgets the stop, and finds the folder again.
  run_reset(run)
  expect_true(run_stopping(run))
  # The first failure in this R session stops it at once, and makes the
  # folder for the others.
  unlink(run$stop_path, recursive = TRUE)
  run_reset(run)
  run_failed(run, list(scenario = "real"))
  expect_true(run$stopped)
  expect_true(dir.exists(run$stop_path))
  expect_true(run_stopping(run))
})

# A plan whose analyses report the threads of the R session where they run:
# the threads of data.table, the variables of thread_variables and the pid.
threads_plan <- function() {
  fn <- function(data, argset) {
    vars <- c("OMP_NUM_THREADS", "OPENBLAS_NUM_THREADS", "MKL_NUM_THREADS")
    return(data.frame(
      dt = data.table::getDTthreads(),
      var = vars,
      value = unname(Sys.getenv(vars, unset = "unset")),
      pid = Sys.getpid()
    ))
  }
  environment(fn) <- globalenv()
  data_fn <- function(scenario, sim) return(data.frame(x = 1:3))
  environment(data_fn) <- globalenv()
  p <- plnr::Plan$new()
  add_data(p, "d", fn = data_fn)
  sc <- scenarios(list(
    scenarios = data.frame(scenario = c("real", "null"), sims = c(1, 4))
  ))
  add_analyses(p, sc, "threads", fn = fn)
  return(p)
}

test_that("make_cluster starts the workers with 1 thread for OpenMP and BLAS, and this R session gets its own values back", {
  withr::local_envvar(
    OMP_NUM_THREADS = "3",
    OPENBLAS_NUM_THREADS = NA,
    MKL_NUM_THREADS = NA
  )
  local_mocked_bindings(
    makePSOCKcluster = function(workers) {
      return(Sys.getenv(c(thread_variables, "R_LIBS"), unset = "unset"))
    },
    .package = "parallel"
  )
  at_start <- make_cluster(2)
  expect_identical(
    unname(at_start[thread_variables]),
    c("1", "1", "1")
  )
  expect_identical(Sys.getenv("OMP_NUM_THREADS"), "3")
  expect_identical(Sys.getenv("OPENBLAS_NUM_THREADS", unset = "unset"), "unset")
  expect_identical(Sys.getenv("MKL_NUM_THREADS", unset = "unset"), "unset")
})

test_that("single_thread sets data.table to 1 thread and the thread variables to 1 in the R session that runs it", {
  withr::local_envvar(
    OMP_NUM_THREADS = "3",
    OPENBLAS_NUM_THREADS = NA,
    MKL_NUM_THREADS = NA
  )
  old <- data.table::setDTthreads(4L)
  on.exit(data.table::setDTthreads(old), add = TRUE)
  single_thread(thread_variables)
  expect_identical(data.table::getDTthreads(), 1L)
  expect_identical(unname(Sys.getenv(thread_variables)), c("1", "1", "1"))
})

test_that("with workers, each worker runs single_thread() and has 1 thread, the results do not depend on it, and with 1 worker nothing changes", {
  skip_if_pbn_not_installed()
  local_clean_state()
  withr::local_envvar(
    OMP_NUM_THREADS = "3",
    OPENBLAS_NUM_THREADS = NA,
    MKL_NUM_THREADS = NA
  )
  dt_here <- data.table::getDTthreads()
  calls <- withr::local_tempdir()
  real_single <- single_thread
  local_mocked_bindings(single_thread = function(vars) {
    writeLines("called", file.path(calls, Sys.getpid()))
    return(real_single(vars))
  })
  r <- run_plan(threads_plan(), workers = 2)
  got <- do.call(rbind, unname(r[grepl("__threads__", names(r))]))
  expect_true(all(got$pid != Sys.getpid()))
  expect_identical(unique(got$dt), 1L)
  expect_identical(unique(got$value), "1")
  expect_setequal(list.files(calls), as.character(unique(got$pid)))
  expect_length(list.files(calls), 2)
  # This R session keeps its own settings.
  expect_identical(data.table::getDTthreads(), dt_here)
  expect_identical(Sys.getenv("OMP_NUM_THREADS"), "3")
  expect_identical(Sys.getenv("MKL_NUM_THREADS", unset = "unset"), "unset")
  # With 1 worker, the analyses run here with the settings of the session.
  r1 <- run_plan(threads_plan())
  got1 <- do.call(rbind, unname(r1[grepl("__threads__", names(r1))]))
  expect_identical(unique(got1$dt), dt_here)
  expect_identical(got1$value[got1$var == "OMP_NUM_THREADS"][1], "3")
  expect_length(list.files(calls), 2)
  # On Linux, the environment that each worker started with holds the
  # thread variables, so a library that starts its threads at the load
  # of a package gets 1.
  skip_if_not(file.exists("/proc/self/environ"), "no /proc on this system")
  fn <- function(data, argset) {
    env <- readBin("/proc/self/environ", "raw", 1e6)
    lines <- strsplit(
      rawToChar(replace(env, env == as.raw(0), as.raw(10))),
      "\n"
    )[[1]]
    return(sort(grep("^(OMP|OPENBLAS|MKL)_NUM_THREADS=", lines, value = TRUE)))
  }
  environment(fn) <- globalenv()
  data_fn <- function(scenario, sim) return(data.frame(x = 1))
  environment(data_fn) <- globalenv()
  p <- plnr::Plan$new()
  add_data(p, "d", fn = data_fn)
  add_analyses(
    p,
    scenarios(list(scenarios = data.frame(scenario = "real", sims = 2))),
    "env",
    fn = fn
  )
  r <- run_plan(p, workers = 2)
  expect_identical(
    r[["real__sim_0001__env__1"]],
    c("MKL_NUM_THREADS=1", "OMP_NUM_THREADS=1", "OPENBLAS_NUM_THREADS=1")
  )
})
