# A fake project with the scenarios real (1 simulation) and pool (4).
# The setting `pause` is the number of seconds that each run of pool sleeps.
# Each run appends "<scenario> <sim>" to runs.txt, records the time of its
# start and of its end in `extra`, and stops when its simulation is in
# fail.txt. A line "<scenario> <sim> <mode>" in plant.txt plants a fault in
# that run:
# - empty: quit with status 0 before save_results().
# - bad: write a results.qs2 that fails check_results(), and quit with
#   status 0.
# - late: stop with an error after save_results().
# - hang: after save_results(), write the process id to child.pid and sleep
#   60 seconds.
local_pool_project <- function(pause = 0, env = parent.frame()) {
  root <- tempfile("pbnpool")
  dir.create(root)
  withr::defer(unlink(root, recursive = TRUE), envir = env)
  home <- file.path(root, "project")
  data <- normalizePath(
    write_data(
      file.path(root, "data"),
      x = c(1, 0, 1, 0, 1, 0, 1, 0, 1, 0),
      y = 1:10
    ),
    winslash = "/"
  )
  write_lines(
    home,
    "v01_functions/_cfg.R",
    c(
      "CFG <- list(",
      "  scenarios = data.frame(",
      "    scenario = c(\"real\", \"pool\"),",
      "    sims = c(1, 4),",
      paste0("    pause = c(0, ", pause, ")"),
      "  )",
      ")"
    )
  )
  write_lines(
    home,
    "v01_functions/fake_pool.R",
    c(
      "fake_pool <- function(dir) {",
      "  d <- data.frame(x = stats::rnorm(10))",
      "  utils::write.csv(d, file.path(dir, \"d.csv\"), row.names = FALSE)",
      "}"
    )
  )
  write_lines(
    home,
    "v01_functions/expect_pool.R",
    c(
      "expect_pool <- function(results) {",
      "  n <- length(unique(results$estimates$sim))",
      "  data.frame(assertion = \"simulations\", expected = 4, observed = n,",
      "    pass = n == 4)",
      "}"
    )
  )
  write_lines(home, "v01_functions/notes.R", "# Notes, version a.")
  write_lines(
    home,
    "v01_run.R",
    c(
      "started <- as.numeric(Sys.time())",
      paste0(
        "p <- pbn::initialize_project(version = \"v01\", home = \".\", ",
        "data_raw = \"",
        data,
        "\")"
      ),
      # One string in one call: two runs at the same time append to runs.txt,
      # and cat() with several arguments can interleave their pieces.
      "cat(paste0(p$scenario, \" \", p$sim, \"\\n\"), file = \"runs.txt\", append = TRUE)",
      "if (file.exists(\"fail.txt\") && p$sim %in% as.integer(readLines(\"fail.txt\"))) {",
      "  stop(\"planted failure in simulation \", p$sim)",
      "}",
      "plant <- if (file.exists(\"plant.txt\")) utils::read.table(\"plant.txt\") else NULL",
      "mode <- plant$V3[plant$V1 == p$scenario & plant$V2 == p$sim]",
      "mode <- if (length(mode) == 1) mode else \"\"",
      "if (mode == \"empty\") quit(save = \"no\", status = 0)",
      "if (mode == \"bad\") {",
      "  qs2::qs_save(list(a = 1), file.path(p$results, \"results.qs2\"))",
      "  quit(save = \"no\", status = 0)",
      "}",
      "d <- utils::read.csv(file.path(p$data_raw, \"d.csv\"))",
      "Sys.sleep(p$settings$pause)",
      "ended <- as.numeric(Sys.time())",
      "pbn::save_results(",
      "  exclusions = data.frame(step = character(0), n_before = numeric(0),",
      "    n_excluded = numeric(0), n_after = numeric(0)),",
      "  estimates = data.frame(term = \"mean_x\", estimate = mean(d$x)),",
      "  tables = list(),",
      "  extra = list(started = started, ended = ended)",
      ")",
      "if (mode == \"late\") stop(\"planted failure after save_results\")",
      "if (mode == \"hang\") {",
      "  writeLines(as.character(Sys.getpid()), \"child.pid\")",
      "  Sys.sleep(60)",
      "}"
    )
  )
  return(list(home = home, root = root))
}

pool_folder <- function(p, sim = NULL) {
  folder <- file.path(p$home, "v01_results", "pool")
  if (is.null(sim)) {
    return(folder)
  }
  return(file.path(folder, sprintf("sim_%04d", sim)))
}

# The time of the start and of the end of each simulation of pool.
pool_times <- function(p) {
  x <- lapply(1:4, function(k) {
    return(qs2::qs_read(file.path(pool_folder(p, k), "results.qs2"))$extra)
  })
  return(data.frame(
    sim = 1:4,
    started = vapply(x, function(e) return(e$started), numeric(1)),
    ended = vapply(x, function(e) return(e$ended), numeric(1))
  ))
}

# The longest time in seconds that two simulations ran at the same time.
# Negative when no two runs overlap.
max_overlap <- function(times) {
  best <- -Inf
  for (i in seq_len(nrow(times))) {
    for (j in seq_len(nrow(times))[-i]) {
      both <- min(times$ended[c(i, j)]) - max(times$started[c(i, j)])
      best <- max(best, both)
    }
  }
  return(best)
}

pool_outputs <- function(p) {
  files <- file.path(pool_folder(p), c("results.qs2", "check.csv"))
  return(stats::setNames(md5_of(files), basename(files)))
}

runs_of <- function(p) {
  file <- file.path(p$home, "runs.txt")
  if (!file.exists(file)) {
    return(character(0))
  }
  return(readLines(file))
}

# run_scenarios() with its console output and its messages, and the error
# when it stops.
run_capture <- function(...) {
  messages <- character(0)
  err <- NULL
  printed <- utils::capture.output(
    err <- tryCatch(
      withCallingHandlers(
        {
          run_scenarios(...)
          NULL
        },
        message = function(m) {
          messages <<- c(messages, conditionMessage(m))
          invokeRestart("muffleMessage")
        }
      ),
      error = function(e) return(e)
    )
  )
  return(list(
    printed = paste(printed, collapse = "\n"),
    messages = trimws(messages),
    error = if (inherits(err, "error")) conditionMessage(err) else NULL
  ))
}

test_that("workers and resume must be valid, and a bad value runs nothing", {
  p <- local_pool_project()
  for (bad in list(0, 1.5, NA, -1, "2", c(1, 2), NULL, Inf)) {
    expect_error(
      run_scenarios("v01", home = p$home, workers = bad),
      "`workers` must be one whole number of 1 or more.",
      fixed = TRUE,
      info = paste(format(bad), collapse = " ")
    )
  }
  for (bad in list(NA, "yes", c(TRUE, TRUE), 1)) {
    expect_error(
      run_scenarios("v01", home = p$home, resume = bad),
      "`resume` must be TRUE or FALSE.",
      fixed = TRUE
    )
  }
  expect_false(dir.exists(file.path(p$home, "v01_results")))
})

test_that("with workers = 2 two runs overlap in time, with workers = 1 none do, and the stacked results and check.csv are byte-identical", {
  skip_if_pbn_not_installed()
  # Each run of pool sleeps 2 seconds. Two runs that start together thus
  # overlap by about 2 seconds, and the test asks for 1 second.
  p <- local_pool_project(pause = 2)
  one <- run_capture("v01", scenario = "pool", home = p$home, workers = 1)
  expect_null(one$error)
  times_one <- pool_times(p)
  out_one <- pool_outputs(p)
  two <- run_capture("v01", scenario = "pool", home = p$home, workers = 2)
  expect_null(two$error)
  times_two <- pool_times(p)
  out_two <- pool_outputs(p)

  # workers = 1: each run starts after the run before it ended.
  ordered <- times_one[order(times_one$started), ]
  expect_true(all(ordered$started[-1] > ordered$ended[-4]))
  expect_lt(max_overlap(times_one), 0)
  # workers = 2: two runs overlap by 1 second or more.
  expect_gte(max_overlap(times_two), 1)
  # The second run ran each simulation again.
  expect_true(all(times_two$started > max(times_one$ended)))

  expect_identical(out_two, out_one)
  stacked <- read_results("v01", scenario = "pool", home = p$home)
  expect_identical(stacked$estimates$sim, 1:4)
  expect_identical(stacked$manifest$settings, list(pause = 2))
  expect_identical(
    sort(one$messages[grepl(": (started|ran)$", one$messages)]),
    sort(two$messages[grepl(": (started|ran)$", two$messages)])
  )
})

test_that("a failed run among four with workers = 2: the started runs end, the error names it, and pool is not stacked or checked", {
  skip_if_pbn_not_installed()
  # Each run of pool sleeps 3 seconds, so simulation 3 still runs when
  # simulation 4 fails, about 1.5 seconds after its start.
  p <- local_pool_project(pause = 3)
  # A full run first, so a stacked file and a check.csv exist.
  expect_null(run_capture("v01", home = p$home, workers = 2)$error)
  expect_true(all(file.exists(file.path(
    pool_folder(p),
    c("results.qs2", "check.csv")
  ))))
  unlink(file.path(p$home, "runs.txt"))

  writeLines("4", file.path(p$home, "fail.txt"))
  res <- run_capture("v01", home = p$home, workers = 2)
  log4 <- file.path(
    normalizePath(pool_folder(p), winslash = "/"),
    "sim_0004",
    "run.log"
  )
  expect_match(
    res$error,
    paste0(
      "The run of scenario \"pool\", simulation 4, stopped with status 1. ",
      "Its log: ",
      log4
    ),
    fixed = TRUE
  )
  expect_no_match(res$error, "simulation [123],")
  expect_match(res$printed, "planted failure in simulation 4", fixed = TRUE)
  # The other three simulations ran to their end.
  expect_setequal(runs_of(p), c("real 1", paste("pool", 1:4)))
  for (k in 1:3) {
    expect_true(
      file.exists(file.path(pool_folder(p, k), "results.qs2")),
      info = paste("simulation", k)
    )
  }
  expect_false(file.exists(file.path(pool_folder(p, 4), "results.qs2")))
  expect_false(file.exists(file.path(pool_folder(p), "results.qs2")))
  expect_false(file.exists(file.path(pool_folder(p), "check.csv")))

  # Two runs that fail together: both are in the error, and no new run
  # starts after them.
  unlink(file.path(p$home, "runs.txt"))
  writeLines(c("1", "2"), file.path(p$home, "fail.txt"))
  res <- run_capture("v01", scenario = "pool", home = p$home, workers = 2)
  for (k in 1:2) {
    expect_match(
      res$error,
      paste0(
        "The run of scenario \"pool\", simulation ",
        k,
        ", stopped with status 1. "
      ),
      fixed = TRUE,
      info = paste("simulation", k)
    )
    expect_match(
      res$printed,
      paste("planted failure in simulation", k),
      fixed = TRUE
    )
  }
  expect_setequal(runs_of(p), c("pool 1", "pool 2"))
  expect_false(any(grepl("simulation [34]: started", res$messages)))
})

test_that("resume = TRUE runs only the simulation without results, and the stacked file is byte-identical", {
  skip_if_pbn_not_installed()
  p <- local_pool_project()
  expect_null(run_capture("v01", home = p$home)$error)
  expect_identical(runs_of(p), c("real 1", paste("pool", 1:4)))
  full <- pool_outputs(p)
  logs <- file.path(pool_folder(p, 1:4), "run.log")
  mtimes <- file.mtime(logs)
  Sys.sleep(1.1)

  unlink(file.path(pool_folder(p, 3), "results.qs2"))
  res <- run_capture("v01", home = p$home, resume = TRUE)
  expect_null(res$error)
  expect_identical(runs_of(p), c("real 1", paste("pool", 1:4), "pool 3"))
  expect_identical(file.mtime(logs)[-3], mtimes[-3])
  expect_true(file.mtime(logs)[3] > mtimes[3])
  expect_identical(
    res$messages[grepl("^pbn: scenario", res$messages)],
    c(
      "pbn: scenario real, simulation 1: kept",
      "pbn: scenario pool, simulation 1: kept",
      "pbn: scenario pool, simulation 2: kept",
      "pbn: scenario pool, simulation 4: kept",
      "pbn: scenario pool, simulation 3: started, because there is no results.qs2",
      "pbn: scenario pool, simulation 3: ran"
    )
  )
  expect_identical(pool_outputs(p), full)

  # resume = FALSE runs every simulation, as before.
  unlink(file.path(p$home, "runs.txt"))
  expect_null(run_capture("v01", home = p$home)$error)
  expect_identical(runs_of(p), c("real 1", paste("pool", 1:4)))
})

test_that("resume = TRUE runs every simulation again after one byte of a functions file changed", {
  skip_if_pbn_not_installed()
  p <- local_pool_project()
  expect_null(run_capture("v01", home = p$home)$error)
  unlink(file.path(p$home, "runs.txt"))
  notes <- file.path(p$home, "v01_functions", "notes.R")
  md5_before <- md5_of(notes)
  writeLines("# Notes, version b.", notes)
  expect_false(identical(md5_of(notes), md5_before))
  expect_equal(file.size(notes), nchar("# Notes, version a.\n"))

  res <- run_capture("v01", home = p$home, resume = TRUE)
  expect_null(res$error)
  expect_identical(runs_of(p), c("real 1", paste("pool", 1:4)))
  expect_false(any(grepl(": kept$", res$messages)))
  expect_true(all(grepl(
    "because the run script or the functions folder changed",
    res$messages[grepl(": started", res$messages)],
    fixed = TRUE
  )))
  # The stored source_md5 is the md5 of the files now.
  m <- read_results("v01", scenario = "pool", sim = 1, home = p$home)$manifest
  expect_identical(m$source_md5[["v01_functions/notes.R"]], md5_of(notes))
})

test_that("resume = TRUE runs a simulation again when run.ok is gone or results.qs2 changed after the run", {
  skip_if_pbn_not_installed()
  p <- local_pool_project()
  expect_null(run_capture("v01", home = p$home)$error)
  full <- pool_outputs(p)
  # run.ok holds the md5 of results.qs2, one line.
  for (k in 1:4) {
    expect_identical(
      readLines(file.path(pool_folder(p, k), "run.ok")),
      md5_of(file.path(pool_folder(p, k), "results.qs2")),
      info = paste("simulation", k)
    )
  }
  unlink(file.path(p$home, "runs.txt"))
  # A run that did not complete, as after a kill: results.qs2 and no run.ok.
  unlink(file.path(pool_folder(p, 1), "run.ok"))
  # Two changes after the run: a corrupt file, and a valid file of another
  # simulation.
  writeBin(as.raw(1:64), file.path(pool_folder(p, 2), "results.qs2"))
  file.copy(
    file.path(pool_folder(p, 3), "results.qs2"),
    file.path(pool_folder(p, 4), "results.qs2"),
    overwrite = TRUE
  )
  expect_identical(
    check_results(qs2::qs_read(file.path(
      pool_folder(p, 4),
      "results.qs2"
    )))$status,
    rep("PASS", 7)
  )

  res <- run_capture("v01", home = p$home, resume = TRUE)
  expect_null(res$error)
  expect_identical(runs_of(p), c("pool 1", "pool 2", "pool 4"))
  started <- res$messages[grepl(": started", res$messages)]
  expect_identical(
    started,
    c(
      "pbn: scenario pool, simulation 1: started, because the earlier run did not complete",
      "pbn: scenario pool, simulation 2: started, because results.qs2 changed after the run",
      "pbn: scenario pool, simulation 4: started, because results.qs2 changed after the run"
    )
  )
  expect_identical(pool_outputs(p), full)
})

test_that("a run that stops with an error after save_results is a failed run without run.ok, and resume runs it again", {
  skip_if_pbn_not_installed()
  p <- local_pool_project()
  writeLines("pool 2 late", file.path(p$home, "plant.txt"))
  res <- run_capture("v01", scenario = "pool", home = p$home)
  expect_match(
    res$error,
    "The run of scenario \"pool\", simulation 2, stopped with status 1. ",
    fixed = TRUE
  )
  expect_match(res$printed, "planted failure after save_results", fixed = TRUE)
  # The run wrote a valid results.qs2 before it stopped, and no run.ok.
  file2 <- file.path(pool_folder(p, 2), "results.qs2")
  expect_true(file.exists(file2))
  expect_identical(check_results(qs2::qs_read(file2))$status, rep("PASS", 7))
  expect_false(file.exists(file.path(pool_folder(p, 2), "run.ok")))
  expect_true(file.exists(file.path(pool_folder(p, 1), "run.ok")))

  unlink(file.path(p$home, c("plant.txt", "runs.txt")))
  res <- run_capture("v01", scenario = "pool", home = p$home, resume = TRUE)
  expect_null(res$error)
  expect_identical(runs_of(p), paste("pool", 2:4))
  expect_true(
    "pbn: scenario pool, simulation 2: started, because the earlier run did not complete" %in%
      res$messages
  )
  expect_true("pbn: scenario pool, simulation 1: kept" %in% res$messages)
  expect_true(file.exists(file.path(pool_folder(p, 2), "run.ok")))
})

test_that("a run that quits with status 0 without results.qs2, or with a results.qs2 that fails check_results(), is a failed run", {
  skip_if_pbn_not_installed()
  p <- local_pool_project()
  # real has 1 simulation and no expect_real(), so only the check of the
  # pool can find the bad file.
  writeLines("real 1 bad", file.path(p$home, "plant.txt"))
  res <- run_capture("v01", scenario = "real", home = p$home)
  real <- normalizePath(
    file.path(p$home, "v01_results", "real"),
    winslash = "/"
  )
  expect_match(
    res$error,
    paste0(
      "The run of scenario \"real\", simulation 1, stopped with status 0, ",
      "but results.qs2 fails check_results(): top_level_names, ",
      "exclusions_table, manifest_scenario, manifest_data, manifest_source: ",
      file.path(real, "results.qs2"),
      ". Its log: ",
      file.path(real, "run.log")
    ),
    fixed = TRUE
  )
  expect_false(file.exists(file.path(real, "run.ok")))

  writeLines("pool 3 empty", file.path(p$home, "plant.txt"))
  res <- run_capture("v01", scenario = "pool", home = p$home)
  expect_match(
    res$error,
    "The run of scenario \"pool\", simulation 3, stopped with status 0 but wrote no results file",
    fixed = TRUE
  )
  expect_false(file.exists(file.path(pool_folder(p, 3), "run.ok")))
  expect_false(file.exists(file.path(pool_folder(p), "results.qs2")))
})

# Starts run_scenarios() in a background R session that uses the installed
# pbn, and waits until the run with the mode "hang" wrote child.pid.
start_hanging_run <- function(p, ...) {
  pid_file <- file.path(p$home, "child.pid")
  unlink(pid_file)
  bg <- callr::r_bg(
    function(home, ...) {
      return(tryCatch(
        {
          pbn::run_scenarios("v01", home = home, ...)
          "no error"
        },
        error = function(e) return(conditionMessage(e))
      ))
    },
    args = list(home = p$home, ...)
  )
  waited <- 0
  while (!file.exists(pid_file) && waited < 60 && bg$is_alive()) {
    Sys.sleep(0.1)
    waited <- waited + 0.1
  }
  testthat::expect_true(file.exists(pid_file))
  return(list(bg = bg, pid = as.integer(readLines(pid_file))))
}

# TRUE while the process `pid` is alive, for at most 10 seconds.
alive_after_wait <- function(pid) {
  waited <- 0
  while (tools::pskill(pid, 0L) && waited < 10) {
    Sys.sleep(0.1)
    waited <- waited + 0.1
  }
  return(tools::pskill(pid, 0L))
}

test_that("a child killed after save_results, or a caller interrupted, leaves no run.ok, and resume runs the simulation again", {
  skip_if_pbn_not_installed()
  skip_on_os("windows")
  p <- local_pool_project()
  writeLines("pool 2 hang", file.path(p$home, "plant.txt"))

  # The child is killed after it wrote results.qs2.
  run <- start_hanging_run(p, scenario = "pool")
  withr::defer(tools::pskill(run$pid))
  expect_true(file.exists(file.path(pool_folder(p, 2), "results.qs2")))
  tools::pskill(run$pid, tools::SIGKILL)
  run$bg$wait(30000)
  expect_match(
    run$bg$get_result(),
    "The run of scenario \"pool\", simulation 2, stopped with status -9. ",
    fixed = TRUE
  )
  expect_false(file.exists(file.path(pool_folder(p, 2), "run.ok")))
  unlink(file.path(p$home, c("plant.txt", "runs.txt")))
  res <- run_capture("v01", scenario = "pool", home = p$home, resume = TRUE)
  expect_null(res$error)
  expect_identical(runs_of(p), paste("pool", 2:4))
  expect_true(
    "pbn: scenario pool, simulation 2: started, because the earlier run did not complete" %in%
      res$messages
  )

  # The caller is interrupted while the child sleeps after save_results().
  writeLines("pool 2 hang", file.path(p$home, "plant.txt"))
  run <- start_hanging_run(p, scenario = "pool")
  withr::defer(tools::pskill(run$pid))
  run$bg$interrupt()
  run$bg$wait(30000)
  expect_false(run$bg$is_alive())
  # run_scenarios() kills the child when it ends.
  expect_false(alive_after_wait(run$pid))
  expect_true(file.exists(file.path(pool_folder(p, 2), "results.qs2")))
  expect_false(file.exists(file.path(pool_folder(p, 2), "run.ok")))
  unlink(file.path(p$home, c("plant.txt", "runs.txt")))
  res <- run_capture("v01", scenario = "pool", home = p$home, resume = TRUE)
  expect_null(res$error)
  expect_identical(runs_of(p), paste("pool", 2:4))
  expect_true(
    "pbn: scenario pool, simulation 2: started, because the earlier run did not complete" %in%
      res$messages
  )
})

test_that("keep_reason keeps only a valid file of the same scenario, simulation and code", {
  folder <- withr::local_tempdir()
  code <- c(v01_run.R = "0123456789abcdef0123456789abcdef")
  job <- data.frame(
    scenario = "null",
    sim = 2L,
    sims = 3L,
    file = file.path(folder, "results.qs2"),
    ok = file.path(folder, "run.ok")
  )
  # The pool writes run.ok after a run that completed.
  complete <- function() {
    writeLines(md5_of(job$file), job$ok)
    return(invisible(NULL))
  }
  x <- valid_results()
  x$manifest$scenario <- "null"
  x$manifest$sim <- 2L
  x$manifest$sims <- 3L
  x$manifest$data_md5 <- NULL
  x$manifest$fake_md5 <- "fedcba9876543210fedcba9876543210"
  x$manifest$source_md5 <- code
  fake <- "fedcba9876543210fedcba9876543210"
  expect_identical(keep_reason(job, code, fake), "there is no results.qs2")
  qs2::qs_save(x, job$file)
  expect_identical(
    keep_reason(job, code, fake),
    "the earlier run did not complete"
  )
  writeLines("00000000000000000000000000000000", job$ok)
  expect_identical(
    keep_reason(job, code, fake),
    "results.qs2 changed after the run"
  )
  complete()
  expect_identical(keep_reason(job, code, fake), NA_character_)

  other <- code
  other[[1]] <- "00000000000000000000000000000000"
  expect_identical(
    keep_reason(job, other, fake),
    "the run script or the functions folder changed"
  )
  expect_identical(
    keep_reason(job, code, "00000000000000000000000000000000"),
    "fake_null() changed"
  )
  for (el in c("scenario", "sim", "sims")) {
    y <- x
    y$manifest[[el]] <- if (el == "scenario") "known" else 1L
    qs2::qs_save(y, job$file)
    complete()
    expect_identical(
      keep_reason(job, code, fake),
      "the manifest names another scenario or simulation",
      info = el
    )
  }
  y <- x
  y$manifest$source_md5 <- NULL
  qs2::qs_save(y, job$file)
  complete()
  expect_identical(
    keep_reason(job, code, fake),
    "results.qs2 fails check_results(): manifest_source"
  )
})
