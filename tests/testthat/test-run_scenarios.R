test_that("run_scenarios runs every scenario, stacks null and writes check.csv; only real/ holds the output part's file", {
  skip_if_pbn_not_installed()
  p <- local_project()
  out <- NULL
  printed <- utils::capture.output(
    out <- suppressMessages(run_scenarios("v01", home = p$home))
  )
  results <- file.path(p$home, "v01_results")
  files <- sort(list.files(results, recursive = TRUE))
  expect_identical(
    files,
    c(
      "known/check.csv",
      "known/data/d.csv",
      "known/results.qs2",
      "null/check.csv",
      "null/results.qs2",
      "null/sim_0001/data/d.csv",
      "null/sim_0001/results.qs2",
      "null/sim_0002/data/d.csv",
      "null/sim_0002/results.qs2",
      "null/sim_0003/data/d.csv",
      "null/sim_0003/results.qs2",
      "real/output.txt",
      "real/results.qs2"
    )
  )
  # end_if_fake_scenario() ends each fake run after save_results(), so the
  # output part runs for the real data only.
  expect_identical(
    grep("output\\.txt$", files, value = TRUE),
    "real/output.txt"
  )

  stacked <- read_results("v01", scenario = "null", home = p$home)
  expect_identical(check_results(stacked)$status, rep("PASS", 6))
  expect_identical(stacked$estimates$sim, 1:3)
  # Each stacked table keeps the class that the run saved: table1() gives a
  # data.table, and the run script gives estimates as a data.frame.
  expect_identical(class(stacked$tables$table1), c("data.table", "data.frame"))
  expect_identical(class(stacked$estimates), "data.frame")
  expect_identical(names(stacked$estimates), c("sim", "term", "estimate"))
  expect_identical(names(stacked$exclusions)[1], "sim")
  expect_identical(unique(stacked$tables$table1$sim), 1:3)
  expect_identical(stacked$extra, list())
  sim2 <- read_results("v01", scenario = "null", sim = 2, home = p$home)
  expect_identical(
    stacked$estimates$estimate[2],
    sim2$estimates$estimate
  )
  sim1 <- read_results("v01", scenario = "null", sim = 1, home = p$home)
  m <- stacked$manifest
  expect_identical(m$sim, NA_integer_)
  expect_identical(m$sims, 3L)
  expect_identical(m$n_rows_data, NA_integer_)
  expect_true(m$stacked)
  expect_identical(m$scenario, "null")
  expect_false("data_md5" %in% names(m))
  expect_match(sim1$manifest$fake_md5, "^[0-9a-f]{32}$")
  expect_identical(m$fake_md5, sim1$manifest$fake_md5)

  expect_identical(
    names(out),
    c("scenario", "assertion", "expected", "observed", "pass")
  )
  expect_identical(out$scenario, c("known", "known", "null", "null"))
  expect_true(all(out$pass))
  csv <- utils::read.csv(file.path(results, "null", "check.csv"))
  expect_identical(names(csv), c("assertion", "expected", "observed", "pass"))
  expect_identical(csv$pass, c(TRUE, TRUE))
  expect_true(any(grepl(
    "Assertions of the scenario \"null\"",
    printed,
    fixed = TRUE
  )))
  expect_identical(failed_rules(project_findings(p$home)), character(0))
})

test_that("stack_scenario stops when a simulation file is missing", {
  skip_if_pbn_not_installed()
  p <- local_project()
  suppressMessages(utils::capture.output(run_scenarios(
    "v01",
    scenario = "null",
    home = p$home
  )))
  stacked_file <- file.path(p$home, "v01_results", "null", "results.qs2")
  md5_before <- md5_of(stacked_file)
  unlink(file.path(p$home, "v01_results", "null", "sim_0002", "results.qs2"))
  expect_error(
    stack_scenario(normalizePath(p$home), "v01", "null", 3L),
    "Missing: sim_0002/results.qs2"
  )
  expect_identical(md5_of(stacked_file), md5_before)

  # A file of a simulation above sims is an error too.
  dir.create(file.path(p$home, "v01_results", "null", "sim_0004"))
  file.copy(
    file.path(p$home, "v01_results", "null", "sim_0001", "results.qs2"),
    file.path(p$home, "v01_results", "null", "sim_0004", "results.qs2")
  )
  expect_error(
    stack_scenario(normalizePath(p$home), "v01", "null", 3L),
    "Not in 1 to 3: sim_0004/results.qs2"
  )

  # Every sim_<digits> folder counts, not only four digits.
  null <- file.path(p$home, "v01_results", "null")
  unlink(file.path(null, "sim_0004"), recursive = TRUE)
  file.copy(
    file.path(null, "sim_0001", "results.qs2"),
    file.path(null, "sim_0002", "results.qs2")
  )
  dir.create(file.path(null, "sim_4"))
  file.copy(
    file.path(null, "sim_0001", "results.qs2"),
    file.path(null, "sim_4", "results.qs2")
  )
  expect_error(
    stack_scenario(normalizePath(p$home), "v01", "null", 3L),
    "Missing: none. Not in 1 to 3: sim_4/results.qs2",
    fixed = TRUE
  )
})

# A run script that quits with status 0 before save_results() for one
# scenario and simulation.
quit_before_save <- function(p, scenario, sim) {
  lines <- run_script_lines("v01", p$data_default)
  lines <- append(
    lines,
    paste0(
      "if (p$scenario == \"",
      scenario,
      "\" && p$sim == ",
      sim,
      ") quit(save = \"no\", status = 0)"
    ),
    after = init_line(lines)
  )
  writeLines(lines, file.path(p$home, "v01_run.R"))
  return(invisible(NULL))
}

test_that("a run that writes no results file stops run_scenarios, and no older file is read", {
  skip_if_pbn_not_installed()
  p <- local_project()
  suppressMessages(utils::capture.output(run_scenarios("v01", home = p$home)))
  results <- file.path(p$home, "v01_results")
  real_before <- file_states(file.path(results, "real"))
  known_before <- file_states(file.path(results, "known"))

  quit_before_save(p, "null", 2)
  err <- tryCatch(
    suppressMessages(run_scenarios("v01", scenario = "null", home = p$home)),
    error = function(e) return(e)
  )
  msg <- if (inherits(err, "error")) conditionMessage(err) else ""
  expect_match(
    msg,
    "The run of scenario \"null\", simulation 2, stopped with status 0 but wrote no results file",
    fixed = TRUE
  )
  expect_match(msg, "sim_0002/results.qs2", fixed = TRUE)
  # The older files of null are gone, so nothing can read them.
  expect_false(file.exists(file.path(
    results,
    "null",
    "sim_0002",
    "results.qs2"
  )))
  expect_false(file.exists(file.path(results, "null", "results.qs2")))
  expect_false(file.exists(file.path(results, "null", "check.csv")))
  expect_true(file.exists(file.path(
    results,
    "null",
    "sim_0001",
    "results.qs2"
  )))

  # A scenario with 1 simulation.
  quit_before_save(p, "known", 1)
  err <- tryCatch(
    suppressMessages(run_scenarios("v01", scenario = "known", home = p$home)),
    error = function(e) return(e)
  )
  msg <- if (inherits(err, "error")) conditionMessage(err) else ""
  expect_match(
    msg,
    "The run of scenario \"known\", simulation 1, stopped with status 0 but wrote no results file",
    fixed = TRUE
  )
  expect_false(file.exists(file.path(results, "known", "results.qs2")))
  expect_false(file.exists(file.path(results, "known", "check.csv")))
  expect_identical(file_states(file.path(results, "real")), real_before)
  expect_false(identical(
    file_states(file.path(results, "known")),
    known_before
  ))
})

test_that("run_scenarios stops before any run when a fake scenario has no expect_ function", {
  skip_if_pbn_not_installed()
  p <- local_project()
  unlink(file.path(p$home, "v01_functions", "expect_null.R"))
  expect_error(
    run_scenarios("v01", home = p$home),
    "These are missing: expect_null(). pbn ran no scenario.",
    fixed = TRUE
  )
  expect_error(
    run_scenarios("v01", scenario = c("real", "null"), home = p$home),
    "expect_null()",
    fixed = TRUE
  )
  expect_false(dir.exists(file.path(p$home, "v01_results")))

  # Only the chosen scenarios need one, and expect_real() is optional.
  checks <- NULL
  suppressMessages(utils::capture.output(
    checks <- run_scenarios("v01", scenario = c("real", "known"), home = p$home)
  ))
  expect_identical(unique(checks$scenario), "known")
})

test_that("an expect_ function that errors, returns zero rows or a wrong shape is a failed assertion", {
  skip_if_pbn_not_installed()
  bodies <- list(
    error = list(
      body = "stop(\"planted expect failure\")",
      assertion = "expect_known() runs without an error",
      observed = "planted expect failure"
    ),
    zero_rows = list(
      body = paste0(
        "data.frame(assertion = character(0), expected = character(0), ",
        "observed = character(0), pass = logical(0))"
      ),
      assertion = "expect_known() returns at least one assertion",
      observed = "0 rows"
    ),
    not_a_data_frame = list(
      body = "list(pass = TRUE)",
      assertion = "expect_known() returns a data.frame with the columns assertion, expected, observed, pass",
      observed = "an object of class list"
    ),
    wrong_columns = list(
      body = "data.frame(assertion = \"a\", pass = TRUE)",
      assertion = "expect_known() returns a data.frame with the columns assertion, expected, observed, pass",
      observed = "the columns assertion, pass"
    )
  )
  for (nm in names(bodies)) {
    b <- bodies[[nm]]
    p <- local_project()
    write_lines(
      p$home,
      "v01_functions/expect_known.R",
      c("expect_known <- function(results) {", b$body, "}")
    )
    err <- NULL
    utils::capture.output(
      err <- tryCatch(
        suppressMessages(run_scenarios("v01", home = p$home)),
        error = function(e) return(e)
      )
    )
    msg <- if (inherits(err, "error")) conditionMessage(err) else ""
    expect_match(msg, "These assertions failed:", fixed = TRUE, info = nm)
    expect_match(msg, paste0("known: ", b$assertion), fixed = TRUE, info = nm)
    expect_match(msg, b$observed, fixed = TRUE, info = nm)
    expect_no_match(msg, "null:", fixed = TRUE)
    results <- file.path(p$home, "v01_results")
    csv <- utils::read.csv(file.path(results, "known", "check.csv"))
    expect_identical(csv$pass, FALSE, info = nm)
    expect_identical(csv$assertion, b$assertion, info = nm)
    # null was still checked after known failed.
    expect_true(file.exists(file.path(results, "null", "check.csv")), info = nm)
    expect_identical(
      failed_rules(project_findings(p$home)),
      "scenario_checks",
      info = nm
    )
  }
})

test_that("a failing assertion stops run_scenarios after every scenario ran", {
  skip_if_pbn_not_installed()
  p <- local_project()
  write_lines(
    p$home,
    "v01_functions/expect_known.R",
    expect_known_lines(events_a = 5)
  )
  err <- NULL
  printed <- utils::capture.output(
    err <- tryCatch(
      suppressMessages(run_scenarios("v01", home = p$home)),
      error = function(e) return(e)
    )
  )
  expect_s3_class(err, "error")
  msg <- if (inherits(err, "error")) conditionMessage(err) else ""
  expect_match(msg, "These assertions failed:", fixed = TRUE)
  expect_match(
    msg,
    "known: events of x in group a (expected 5, observed 4)",
    fixed = TRUE
  )
  expect_no_match(msg, "null:", fixed = TRUE)
  # Every scenario ran, and null was stacked and checked after known failed.
  results <- file.path(p$home, "v01_results")
  expect_true(all(file.exists(file.path(
    results,
    c(
      "real/results.qs2",
      "known/check.csv",
      "null/results.qs2",
      "null/check.csv"
    )
  ))))

  found <- project_findings(p$home)
  expect_identical(failed_rules(found), "scenario_checks")
  expect_identical(
    found$path[found$status == "FAIL"],
    "v01_results/known/check.csv"
  )
  expect_match(
    found$detail[found$status == "FAIL"],
    "these assertions failed: events of x in group a",
    fixed = TRUE
  )
})

test_that("run_scenarios with one scenario does not touch the others", {
  skip_if_pbn_not_installed()
  p <- local_project()
  suppressMessages(utils::capture.output(run_scenarios("v01", home = p$home)))
  results <- file.path(p$home, "v01_results")
  real_before <- file_states(file.path(results, "real"))
  null_before <- file_states(file.path(results, "null"))
  known_before <- file_states(file.path(results, "known"))
  # A second has to pass, or a rewrite in the same second keeps the mtime.
  Sys.sleep(1.1)

  out <- suppressMessages(utils::capture.output(
    checks <- run_scenarios("v01", scenario = "known", home = p$home)
  ))
  expect_identical(unique(checks$scenario), "known")
  expect_identical(file_states(file.path(results, "real")), real_before)
  expect_identical(file_states(file.path(results, "null")), null_before)
  known_after <- file_states(file.path(results, "known"))
  expect_identical(known_after$file, known_before$file)
  expect_true(all(known_after$mtime > known_before$mtime))
})

test_that("run_scenarios stops on an unknown scenario before it runs anything", {
  skip_if_pbn_not_installed()
  p <- local_project()
  expect_error(
    run_scenarios("v01", scenario = c("known", "absent"), home = p$home),
    "These scenarios are not in CFG\\$scenarios: absent"
  )
  expect_false(dir.exists(file.path(p$home, "v01_results")))
})

test_that("a failing run stops run_scenarios with its output and names the scenario and simulation", {
  skip_if_pbn_not_installed()
  p <- local_project()
  lines <- run_script_lines("v01", p$data_default)
  lines <- append(
    lines,
    c(
      "cat(\"before the stop\\n\")",
      "if (p$scenario == \"null\" && p$sim == 2) stop(\"planted failure\")"
    ),
    after = init_line(lines)
  )
  writeLines(lines, file.path(p$home, "v01_run.R"))
  err <- tryCatch(
    suppressMessages(run_scenarios("v01", scenario = "null", home = p$home)),
    error = function(e) return(e)
  )
  msg <- if (inherits(err, "error")) conditionMessage(err) else ""
  expect_match(msg, "The run of scenario \"null\", simulation 2,", fixed = TRUE)
  expect_match(msg, "planted failure", fixed = TRUE)
  expect_match(msg, "before the stop", fixed = TRUE)
})

test_that("read_project_functions does not change the session of the caller", {
  skip_if_pbn_not_installed()
  p <- local_project()
  org_state <- function() {
    x <- as.list(org::project, all.names = TRUE)
    return(x[sort(names(x))])
  }
  org_before <- org_state()
  expect_false(exists("CFG", envir = globalenv(), inherits = FALSE))
  info <- read_project_functions(normalizePath(p$home), "v01")
  expect_identical(info$scenarios$scenario, c("real", "known", "null"))
  expect_identical(info$scenarios$sims, c(1L, 1L, 3L))
  expect_setequal(info$expect, c("known", "null"))
  expect_false(exists("CFG", envir = globalenv(), inherits = FALSE))
  expect_false(exists("fake_null", envir = globalenv(), inherits = FALSE))
  # org keeps the paths of the last project it started in org::project.
  expect_identical(org_state(), org_before)
})

test_that("an older results file that cannot be deleted stops the run", {
  p <- local_project()
  folder <- file.path(p$home, "v01_results", "known")
  # A folder in the place of the file: unlink() without recursive leaves it.
  dir.create(file.path(folder, "results.qs2"), recursive = TRUE)
  writeLines("x", file.path(folder, "results.qs2", "keep.txt"))
  expect_error(
    run_scenarios("v01", scenario = "known", home = p$home),
    "could not delete an older file"
  )
})

# Three simulation results objects of the fake scenario null, with the class
# `as_class` for each table.
three_runs <- function(as_class) {
  return(lapply(1:3, function(k) {
    x <- valid_results()
    x$manifest$scenario <- "null"
    x$manifest$data_md5 <- NULL
    x$manifest$fake_md5 <- "fedcba9876543210fedcba9876543210"
    x$manifest$sim <- k
    x$manifest$sims <- 3L
    x$exclusions <- as_class(x$exclusions)
    x$estimates <- as_class(data.frame(term = "a", estimate = k / 10))
    x$tables <- list(counts = as_class(x$tables$counts))
    return(x)
  }))
}

test_that("stack_results keeps the class of each table of simulation 1", {
  classes <- list(
    data.table = list(
      as_class = data.table::as.data.table,
      class = c("data.table", "data.frame")
    ),
    data.frame = list(as_class = as.data.frame, class = "data.frame"),
    subclass = list(
      as_class = function(x) {
        return(structure(as.data.frame(x), class = c("my_frame", "data.frame")))
      },
      class = c("my_frame", "data.frame")
    )
  )
  for (nm in names(classes)) {
    x <- stack_results(three_runs(classes[[nm]]$as_class), "null")
    for (part in list(x$estimates, x$exclusions, x$tables$counts)) {
      expect_identical(class(part), classes[[nm]]$class, info = nm)
      expect_identical(names(part)[1], "sim", info = nm)
    }
    expect_identical(x$estimates$sim, 1:3, info = nm)
    expect_identical(x$estimates$estimate, c(0.1, 0.2, 0.3), info = nm)
    expect_identical(x$tables$counts$sim, rep(1:3, each = 2), info = nm)
    expect_identical(check_results(x)$status, rep("PASS", 6), info = nm)
  }
})

test_that("stack_results stops when the simulations have different fake_md5", {
  runs <- three_runs(as.data.frame)
  expect_identical(
    stack_results(runs, "null")$manifest$fake_md5,
    "fedcba9876543210fedcba9876543210"
  )
  runs[[3]]$manifest$fake_md5 <- "0123456789abcdef0123456789abcdef"
  expect_error(
    stack_results(runs, "null"),
    "The simulations of the scenario \"null\" have different values of manifest$fake_md5",
    fixed = TRUE
  )
})

test_that("stack_results stops when a simulation of a fake scenario has no valid fake_md5", {
  for (bad in list(NULL, NA_character_, "x", 1)) {
    runs <- three_runs(as.data.frame)
    runs[[2]]$manifest["fake_md5"] <- list(bad)
    expect_error(
      stack_results(runs, "null"),
      paste0(
        "The simulations 2 of the scenario \"null\" have no valid ",
        "manifest$fake_md5, so pbn cannot stack them."
      ),
      fixed = TRUE,
      info = paste(format(bad), collapse = "")
    )
  }
  # A real scenario with simulations has data_md5 and no fake_md5.
  runs <- lapply(1:2, function(k) {
    x <- valid_results()
    x$manifest$sim <- k
    x$manifest$sims <- 2L
    return(x)
  })
  x <- stack_results(runs, "real")
  expect_identical(x$manifest$data_md5, runs[[1]]$manifest$data_md5)
  expect_false("fake_md5" %in% names(x$manifest))
})

test_that("run_scenarios run twice writes the same bytes when fake_<scenario>() stamps the time into a file", {
  skip_if_pbn_not_installed()
  p <- local_project()
  write_lines(p$home, "v01_functions/fake_null.R", fake_with_time_lines("null"))
  write_lines(
    p$home,
    "v01_functions/fake_known.R",
    append(
      fake_known_lines(),
      "  writeLines(format(Sys.time(), \"%OS6\"), file.path(dir, \"header.txt\"))",
      after = 1
    )
  )
  results <- file.path(p$home, "v01_results")
  outputs <- function() {
    files <- sort(list.files(results, recursive = TRUE))
    files <- files[grepl("(results\\.qs2|check\\.csv)$", files)]
    return(stats::setNames(md5_of(file.path(results, files)), files))
  }
  headers <- function() {
    files <- sort(list.files(results, "^header\\.txt$", recursive = TRUE))
    return(stats::setNames(md5_of(file.path(results, files)), files))
  }
  suppressMessages(utils::capture.output(run_scenarios("v01", home = p$home)))
  first <- outputs()
  first_headers <- headers()
  suppressMessages(utils::capture.output(run_scenarios("v01", home = p$home)))

  # The planted time stamps differ, so the data files differ.
  expect_identical(
    names(first_headers),
    c(
      "known/data/header.txt",
      paste0("null/sim_000", 1:3, "/data/header.txt")
    )
  )
  expect_identical(names(headers()), names(first_headers))
  expect_true(all(headers() != first_headers))
  # The stacked file, every simulation file and every check.csv are the same.
  expect_identical(
    names(first),
    c(
      "known/check.csv",
      "known/results.qs2",
      "null/check.csv",
      "null/results.qs2",
      paste0("null/sim_000", 1:3, "/results.qs2"),
      "real/results.qs2"
    )
  )
  expect_identical(outputs(), first)
})
