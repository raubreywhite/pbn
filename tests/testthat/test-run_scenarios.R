test_that("run_scenarios runs every scenario, stacks null and writes check.csv", {
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
      "real/results.qs2"
    )
  )

  stacked <- read_results("v01", scenario = "null", home = p$home)
  expect_identical(check_results(stacked)$status, rep("PASS", 5))
  expect_identical(stacked$estimates$sim, 1:3)
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
  expect_identical(m$data_md5, sim1$manifest$data_md5)

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
  lines <- append(
    run_script_lines("v01", p$data_default),
    paste0(
      "if (p$scenario == \"",
      scenario,
      "\" && p$sim == ",
      sim,
      ") quit(save = \"no\", status = 0)"
    ),
    after = 1
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

test_that("run_scenarios stops before any run when a made-up scenario has no expect_ function", {
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
  lines <- append(
    run_script_lines("v01", p$data_default),
    c(
      "cat(\"before the stop\\n\")",
      "if (p$scenario == \"null\" && p$sim == 2) stop(\"planted failure\")"
    ),
    after = 1
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
