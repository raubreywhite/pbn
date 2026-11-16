# Extracted from test-run_scenarios.R:265

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "pbn", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
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

# test -------------------------------------------------------------------------
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
