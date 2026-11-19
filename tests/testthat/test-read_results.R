# read_results() for "real" and manifest$failed_fake_checks, the record of
# the fake scenarios of its run whose assertions did not pass.

# The error of read_results() for the real results of the project `p`, when
# their manifest records the failed fake scenarios `failed`.
real_refusal <- function(p, failed) {
  root <- file.path(p$home, "v01_results")
  file <- file.path(root, "real", "results.qs2")
  one <- length(failed) == 1
  return(paste0(
    "manifest$failed_fake_checks of ",
    file,
    " names ",
    if (one) "a fake scenario" else paste(length(failed), "fake scenarios"),
    " with a failed assertion: ",
    paste0("\"", failed, "\"", collapse = ", "),
    ". ",
    if (one) "Its" else "Their",
    " assertions are in ",
    file.path(root, if (one) failed else "<scenario>", "check.csv"),
    ". Correct the analysis, and run the scenario \"real\" again with the ",
    "fake scenarios. To look at the results for debugging, read them with ",
    "qs2::qs_read(\"",
    file,
    "\")."
  ))
}

# Makes expect_null() fail in the project `p`, or pass again with
# `fail = FALSE`.
fail_null <- function(p, fail = TRUE) {
  lines <- expect_null_lines()
  if (fail) {
    lines <- sub("abs(b) < 1", "abs(b) > 100", lines, fixed = TRUE)
  }
  write_lines(p$home, "v01_functions/expect_null.R", lines)
  return(invisible(NULL))
}

real_manifest <- function(p) {
  file <- file.path(p$home, "v01_results", "real", "results.qs2")
  return(qs2::qs_read(file)$manifest)
}

saved <- function(run) {
  return(!inherits(run$err, "error"))
}

test_that("read_results refuses the real results of a run whose fake scenario failed, and names the scenarios, their check.csv and the fix", {
  p <- local_project()
  fail_null(p)
  run <- save_run(p)
  expect_match(
    conditionMessage(run$err),
    "in the scenarios null.",
    fixed = TRUE
  )
  expect_identical(real_manifest(p)$failed_fake_checks, "null")
  expect_error(
    read_results("v01", home = p$home),
    real_refusal(p, "null"),
    fixed = TRUE
  )
  # Also for one simulation of real.
  expect_error(
    read_results("v01", sim = 1, home = p$home),
    real_refusal(p, "null"),
    fixed = TRUE
  )
  # Two failed scenarios, in the order of CFG$scenarios.
  write_lines(p$home, "v01_functions/expect_known.R", expect_known_lines(5))
  run <- save_run(p)
  expect_identical(real_manifest(p)$failed_fake_checks, c("known", "null"))
  expect_error(
    read_results("v01", home = p$home),
    real_refusal(p, c("known", "null")),
    fixed = TRUE
  )
})

test_that("read_results reads the real results of a run whose fake scenarios all passed, or of a run of real alone", {
  p <- local_project()
  expect_true(saved(save_run(p)))
  expect_identical(real_manifest(p)$failed_fake_checks, character(0))
  expect_identical(names(read_results("v01", home = p$home))[1], "manifest")

  q <- local_project()
  expect_true(saved(save_run(q, only = "real")))
  expect_identical(real_manifest(q)$failed_fake_checks, character(0))
  expect_identical(names(read_results("v01", home = q$home))[1], "manifest")
})

test_that("a run of real alone on the same day with the same code reads, after a run whose fake scenario failed", {
  p <- local_project()
  fail_null(p)
  save_run(p)
  expect_error(read_results("v01", home = p$home), "failed_fake_checks of")
  expect_true(saved(save_run(p, only = "real")))
  expect_identical(real_manifest(p)$failed_fake_checks, character(0))
  expect_identical(names(read_results("v01", home = p$home))[1], "manifest")
  # null itself still fails.
  expect_error(read_results("v01", "null", home = p$home), "did not pass")
})

test_that("a fixed fake scenario run alone leaves the old real results refused, until real runs again", {
  p <- local_project()
  fail_null(p)
  save_run(p)
  fail_null(p, fail = FALSE)
  expect_true(saved(save_run(p, only = "null")))
  # null passes now, and its check.csv says so.
  expect_identical(
    names(read_results("v01", "null", home = p$home))[1],
    "manifest"
  )
  expect_error(
    read_results("v01", home = p$home),
    real_refusal(p, "null"),
    fixed = TRUE
  )
  expect_true(saved(save_run(p, only = c("real", "null"))))
  expect_identical(names(read_results("v01", home = p$home))[1], "manifest")
})

test_that("real results without failed_fake_checks, as pbn 2026.11.24 wrote them, read as before", {
  p <- local_project()
  fail_null(p)
  save_run(p)
  file <- file.path(p$home, "v01_results", "real", "results.qs2")
  old <- qs2::qs_read(file)
  old$manifest$failed_fake_checks <- NULL
  old$manifest$pbn_version <- "2026.11.24"
  qs2::qs_save(old, file)
  expect_identical(read_results("v01", home = p$home)$manifest, old$manifest)
})

test_that("read_results for real does not read the check.csv of another scenario", {
  p <- local_project()
  save_run(p)
  write_lines(
    p$home,
    "v01_results/known/check.csv",
    c("assertion,expected,observed,pass", "a,1,2,FALSE")
  )
  expect_identical(names(read_results("v01", home = p$home))[1], "manifest")
  expect_error(read_results("v01", "known", home = p$home), "did not pass")
})

test_that("manifest_data fails a malformed failed_fake_checks, and a new real manifest without it, and passes an old one without it", {
  x <- valid_results()
  expect_identical(
    check_results(x)$status[check_results(x)$rule == "manifest_data"],
    "PASS"
  )
  for (bad in list(NA_character_, "real", c("null", "null"), 1L, "Null")) {
    x$manifest$failed_fake_checks <- bad
    found <- check_results(x)
    expect_identical(
      found$detail[found$rule == "manifest_data"],
      paste(
        "manifest$failed_fake_checks is not a character vector of names of",
        "fake scenarios. Each name has lower-case letters, digits and",
        "underscores only, is not \"real\" and comes once"
      ),
      info = deparse(bad)
    )
  }
  for (good in list(character(0), "null", c("known", "null"))) {
    x$manifest$failed_fake_checks <- good
    found <- check_results(x)
    expect_identical(found$status[found$rule == "manifest_data"], "PASS")
  }
  x$manifest$failed_fake_checks <- NULL
  x$manifest$pbn_version <- "2026.11.25"
  found <- check_results(x)
  expect_identical(
    found$detail[found$rule == "manifest_data"],
    paste(
      "manifest has no failed_fake_checks. pbn 2026.11.25 and newer write",
      "it for \"real\""
    )
  )
  x$manifest$pbn_version <- "2026.11.24"
  found <- check_results(x)
  expect_identical(found$status[found$rule == "manifest_data"], "PASS")
})

test_that("two runs on one day with the same inputs write the same bytes of the real results, with and without a failed fake scenario", {
  p <- local_project()
  real_md5 <- function() {
    return(unname(tools::md5sum(file.path(
      p$home,
      "v01_results",
      "real",
      "results.qs2"
    ))))
  }
  save_run(p)
  first <- real_md5()
  save_run(p)
  expect_identical(real_md5(), first)
  fail_null(p)
  save_run(p)
  failed <- real_md5()
  expect_false(identical(failed, first))
  save_run(p)
  expect_identical(real_md5(), failed)
})

test_that("an older real result stops reading as soon as save_results starts a run with real, also when the run stops during a fake check", {
  p <- local_project()
  expect_true(saved(save_run(p)))
  expect_identical(names(read_results("v01", home = p$home))[1], "manifest")
  # expect_null() interrupts the run. An interrupt is no error, so
  # save_results() does not catch it, and it stops in the check of null.
  write_lines(
    p$home,
    "v01_functions/expect_null.R",
    c(
      "expect_null <- function(results) {",
      "  stop(structure(class = c(\"interrupt\", \"condition\"),",
      "    list(message = \"interrupted\", call = NULL)))",
      "}"
    )
  )
  sourced <- init(p)
  sc <- scenarios(sourced$CFG)
  plan <- build_plan(sourced, sc)
  r <- run_plan(plan)
  stopped <- NULL
  utils::capture.output(
    stopped <- tryCatch(
      save_results(r, plan, sc),
      interrupt = function(e) return("interrupted")
    )
  )
  expect_identical(stopped, "interrupted")
  csv <- file.path(p$home, "v01_results", "real", "check.csv")
  expect_error(
    read_results("v01", home = p$home),
    paste0(csv, " holds an assertion that did not pass"),
    fixed = TRUE
  )
  expect_identical(utils::read.csv(csv)$observed, "not checked yet")
})
