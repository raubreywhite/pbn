# The run log <version>_results/run.md, which save_results() writes, and
# run_plan() when it stops on failed analyses.

run_md <- function(p) {
  return(readLines(file.path(p$home, "v01_results", "run.md"), warn = FALSE))
}

# The lines of the section `head` of the run log, without its heading: up to
# the next heading of the same level.
md_section <- function(lines, head) {
  at <- match(head, lines)
  if (is.na(at)) {
    return(NULL)
  }
  level <- sub(" .*$", " ", head)
  after <- which(startsWith(lines, level) & seq_along(lines) > at)
  end <- if (length(after) > 0) after[1] - 1 else length(lines)
  return(lines[seq(at + 1, end)])
}

# The cells of one row of a Markdown table, without their padding.
md_cells <- function(line) {
  cells <- strsplit(
    sub("^\\| ", "", sub(" \\|$", "", line)),
    " | ",
    fixed = TRUE
  )[[1]]
  return(trimws(cells))
}

test_that("save_results writes run.md with the sections in their order, and the last line it prints names the file", {
  p <- local_project()
  run <- save_run(p)
  lines <- run_md(p)
  expect_identical(lines[1], "# Run log of `project`, version v01")
  expect_identical(
    grep("^## ", lines, value = TRUE),
    c(
      "## When",
      "## Run",
      "## Scenarios",
      "## Data",
      "## Exclusions",
      "## Assertions",
      "## Output",
      "## Files",
      "## Timings",
      "## History"
    )
  )
  expect_identical(
    md_section(lines, "## Run")[2:3],
    c(
      "- Result: All 4 assertions passed.",
      "- Scenarios of this run: real, known and null."
    )
  )
  expect_identical(
    md_section(lines, "## Scenarios")[2:6],
    c(
      "| Scenario | Simulations | Analyses |",
      "|----------|------------:|---------:|",
      "| real     |           1 |        3 |",
      "| known    |           1 |        5 |",
      "| null     |           3 |       15 |"
    )
  )
  # The assertion table of null is that of its check.csv.
  check <- utils::read.csv(file.path(
    p$home,
    "v01_results",
    "null",
    "check.csv"
  ))
  null_rows <- md_section(lines, "### null")
  expect_true(all(vapply(
    check$assertion,
    function(a) return(any(startsWith(null_rows, paste0("| ", a)))),
    logical(1)
  )))
  expect_true(any(grepl(
    "| v01_results/null/sims_gitexclude/ | 3 files,",
    lines,
    fixed = TRUE
  )))
  expect_identical(
    run$out[length(run$out)],
    "pbn: wrote the run log v01_results/run.md."
  )
  # Plain ASCII, and no HTML.
  expect_true(all(!grepl("[^ -~]", lines)))
  expect_false(any(grepl("<[a-zA-Z/!]", lines)))
})

test_that("two runs of the same code give the same run.md without the sections When, Timings and History", {
  p <- local_project()
  save_run(p)
  first <- run_md(p)
  Sys.sleep(1.1)
  save_run(p)
  second <- run_md(p)
  # The sections When, Timings and History differ, so the whole files
  # differ.
  expect_false(identical(first, second))
  expect_identical(run_log_stable(second), run_log_stable(first))
  expect_identical(
    grep("^## ", run_log_stable(first), value = TRUE),
    c(
      "## Run",
      "## Scenarios",
      "## Data",
      "## Exclusions",
      "## Assertions",
      "## Output",
      "## Files"
    )
  )
  # The varying sections are the first and the last two.
  heads <- grep("^## ", first, value = TRUE)
  expect_identical(
    heads[c(1, length(heads) - 1, length(heads))],
    c("## When", "## Timings", "## History")
  )
})

test_that("run_log_stable removes the sections When, Timings and History, also with CR LF line ends", {
  lines <- c(
    "# T",
    "",
    "## When",
    "",
    "now",
    "",
    "## Run",
    "",
    "- a",
    "",
    "## Timings",
    "",
    "### Main",
    "",
    "1.00",
    "",
    "## History",
    "",
    "| Started | Result |",
    "|---|---|",
    "| 2026-11-23 14:02:00 | did not finish |"
  )
  expect_identical(run_log_stable(lines), c("# T", "", "## Run", "", "- a", ""))
  expect_identical(
    run_log_stable(paste0(lines, "\r")),
    c("# T", "", "## Run", "", "- a", "")
  )
})

test_that("a run with only = null says so, and names the other scenarios on the disk with the run_date of their manifests", {
  p <- local_project()
  save_run(p)
  save_run(p, only = "null")
  run <- md_section(run_md(p), "## Run")
  dates <- vapply(
    c("known", "real"),
    function(s) {
      return(
        qs2::qs_read(file.path(
          p$home,
          "v01_results",
          s,
          "results.qs2"
        ))$manifest$run_date
      )
    },
    character(1)
  )
  expect_identical(run[3], "- Scenarios of this run: null.")
  expect_identical(
    run[4],
    paste0(
      "- The results of the other scenarios on the disk are from earlier runs: `known` (run date `",
      dates[["known"]],
      "`) and `real` (run date `",
      dates[["real"]],
      "`)."
    )
  )
  # A full run names no earlier scenario.
  save_run(p)
  expect_false(any(grepl("from earlier runs", run_md(p), fixed = TRUE)))
})

test_that("a failed assertion: run.md says so in its result, and lists the failure first", {
  p <- local_project()
  write_lines(p$home, "v01_functions/expect_known.R", expect_known_lines(5))
  run <- save_run(p)
  expect_match(
    conditionMessage(run$err),
    "1 of 4 assertions failed",
    fixed = TRUE
  )
  lines <- run_md(p)
  expect_identical(
    md_section(lines, "## Run")[2],
    "- Result: 1 of 4 assertions failed."
  )
  heads <- grep("^## ", lines, value = TRUE)
  expect_identical(heads[3], "## Failures")
  expect_identical(
    md_section(lines, "### Failed assertions")[2:4],
    c(
      "| Scenario | Assertion              | Expected | Observed |",
      "|----------|------------------------|----------|----------|",
      "| known    | events of x in group a | 5        | 4        |"
    )
  )
})

test_that("a failure of check_results() writes run.md with the failed rules, and their details go to the errors file only", {
  p <- local_project()
  sourced <- init(p)
  sc <- scenarios(sourced$CFG)
  plan <- build_plan(sourced, sc)
  add_analyses(plan, sc, name = "rows", fn = function(data, argset) {
    return(data.frame(y = data$d$y))
  })
  r <- run_plan(plan)
  utils::capture.output(expect_error(
    save_results(r, plan, sc),
    "failures of check_results()"
  ))
  expect_identical(results_files(p), log_files)
  lines <- run_md(p)
  result <- md_section(lines, "## Run")[2]
  expect_match(
    result,
    "^- Result: [0-9]+ failures of check_results\\(\\), so save_results\\(\\) saved no results\\.$"
  )
  rules <- md_section(lines, "### Failed rules of check_results()")
  expect_true(any(grepl("^- `real: no_row_per_person`, error [0-9]+\\.$", rules)))
  # The detail names the element rows, which the project named. It is in
  # the errors file, and not in run.md.
  expect_false(any(grepl("rows", rules, fixed = TRUE)))
  errors <- readLines(file.path(p$home, "v01_results", "run_errors_gitexclude.md"))
  expect_true(any(grepl("real: no_row_per_person: ", errors, fixed = TRUE)))
  expect_identical(
    md_section(lines, "## Assertions")[2],
    "save_results() stopped before the expect_ functions ran, so the run has no assertion."
  )
})

test_that("run_plan that stops on failed analyses writes run.md with the numbers of the errors, and the full errors to the errors file", {
  p <- local_project()
  sourced <- init(p)
  sc <- scenarios(sourced$CFG)
  plan <- plnr::Plan$new()
  data_fn <- function(scenario, sim) {
    if (sim == 2) {
      stop("bad id 4711 ", strrep("x", 1200), " end")
    }
    return(data.frame(x = 1:3))
  }
  environment(data_fn) <- globalenv()
  add_data(plan, "d", fn = data_fn)
  add_analyses(plan, sc, "a", fn = function(data, argset) {
    if (argset$scenario == "real") {
      stop("bad id 4712")
    }
    return(data.frame(m = 1))
  })
  # The data of null 2 fails first, because the analyses that count the
  # rows come first. Its analyses fail with it, and the run stops: the
  # analysis of real, which would fail with error 2, does not run.
  out <- utils::capture.output(expect_error(
    run_plan(plan),
    "^2 of 10 analyses failed, and the run stopped early: 5 analyses did not run\\. "
  ))
  # The display says at once that the run stops, also in its first 60
  # seconds in a log.
  expect_match(
    out[1],
    "^pbn: \\[[0-9]+s\\] an analysis of the scenario null failed, so the run stops early: no new analysis starts\\.$"
  )
  expect_identical(out[2], "pbn: wrote the run log v01_results/run.md.")
  expect_identical(results_files(p), log_files)
  lines <- run_md(p)
  expect_false(any(grepl("471", lines, fixed = TRUE)))
  expect_identical(
    md_section(lines, "## Run")[2],
    paste0(
      "- Result: 2 of 10 analyses failed, so save_results() did not run. ",
      "The run stopped early: 5 analyses did not run."
    )
  )
  expect_match(
    md_section(lines, "## Failures")[2],
    "^The run had 1 error, error 1\\. The text of the errors is in v01_results/run_errors_gitexclude\\.md\\."
  )
  failed <- md_section(lines, "### Failed analyses")
  expect_identical(failed[2:5], c(
    paste0(
      "2 of 10 analyses failed. The run stopped early at the first failed ",
      "analysis: 5 analyses did not run."
    ),
    "",
    "- 2 analyses failed with error 1: `null__sim_0002__pbn_n_rows`, `null__sim_0002__a__1`.",
    ""
  ))
  errors <- readLines(file.path(p$home, "v01_results", "run_errors_gitexclude.md"))
  expect_identical(grep("^## Error", errors, value = TRUE), "## Error 1")
  # The whole message, with no cut.
  expect_true(any(errors == paste0(
    "The data function 'd' failed: bad id 4711 ",
    strrep("x", 1200),
    " end"
  )))
  expect_false(any(grepl("4712", errors, fixed = TRUE)))
  expect_identical(
    md_section(lines, "## Assertions")[2],
    "save_results() did not run, so the run has no assertion."
  )
  data <- md_section(lines, "## Data")
  expect_identical(
    md_cells(data[6]),
    c("d", "null", "3", "no cache, failed 1", "n/a", "n/a", "3")
  )
  # A log without an error deletes an errors file that is there, for
  # example one of a later failed run of the same plan.
  plan2 <- build_plan(sourced, sc)
  r2 <- run_plan(plan2)
  expect_false(file.exists(file.path(p$home, "v01_results", "run_errors_gitexclude.md")))
  write_lines(p$home, "v01_results/run_errors_gitexclude.md", "old")
  utils::capture.output(save_results(r2, plan2, sc))
  expect_false(file.exists(file.path(p$home, "v01_results", "run_errors_gitexclude.md")))
})

test_that("in a git repository that does not ignore the errors file, pbn does not write it, run.md says so, and the error of the run stays", {
  skip_if_not(nzchar(Sys.which("git")), "git is not installed")
  p <- local_project()
  git_in(p$home, "init", "-q")
  write_lines(p$home, ".gitignore", "*.log")
  sourced <- init(p)
  sc <- scenarios(sourced$CFG)
  plan <- plnr::Plan$new()
  add_data(plan, "d", fn = function(scenario, sim) return(data.frame(x = 1)))
  add_analyses(plan, sc, "a", fn = function(data, argset) stop("bad id 4711"))
  utils::capture.output(err <- tryCatch(run_plan(plan), error = function(e) e))
  expect_match(
    conditionMessage(err),
    "^1 of 10 analyses failed, and the run stopped early: 4 analyses did not run\\. "
  )
  expect_identical(results_files(p), "run.md")
  lines <- run_md(p)
  expect_false(any(grepl("4711", lines, fixed = TRUE)))
  expect_identical(
    md_section(lines, "## Failures")[2],
    paste0(
      "The run had 1 error, error 1. pbn did not write the text of the errors ",
      "to v01_results/run_errors_gitexclude.md, because git does not ignore ",
      "that file. Add the line *gitexclude* to .gitignore. The console showed ",
      "each error."
    )
  )
  # With the line, the file is there.
  write_lines(p$home, ".gitignore", "*gitexclude*")
  utils::capture.output(try(run_plan(plan), silent = TRUE))
  expect_identical(results_files(p), log_files)
})

test_that("an expect_ function that stops: run.md names the error by its number, and the errors file holds its text", {
  p <- local_project()
  write_lines(p$home, "v01_functions/expect_known.R", c(
    "expect_known <- function(results) {",
    "  stop(\"bad id 4711\")",
    "}"
  ))
  sourced <- init(p)
  sc <- scenarios(sourced$CFG)
  plan <- build_plan(sourced, sc)
  utils::capture.output(expect_error(save_results(run_plan(plan), plan, sc), "assertions failed"))
  lines <- run_md(p)
  expect_false(any(grepl("4711", lines, fixed = TRUE)))
  expect_identical(
    md_cells(md_section(lines, "### Failed assertions")[4]),
    c("known", "expect_known() runs without an error", "see the assertion", "error 1")
  )
  errors <- readLines(file.path(p$home, "v01_results", "run_errors_gitexclude.md"))
  expect_true(any(errors == "expect_known() stopped with this error."))
  expect_true(any(errors == "bad id 4711"))
})

test_that("run_plan replaces run.md at its start with a log that says the run has not finished, and deletes the errors file", {
  p <- local_project()
  sourced <- init(p)
  sc <- scenarios(sourced$CFG)
  plan <- build_plan(sourced, sc)
  utils::capture.output(save_results(run_plan(plan), plan, sc))
  write_lines(p$home, "v01_results/run_errors_gitexclude.md", "old")
  run_plan(plan)
  lines <- run_md(p)
  expect_identical(
    grep("^## ", lines, value = TRUE),
    c("## When", "## Run", "## History")
  )
  expect_identical(md_section(lines, "## Run")[2:3], c(
    "- Result: The run has not finished. run_plan() started it, and no later step wrote this log again.",
    "- Scenarios of this run: real, known and null."
  ))
  expect_false(file.exists(file.path(p$home, "v01_results", "run_errors_gitexclude.md")))
  # The result of the earlier run is in its line of the history only, and
  # this run has the line "did not finish".
  expect_false(any(grepl("assertions passed", run_log_stable(lines), fixed = TRUE)))
  history <- md_section(lines, "## History")
  rows <- history[startsWith(history, "| 20")]
  expect_length(rows, 2)
  expect_identical(md_cells(rows[1])[2], "All 4 assertions passed")
  expect_identical(md_cells(rows[2])[2], "did not finish")
})

test_that("save_results that stops in the checks of its arguments writes run.md as failed, with its error in the errors file", {
  p <- local_project()
  sourced <- init(p)
  sc <- scenarios(sourced$CFG)
  plan <- build_plan(sourced, sc)
  r <- run_plan(plan)
  utils::capture.output(save_results(r, plan, sc))
  expect_true(any(grepl("All 4 assertions passed", run_md(p), fixed = TRUE)))
  out <- utils::capture.output(expect_error(
    save_results(r, plan, sc, gitexclude = "no_such_kind"),
    "not analysis kinds of the plan"
  ))
  lines <- run_md(p)
  expect_identical(
    md_section(lines, "## Run")[2],
    "- Result: save_results() stopped before it saved anything. The section Failures names its error."
  )
  expect_false(any(grepl("no_such_kind", lines, fixed = TRUE)))
  expect_identical(
    md_section(lines, "### Error of save_results()")[2],
    "save_results() stopped with error 1."
  )
  errors <- readLines(file.path(p$home, "v01_results", "run_errors_gitexclude.md"))
  expect_true(any(grepl("no_such_kind", errors, fixed = TRUE)))
})

test_that("make_outputs adds its section to run.md after save_results in the same R session, and leaves run.md alone otherwise", {
  p <- local_project()
  cfg <- cfg_lines()
  cfg[
    cfg == "    sims = c(1, 1, 3),"
  ] <- "    sims = c(1, 1, 3), output = c(FALSE, TRUE, FALSE),"
  write_lines(p$home, "v01_functions/_cfg.R", cfg)
  run <- save_run(p)
  fn <- function(results, dir) {
    writeLines("x", file.path(dir, "figure.png"))
    return(invisible(NULL))
  }
  utils::capture.output(make_outputs(fn, run$scenarios))
  lines <- run_md(p)
  expect_identical(
    md_section(lines, "## Output")[2:5],
    c(
      "make_outputs() wrote these files:",
      "",
      "- real: `v01_results/real/output/figure.png`",
      "- known: `v01_results/known/output_fake_data/fake_data_figure.png`"
    )
  )
  main <- md_section(lines, "### Main session")
  expect_true(any(startsWith(main, "| make_outputs(): real ")))
  expect_true(any(startsWith(main, "| make_outputs(): known ")))

  # initialize_project() starts the project again and forgets the log of the
  # run, as in a new R session, so make_outputs() does not touch run.md.
  before <- md5_of(file.path(p$home, "v01_results", "run.md"))
  suppressMessages(initialize_project(
    "v01",
    p$home,
    p$data_default,
    env = new.env(parent = globalenv())
  ))
  printed <- utils::capture.output(make_outputs(fn, run$scenarios))
  expect_false(any(startsWith(printed, "pbn: wrote the run log")))
  expect_identical(md5_of(file.path(p$home, "v01_results", "run.md")), before)
})

test_that("the timings section holds a wall time for each step of the main session, as numbers of 0 or more", {
  p <- local_project()
  save_run(p)
  main <- md_section(run_md(p), "### Main session")
  rows <- main[startsWith(main, "| ") & !startsWith(main, "| Step")]
  steps <- trimws(sub("^\\| (.*?) +\\|.*$", "\\1", rows))
  expect_identical(
    steps,
    c(
      "run_plan()",
      "save_results(): build, write and stack the results",
      "save_results(): the checks of check_results()",
      "save_results(): the expect_ functions",
      "save_results() in total"
    )
  )
  seconds <- as.numeric(sub("^.*\\| +([0-9.]+) \\|$", "\\1", rows))
  expect_true(all(!is.na(seconds) & seconds >= 0))
})

test_that("the run log holds no value of a person: a value planted in the data is not in run.md", {
  p <- local_project()
  write_lines(
    p$home,
    "v01_functions/get_data.R",
    sub(
      "z = 1:10",
      "z = 1:10, id = \"person_7731_secret\"",
      get_data_lines(),
      fixed = TRUE
    )
  )
  save_run(p)
  expect_true(any(grepl(
    "person_7731_secret",
    readLines(file.path(p$home, "v01_functions", "get_data.R")),
    fixed = TRUE
  )))
  expect_false(any(grepl("7731", run_md(p), fixed = TRUE)))
})

test_that("unknown_path knows run.md and run.pdf in vNN_results/, and warns on another file there", {
  p <- local_project()
  write_lines(p$home, "v01_results/run.md", "# Run log")
  write_lines(p$home, "v01_results/run.pdf", "%PDF")
  write_lines(p$home, "v01_results/run_errors_gitexclude.md", "# Errors")
  found <- project_findings(p$home)
  expect_identical(found$status[found$rule == "unknown_path"], "PASS")
  write_lines(p$home, "v01_results/notes.txt", "x")
  found <- project_findings(p$home)
  expect_identical(failed_rules(found), character(0))
  expect_identical(found$status[found$rule == "unknown_path"], "WARN")
  expect_identical(
    found$path[found$rule == "unknown_path"],
    "v01_results/notes.txt"
  )
})

test_that("the run log shows the exclusion table of each scenario, the rows before and after the exclusions, and the cache", {
  p <- local_cohort_project()
  run <- cohort_run(p, steps = cohort_steps)
  utils::capture.output(save_results(run$r, run$plan, run$scenarios))
  lines <- run_md(p)
  data <- md_section(lines, "## Data")
  expect_identical(
    md_cells(data[4]),
    c("d", "real", "1", "computed 1", "8", "6", "6")
  )
  real <- md_section(lines, "### real")
  expect_identical(md_cells(real[4]), c("d", "age under 18", "8", "2", "6"))
  null <- md_section(lines, "### null")
  expect_identical(null[2], "The table is per simulation. This is simulation 1 of 2.")
  expect_true(any(
    lines == "pbn: data d, real sim_0001: computed, because there is no cache file."
  ))
  # A second run reads the cache.
  run <- cohort_run(p, steps = cohort_steps)
  utils::capture.output(save_results(run$r, run$plan, run$scenarios))
  data <- md_section(run_md(p), "## Data")
  expect_identical(md_cells(data[4])[4], "reused 1")
})

test_that("a newline in a project string cannot start a heading or a list item in run.md", {
  p <- local_cohort_project()
  write_lines(p$home, "v01_functions/data_cohort.R", c(
    "data_cohort <- function(d) {",
    "  cp <- cohort::CohortPipeline$new(d)",
    "  cp$exclude_and_track(\"root\", \"age\\n## Timings\\n- item\", \"age < 18\")",
    "  cp",
    "}"
  ))
  write_lines(p$home, "v01_functions/expect_null.R", c(
    "expect_null <- function(results) {",
    "  data.frame(assertion = \"one\\n## Timings\", expected = \"x\\n- item\",",
    "    observed = \"x\", pass = TRUE)",
    "}"
  ))
  run <- cohort_run(p, steps = cohort_steps)
  utils::capture.output(save_results(run$r, run$plan, run$scenarios))
  fn <- function(results, dir) {
    writeLines("x", file.path(dir, "fig.png"))
    return(invisible(NULL))
  }
  utils::capture.output(make_outputs(fn, run$scenarios))
  lines <- run_md(p)
  expect_identical(sum(lines == "## Timings"), 1L)
  expect_false(any(startsWith(lines, "- item")))
  expect_identical(md_cells(md_section(lines, "### real")[4])[2], "age ## Timings - item")
  checks <- md_section(md_section(lines, "## Assertions"), "### null")
  expect_identical(md_cells(checks[4])[1:2], c("one ## Timings", "x - item"))
  expect_identical(
    md_section(lines, "## Output")[4],
    "- real: `v01_results/real/output/fig.png`"
  )
  # md_inline() keeps a project string on one line, as a code span.
  expect_identical(md_inline("a\n## b"), "`a ## b`")
  expect_identical(md_inline("x `y` z"), "``x `y` z``")
})

test_that("make_outputs that stops: the result line says so, run.md names the error by its number, and the errors file holds its text", {
  p <- local_project()
  sourced <- init(p)
  sc <- scenarios(sourced$CFG)
  plan <- build_plan(sourced, sc)
  utils::capture.output(save_results(run_plan(plan), plan, sc))
  fn <- function(results, dir) stop("bad id 4711")
  utils::capture.output(expect_error(
    make_outputs(fn, sc),
    "The output function failed for the scenario \"real\": bad id 4711",
    fixed = TRUE
  ))
  lines <- run_md(p)
  expect_false(any(grepl("4711", lines, fixed = TRUE)))
  expect_identical(
    md_section(lines, "## Run")[2],
    "- Result: All 4 assertions passed. make_outputs() failed for the scenario real."
  )
  expect_identical(
    md_section(lines, "### Error of make_outputs()")[2],
    "make_outputs() stopped at the scenario real with error 1."
  )
  errors <- readLines(file.path(p$home, "v01_results", "run_errors_gitexclude.md"))
  expect_true(any(grepl("bad id 4711", errors, fixed = TRUE)))
})


test_that("the Timings section gives clock seconds only: the main session, each scenario, and the median, the maximum and the share of each data step and analysis kind", {
  p <- local_project()
  save_run(p)
  lines <- run_md(p)
  timings <- md_section(lines, "## Timings")
  expect_identical(
    grep("^### ", timings, value = TRUE),
    c("### Main session", "### Scenarios", "### Data steps", "### Analysis kinds")
  )
  expect_false(any(grepl("computing|Summed|CPU", timings)))
  scen <- md_section(lines, "### Scenarios")
  expect_identical(md_cells(scen[4]), c("Scenario", "Wall seconds"))
  walls <- vapply(scen[6:8], function(x) return(md_cells(x)), character(2))
  expect_identical(walls[1, ], c("real", "known", "null"), ignore_attr = TRUE)
  expect_true(all(as.numeric(walls[2, ]) >= 0))
  kinds <- md_section(lines, "### Analysis kinds")
  # The last subsection ends where the section History starts.
  kinds <- kinds[seq_len(match("## History", kinds) - 1)]
  expect_identical(
    md_cells(kinds[2]),
    c("Scenario", "Kind", "Analyses", "Median", "Maximum", "Share")
  )
  steps <- md_section(lines, "### Data steps")
  expect_identical(
    md_cells(steps[2]),
    c("Data", "Scenario", "Step", "Calls", "Median", "Maximum", "Share")
  )
  rows <- c(kinds[startsWith(kinds, "| ")][-1], steps[startsWith(steps, "| ")][-1])
  rows <- rows[!grepl("^\\|-", rows)]
  shares <- vapply(rows, function(x) return(utils::tail(md_cells(x), 1)), "")
  expect_true(all(grepl("^[0-9]+(\\.[0-9])?%$", shares)))
  # The shares are parts of one sum, so they add up to 100%, give or take
  # the rounding of each.
  expect_lt(abs(sum(as.numeric(sub("%", "", shares))) - 100), 0.5 * length(shares) + 0.1)
})

test_that("share gives one decimal below 10% and whole numbers from 10%, and the wall seconds of a scenario run from the start of its first record to the end of its last", {
  expect_identical(share(c(98, 1.5, 0.5), 100), c("98%", "1.5%", "0.5%"))
  expect_identical(share(c(10, 0), 100), c("10%", "0.0%"))
  expect_identical(share(1, 0), "n/a")
  log <- list(
    scenarios = data.frame(scenario = c("a", "b")),
    run = list(
      # A data step of a from 8 to 10, and analyses of a from 12 to 15 and
      # from 19 to 20: 12 seconds. b has no timed record.
      data = data.frame(scenario = "a", seconds = 2, at = 10),
      analyses = data.frame(
        scenario = c("a", "a", "b"),
        seconds = c(3, 1, NA),
        at = c(15, 20, NA)
      )
    )
  )
  expect_identical(
    scenario_timings(log),
    data.frame(Scenario = c("a", "b"), `Wall seconds` = c("12.00", "n/a"), check.names = FALSE)
  )
})

test_that("the section Run names pbn, plnr and cohort, and the number of the other packages of the manifest", {
  p <- local_project()
  save_run(p)
  line <- grep("^- Packages: ", run_md(p), value = TRUE)
  manifest <- read_results("v01", home = p$home)$manifest
  others <- length(setdiff(names(manifest$packages), c("pbn", "plnr", "cohort")))
  expect_match(
    line,
    paste0(
      "^- Packages: pbn [0-9.]+, plnr [0-9.]+ and cohort [^,]+, and ",
      others,
      " other packages, listed in the manifest of each results file\\.$"
    )
  )
})

test_that("a scenario whose cohort has no exclusion step gets one sentence, and a run without any step one sentence for the section", {
  steps <- function(n) {
    return(data.frame(
      data = rep("d", n),
      step = rep("age under 18", n),
      n_before = rep(8L, n),
      n_excluded = rep(2L, n),
      n_after = rep(6L, n)
    ))
  }
  log <- list(
    scenarios = data.frame(scenario = c("real", "null"), sims = c(1, 2)),
    run = list(exclusions = list(
      real = list(d = steps(1), e = steps(0)),
      null = list(d = steps(0), e = steps(0))
    ))
  )
  lines <- md_exclusions(log)
  expect_identical(
    md_section(lines, "### real"),
    c(
      "",
      "`e`: no exclusion step.",
      "",
      "| Data | Step         | Rows before | Excluded | Rows after |",
      "|------|--------------|------------:|---------:|-----------:|",
      "| d    | age under 18 |           8 |        2 |          6 |",
      ""
    )
  )
  expect_identical(
    md_section(lines, "### null"),
    c(
      "",
      "The table is per simulation. This is simulation 1 of 2.",
      "",
      "`d`: no exclusion step.",
      "",
      "`e`: no exclusion step.",
      ""
    )
  )
  log$run$exclusions$real <- list(d = steps(0))
  expect_identical(
    md_exclusions(log),
    c("## Exclusions", "", "No cohort of this run has an exclusion step.", "")
  )
})

test_that("each row of a Markdown table is as wide as its header row: the row of dashes, also for a cell longer than 60 characters, and a row with an escaped |, and a short column of a wide table gets 6% of the dashes", {
  tab <- md_table(data.frame(
    Assertion = c(strrep("a", 75), "|bias| <= 0.08"),
    Pass = c("yes", "no")
  ))
  # The table is wider than 72 characters, so Pass gets at least 6% of the
  # 83 dashes: 5 characters and its 2 spaces.
  expect_identical(tab[4], paste0("| \\|bias\\| <= 0.08", strrep(" ", 59), " | no    |"))
  expect_identical(tab[2], paste0("|", strrep("-", 77), "|-------|"))
  expect_identical(nchar(tab), rep(nchar(tab[1]), 4))
})

test_that("make_outputs counts the warnings of the output function of each scenario in the section Output, without their text", {
  p <- local_project()
  run <- save_run(p)
  fn <- function(results, dir) {
    warning("NaNs produced 4711")
    warning("NaNs produced 4712")
    writeLines("x", file.path(dir, "figure.png"))
    return(invisible(NULL))
  }
  utils::capture.output(suppressWarnings(make_outputs(fn, run$scenarios)))
  lines <- run_md(p)
  expect_identical(
    md_section(lines, "## Output")[4:5],
    c(
      "- real: `v01_results/real/output/figure.png`",
      "- real: 2 warnings of the output function."
    )
  )
  expect_false(any(grepl("471", lines, fixed = TRUE)))
  # The warnings still reach the console.
  warned <- testthat::capture_warnings(
    utils::capture.output(make_outputs(fn, run$scenarios))
  )
  expect_identical(warned, c("NaNs produced 4711", "NaNs produced 4712"))
})

# The rows of the table of the section History of `lines`, as cells.
history_table <- function(lines) {
  section <- md_section(lines, "## History")
  rows <- section[grepl("^\\| [0-9]{4}-", section)]
  return(lapply(rows, md_cells))
}

test_that("the section History holds one line for each run, oldest first: a full run, a run with only =, a failed run, a run that R stopped, and a run with make_outputs", {
  skip_if_pbn_not_installed()
  p <- local_project()
  save_run(p)
  save_run(p, only = "null")
  # A failed run.
  sourced <- init(p)
  sc <- scenarios(sourced$CFG)
  bad <- plnr::Plan$new()
  add_data(bad, "d", fn = function(scenario, sim) return(data.frame(x = 1)))
  add_analyses(bad, sc, "a", fn = function(data, argset) stop("bad id 4711"))
  utils::capture.output(try(run_plan(bad), silent = TRUE))
  # A run in a new R session that R stops while it runs.
  write_lines(p$home, "v01_slow.R", c(
    paste0("pbn::initialize_project(version = \"v01\", home = \".\", folders = c(main = \"", p$data_default, "\"))"),
    "p <- plnr::Plan$new()",
    "pbn::add_data(p, \"d\", fn = function(scenario, sim) return(data.frame(x = 1)))",
    "pbn::add_analyses(p, pbn::scenarios(CFG), \"a\", fn = function(data, argset) Sys.sleep(60))",
    "r <- pbn::run_plan(p)"
  ))
  slow <- callr::rscript_process$new(callr::rscript_process_options(
    script = file.path(p$home, "v01_slow.R"),
    wd = p$home
  ))
  # kill_tree() also ends the R process that Rscript starts on Windows.
  withr::defer(slow$kill_tree())
  deadline <- Sys.time() + 60
  while (Sys.time() < deadline && length(history_table(run_md(p))) < 4) {
    Sys.sleep(0.2)
  }
  slow$kill_tree()
  # The last run, in this R session, with make_outputs().
  run <- save_run(p)
  out_fn <- function(results, dir) {
    writeLines("x", file.path(dir, "figure.png"))
    return(invisible(NULL))
  }
  utils::capture.output(make_outputs(out_fn, run$scenarios))
  rows <- history_table(run_md(p))
  expect_length(rows, 5)
  expect_identical(
    vapply(rows, function(x) return(x[2]), ""),
    c(
      "All 4 assertions passed",
      "All 2 assertions passed",
      "1 of 10 analyses failed, stopped early",
      "did not finish",
      "All 4 assertions passed"
    )
  )
  expect_identical(vapply(rows, function(x) return(x[3]), ""), c("3", "null", "3", "3", "3"))
  expect_identical(vapply(rows, function(x) return(x[4]), ""), rep("1", 5))
  # The wall seconds of run_plan(), save_results() and make_outputs(), and
  # an empty cell for a step that did not run.
  steps <- t(vapply(rows, function(x) return(x[5:7]), character(3)))
  expect_identical(steps[3:4, 2:3], matrix("", 2, 2))
  expect_identical(steps[4, 1], "")
  expect_identical(steps[1:2, 3], c("", ""))
  expect_true(all(grepl("^[0-9]+\\.[0-9]{2}$", c(steps[c(1, 2, 3, 5), 1], steps[c(1, 2, 5), 2], steps[5, 3]))))
  expect_identical(
    unique(vapply(rows, function(x) return(x[8]), "")),
    as.character(utils::packageVersion("pbn"))
  )
  started <- vapply(rows, function(x) return(x[1]), "")
  expect_identical(started, sort(started))
  # History is the last section.
  heads <- grep("^## ", run_md(p), value = TRUE)
  expect_identical(heads[length(heads)], "## History")
})

test_that("a damaged section History keeps the lines that pbn can read, says so in one line, and never stops the run", {
  p <- local_project()
  save_run(p)
  file <- file.path(p$home, "v01_results", "run.md")
  lines <- readLines(file)
  rows <- which(grepl("^\\| [0-9]{4}-", lines))
  # A hand edit that breaks the cells of a second line of the table.
  lines <- append(lines, "| 2026-01-01 10:00:00 | All 1 assertions passed | 3 |", after = rows)
  writeLines(lines, file)
  printed <- utils::capture.output(save_run(p))
  out <- printed[startsWith(printed, "pbn: pbn could not read")]
  expect_identical(
    out,
    "pbn: pbn could not read 1 line of the section History of v01_results/run.md, and the history leaves it out."
  )
  expect_length(history_table(run_md(p)), 2)

  # A section with no line that pbn can read starts again.
  lines <- readLines(file)
  lines[grepl("^\\| [0-9]{4}-", lines)] <- "| broken |"
  writeLines(lines, file)
  printed <- utils::capture.output(save_run(p))
  expect_identical(
    printed[startsWith(printed, "pbn: pbn could not read")],
    "pbn: pbn could not read the section History of v01_results/run.md, so the history starts again."
  )
  expect_length(history_table(run_md(p)), 1)

  # A deleted run.md starts the history again, with no line.
  unlink(file)
  printed <- utils::capture.output(save_run(p))
  expect_false(any(startsWith(printed, "pbn: pbn could not read")))
  expect_length(history_table(run_md(p)), 1)
})
