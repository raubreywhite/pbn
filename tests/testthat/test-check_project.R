project_rule_names <- c(
  "lower_case_names",
  "run_script_initialises",
  "version_items_complete",
  "results_contract",
  "manual_manuscript_names",
  "no_local_copy_of_pbn_function",
  "loc_per_file",
  "no_environment_variables",
  "unknown_path"
)

test_that("check_project passes the valid synthetic project on every rule", {
  p <- local_project()
  found <- project_findings(p$home)
  expect_identical(found$rule, project_rule_names)
  expect_identical(found$status, rep("PASS", length(project_rule_names)))
})

test_that("check_project prints one line per rule and a count line", {
  p <- local_project()
  out <- capture.output(found <- check_project(p$home))
  expect_length(out, length(project_rule_names) + 1)
  expect_identical(
    out[1],
    "PASS  lower_case_names  no capital letter in a name"
  )
  expect_identical(out[length(out)], "9 PASS, 0 WARN, 0 FAIL")
  expect_identical(names(found), c("rule", "status", "path", "detail"))
})

test_that("pbn_exports is the export list of the NAMESPACE file", {
  ns <- readLines(system.file("NAMESPACE", package = "pbn"))
  exported <- sub(
    "^export\\((.*)\\)$",
    "\\1",
    grep("^export\\(", ns, value = TRUE)
  )
  expect_setequal(pbn_exports, exported)
})

test_that("lower_case_names fails on a capital letter in a name", {
  p <- local_project()
  write_lines(p$home, "adhoc/Notes.txt", "x")
  found <- project_findings(p$home)
  expect_identical(failed_rules(found), "lower_case_names")
  expect_identical(found$path[found$status == "FAIL"], "adhoc/Notes.txt")
})

test_that("lower_case_names fails on a capital letter in a folder name", {
  p <- local_project()
  dir.create(file.path(p$home, "adhoc", "Old"))
  found <- project_findings(p$home)
  expect_identical(failed_rules(found), "lower_case_names")
  expect_identical(found$path[found$status == "FAIL"], "adhoc/Old")
})

test_that("run_script_initialises fails on the wrong version", {
  p <- local_project()
  lines <- readLines(file.path(p$home, "v01_run.R"))
  lines[1] <- sub("\"v01\"", "\"v02\"", lines[1], fixed = TRUE)
  writeLines(lines, file.path(p$home, "v01_run.R"))
  found <- project_findings(p$home)
  expect_identical(failed_rules(found), "run_script_initialises")
})

test_that("run_script_initialises fails when the call is only in a comment", {
  p <- local_project()
  lines <- readLines(file.path(p$home, "v01_run.R"))
  lines[1] <- paste("#", lines[1])
  lines <- c(lines[1], "p <- list(data_raw = \".\")", lines[-1])
  writeLines(lines, file.path(p$home, "v01_run.R"))
  found <- project_findings(p$home)
  expect_identical(failed_rules(found), "run_script_initialises")
})

test_that("run_script_initialises reads a version given by position", {
  p <- local_project()
  writeLines(
    "pbn::initialize_project(\"v01\", \".\", \"x\")",
    file.path(p$home, "v01_run.R")
  )
  found <- project_findings(p$home)
  expect_identical(failed_rules(found), character(0))
})

test_that("version_items_complete fails on a run script without functions", {
  p <- local_project()
  write_lines(p$home, "v02_run.R", run_script_lines("v02", p$data_default))
  found <- project_findings(p$home)
  expect_identical(failed_rules(found), "version_items_complete")
  expect_identical(found$path[found$status == "FAIL"], "v02_run.R")
})

test_that("version_items_complete fails on functions without a run script", {
  p <- local_project()
  dir.create(file.path(p$home, "v02_functions"))
  found <- project_findings(p$home)
  expect_identical(failed_rules(found), "version_items_complete")
  expect_identical(found$path[found$status == "FAIL"], "v02_functions")
})

test_that("results_contract passes a valid results file", {
  p <- local_project()
  dir.create(file.path(p$home, "v01_results"))
  x <- list(
    manifest = list(n_rows_data = 10L),
    exclusions = data.frame(
      step = "a",
      n_before = 10,
      n_excluded = 0,
      n_after = 10
    ),
    estimates = data.frame(term = "x", estimate = 1),
    tables = list(),
    extra = list()
  )
  qs2::qs_save(x, file.path(p$home, "v01_results", "results.qs2"))
  found <- project_findings(p$home)
  expect_identical(failed_rules(found), character(0))
  expect_identical(found$status[found$rule == "results_contract"], "PASS")
})

test_that("results_contract fails on a results file that fails check_results", {
  p <- local_project()
  dir.create(file.path(p$home, "v01_results"))
  x <- list(
    manifest = list(n_rows_data = 10L),
    exclusions = data.frame(
      step = "a",
      n_before = 10,
      n_excluded = 1,
      n_after = 10
    ),
    estimates = data.frame(term = "x", estimate = 1),
    tables = list(),
    extra = list()
  )
  qs2::qs_save(x, file.path(p$home, "v01_results", "results.qs2"))
  found <- project_findings(p$home)
  expect_identical(failed_rules(found), "results_contract")
  expect_match(
    found$detail[found$status == "FAIL"],
    "exclusions_table",
    fixed = TRUE
  )
})

test_that("manual_manuscript_names fails on a name outside the pattern", {
  p <- local_project()
  write_lines(p$home, "manuscript/manual/v01_ab.docx", "")
  found <- project_findings(p$home)
  expect_identical(failed_rules(found), "manual_manuscript_names")
  expect_identical(
    found$path[found$status == "FAIL"],
    "manuscript/manual/v01_ab.docx"
  )
})

test_that("no_local_copy_of_pbn_function fails on a local table1", {
  p <- local_project()
  write_lines(
    p$home,
    "v01_functions/tables.R",
    "table1 <- function(d) return(d)"
  )
  found <- project_findings(p$home)
  expect_identical(failed_rules(found), "no_local_copy_of_pbn_function")
  expect_identical(found$path[found$status == "FAIL"], "v01_functions/tables.R")
})

test_that("no_local_copy_of_pbn_function passes a call to a pbn export", {
  p <- local_project()
  write_lines(
    p$home,
    "v01_functions/tables.R",
    "my_table <- function(d) return(pbn::table1(d, \"g\", CFG$vars))"
  )
  found <- project_findings(p$home)
  expect_identical(failed_rules(found), character(0))
})

test_that("loc_per_file fails on a file with 1001 code lines and passes 1000", {
  p <- local_project()
  write_lines(p$home, "adhoc/long.R", paste0("x", seq_len(1000), " <- 1"))
  expect_identical(failed_rules(project_findings(p$home)), character(0))

  write_lines(p$home, "adhoc/long.R", paste0("x", seq_len(1001), " <- 1"))
  found <- project_findings(p$home)
  expect_identical(failed_rules(found), "loc_per_file")
  expect_identical(found$path[found$status == "FAIL"], "adhoc/long.R")
})

test_that("no_environment_variables fails on Sys.getenv in a functions folder", {
  p <- local_project()
  write_lines(
    p$home,
    "v01_functions/double_it.R",
    c("double_it <- function(x) return(2 * x)", "key <- Sys.getenv(\"KEY\")")
  )
  found <- project_findings(p$home)
  expect_identical(failed_rules(found), "no_environment_variables")
  expect_match(
    found$detail[found$status == "FAIL"],
    "Sys.getenv on line 2",
    fixed = TRUE
  )
})

test_that("no_environment_variables fails on Sys.setenv in a run script", {
  p <- local_project()
  lines <- c(
    readLines(file.path(p$home, "v01_run.R")),
    "Sys.setenv(KEY = \"1\")"
  )
  writeLines(lines, file.path(p$home, "v01_run.R"))
  found <- project_findings(p$home)
  expect_identical(failed_rules(found), "no_environment_variables")
})

test_that("no_environment_variables passes the word in a string or a comment", {
  p <- local_project()
  write_lines(
    p$home,
    "v01_functions/note.R",
    c("# Sys.getenv is not used", "note <- \"Sys.getenv\"")
  )
  expect_identical(failed_rules(project_findings(p$home)), character(0))
})

test_that("unknown_path warns on a top-level path the layout does not name", {
  p <- local_project()
  write_lines(p$home, "notes.txt", "x")
  found <- project_findings(p$home)
  expect_identical(failed_rules(found), character(0))
  expect_identical(found$status[found$rule == "unknown_path"], "WARN")
  expect_identical(found$path[found$rule == "unknown_path"], "notes.txt")
})

test_that("an allowed finding becomes WARN with (allowed)", {
  p <- local_project()
  write_lines(p$home, "adhoc/Notes.txt", "x")
  write_lines(p$home, "pbn_allow.txt", "lower_case_names\tadhoc/Notes.txt")
  found <- project_findings(p$home)
  expect_identical(failed_rules(found), character(0))
  row <- found[found$rule == "lower_case_names", ]
  expect_identical(row$status, "WARN")
  expect_identical(
    row$detail,
    "adhoc/Notes.txt: the name has a capital letter (allowed)"
  )
  expect_false("pbn_allow" %in% found$rule)
})

test_that("a stale allowlist entry is a WARN", {
  p <- local_project()
  write_lines(p$home, "pbn_allow.txt", "loc_per_file\tadhoc/gone.R")
  found <- project_findings(p$home)
  expect_identical(failed_rules(found), character(0))
  stale <- found[found$rule == "pbn_allow", ]
  expect_identical(stale$status, "WARN")
  expect_match(stale$detail, "loc_per_file<TAB>adhoc/gone.R", fixed = TRUE)
})

test_that("check_project quits with status 1 after it prints every line", {
  skip_if_pbn_not_installed()
  p <- local_project()
  ok <- callr::rscript(
    file.path(p$home, "check_project.R"),
    wd = p$home,
    show = FALSE,
    fail_on_status = FALSE
  )
  expect_identical(ok$status, 0L)
  expect_match(ok$stdout, "9 PASS, 0 WARN, 0 FAIL", fixed = TRUE)

  write_lines(p$home, "adhoc/Notes.txt", "x")
  bad <- callr::rscript(
    file.path(p$home, "check_project.R"),
    wd = p$home,
    show = FALSE,
    fail_on_status = FALSE
  )
  expect_identical(bad$status, 1L)
  expect_match(
    bad$stdout,
    "FAIL  lower_case_names  adhoc/Notes.txt",
    fixed = TRUE
  )
  expect_match(bad$stdout, "PASS  unknown_path", fixed = TRUE)
  expect_match(bad$stdout, "8 PASS, 0 WARN, 1 FAIL", fixed = TRUE)
})

test_that("check_project reads only the files that git does not ignore", {
  skip_if_not(nzchar(Sys.which("git")), "git is not installed")
  p <- local_project()
  writeLines(c("v01_results/", "Notes.TXT"), file.path(p$home, ".gitignore"))
  git <- system2(
    "git",
    c("-C", shQuote(p$home), "init", "-q"),
    stdout = TRUE,
    stderr = TRUE
  )
  expect_null(attr(git, "status"))
  write_lines(p$home, "adhoc/Notes.TXT", "x")
  write_lines(p$home, ".DS_Store", "x")
  write_lines(p$home, "adhoc/.DS_Store", "x")

  found <- project_findings(p$home)
  expect_identical(found$rule, project_rule_names)
  expect_identical(found$status, rep("PASS", length(project_rule_names)))

  # Un-ignore the file. git now lists it, so the rule fails.
  writeLines("v01_results/", file.path(p$home, ".gitignore"))
  found <- project_findings(p$home)
  expect_identical(failed_rules(found), "lower_case_names")
  expect_identical(found$path[found$status == "FAIL"], "adhoc/Notes.TXT")
})

test_that("check_project leaves out OS and editor files outside git", {
  p <- local_project()
  write_lines(p$home, ".DS_Store", "x")
  write_lines(p$home, "adhoc/.DS_Store", "x")
  write_lines(p$home, ".Rhistory", "x")
  write_lines(p$home, ".RData", "x")
  write_lines(p$home, ".Rproj.user/Shared/notebooks/A.txt", "x")
  write_lines(p$home, "manuscript/manual/~$v01_AB.docx", "x")
  write_lines(p$home, "adhoc/~$Draft.docx", "x")
  write_lines(p$home, ".Rbuildignore", "x")
  write_lines(p$home, "Project.Rproj", "x")

  found <- project_findings(p$home)
  expect_identical(found$rule, project_rule_names)
  expect_identical(found$status, rep("PASS", length(project_rule_names)))
})

test_that("unknown_path knows the check scripts and pbn_allow.txt", {
  p <- local_project()
  write_lines(p$home, "v01_check_functions.R", "x <- 1")
  write_lines(p$home, "v01_check_scenarios.R", "x <- 1")
  write_lines(p$home, "pbn_allow.txt", character(0))

  found <- project_findings(p$home)
  expect_identical(found$rule, project_rule_names)
  expect_identical(found$status, rep("PASS", length(project_rule_names)))
})

test_that("results_contract fails on a scenario results file that fails check_results", {
  p <- local_project()
  folder <- file.path(p$home, "v01_results", "scenario", "made_up")
  dir.create(folder, recursive = TRUE)
  x <- list(
    manifest = list(n_rows_data = 10L),
    exclusions = data.frame(
      step = "a",
      n_before = 10,
      n_excluded = 1,
      n_after = 10
    ),
    estimates = data.frame(term = "x", estimate = 1),
    tables = list(),
    extra = list()
  )
  qs2::qs_save(x, file.path(folder, "results.qs2"))
  found <- project_findings(p$home)
  expect_identical(failed_rules(found), "results_contract")
  expect_identical(
    found$path[found$status == "FAIL"],
    "v01_results/scenario/made_up/results.qs2"
  )
  expect_match(
    found$detail[found$status == "FAIL"],
    "exclusions_table",
    fixed = TRUE
  )
})
