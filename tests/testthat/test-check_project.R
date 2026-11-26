project_rule_names <- c(
  "lower_case_names",
  "run_script_initialises",
  "version_items_complete",
  "results_contract",
  "gitexclude_not_tracked",
  "scenario_checks",
  "manual_manuscript_names",
  "manuscript_reads_real",
  "fake_data_names",
  "no_local_copy_of_pbn_function",
  "loc_per_file",
  "cyclomatic_complexity",
  "no_environment_variables",
  "no_local_assign",
  "no_absolute_paths",
  "blocks_list",
  "real_results_are_fake",
  "unknown_path"
)

test_that("check_project passes the valid synthetic project on every rule", {
  p <- local_project()
  found <- project_findings(p$home)
  expect_identical(found$rule, project_rule_names)
  expect_identical(found$status, rep("PASS", length(project_rule_names)))
})

test_that("check_project prints the line of the rules, one line per rule and a count line", {
  p <- local_project()
  out <- capture.output(found <- check_project(p$home))
  expect_length(out, length(project_rule_names) + 2)
  expect_identical(
    out[1],
    "Rules: house_2026_11_17, 0 overrides, 0 allowed paths (pbn.yml)"
  )
  expect_identical(
    out[2],
    "PASS  lower_case_names  no capital letter in a name"
  )
  expect_identical(out[length(out)], "18 PASS, 0 WARN, 0 FAIL, 0 OFF")
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
  i <- init_line(lines)
  lines[i] <- sub("\"v01\"", "\"v02\"", lines[i], fixed = TRUE)
  writeLines(lines, file.path(p$home, "v01_run.R"))
  found <- project_findings(p$home)
  expect_identical(failed_rules(found), "run_script_initialises")
})

test_that("run_script_initialises fails when the call is only in a comment", {
  p <- local_project()
  lines <- readLines(file.path(p$home, "v01_run.R"))
  i <- init_line(lines)
  lines[i] <- paste("#", lines[i])
  lines <- append(lines, "p <- list(data_raw = \".\")", after = i)
  writeLines(lines, file.path(p$home, "v01_run.R"))
  found <- project_findings(p$home)
  expect_identical(failed_rules(found), "run_script_initialises")
})

test_that("run_script_initialises reads a version given by position", {
  p <- local_project()
  lines <- readLines(file.path(p$home, "v01_run.R"))
  writeLines(
    c("pbn::initialize_project(\"v01\", \".\", \"x\")", lines[-1]),
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
  dir.create(file.path(p$home, "v01_results", "real"), recursive = TRUE)
  qs2::qs_save(
    valid_results(),
    file.path(p$home, "v01_results", "real", "results.qs2")
  )
  found <- project_findings(p$home)
  expect_identical(failed_rules(found), character(0))
  expect_identical(found$status[found$rule == "results_contract"], "PASS")
})

test_that("results_contract fails on a results file that fails check_results", {
  p <- local_project()
  dir.create(file.path(p$home, "v01_results", "real"), recursive = TRUE)
  x <- valid_results()
  x$exclusions$n_after[2] <- 41
  qs2::qs_save(x, file.path(p$home, "v01_results", "real", "results.qs2"))
  found <- project_findings(p$home)
  expect_identical(failed_rules(found), "results_contract")
  expect_match(
    found$detail[found$status == "FAIL"],
    "exclusions_table",
    fixed = TRUE
  )
})

test_that("results_contract fails on a results file directly in vNN_results/", {
  p <- local_project()
  dir.create(file.path(p$home, "v01_results"))
  qs2::qs_save(valid_results(), file.path(p$home, "v01_results", "results.qs2"))
  found <- project_findings(p$home)
  expect_identical(failed_rules(found), "results_contract")
  expect_identical(
    found$path[found$status == "FAIL"],
    "v01_results/results.qs2"
  )
  expect_match(
    found$detail[found$status == "FAIL"],
    "in the old place",
    fixed = TRUE
  )
})

test_that("results_contract fails on a results file without the scenario in its manifest", {
  p <- local_project()
  dir.create(file.path(p$home, "v01_results", "real"), recursive = TRUE)
  x <- valid_results()
  x$manifest$scenario <- NULL
  qs2::qs_save(x, file.path(p$home, "v01_results", "real", "results.qs2"))
  found <- project_findings(p$home)
  expect_identical(failed_rules(found), "results_contract")
  expect_match(
    found$detail[found$status == "FAIL"],
    "manifest_scenario",
    fixed = TRUE
  )
})

test_that("manual_manuscript_names fails on a name outside the pattern", {
  p <- local_project()
  # Not v01_ab.docx: where file names ignore case, as on Windows, that name
  # is the file v01_AB.docx of local_project().
  write_lines(p$home, "manuscript/manual/v01_cd.docx", "")
  found <- project_findings(p$home)
  expect_identical(failed_rules(found), "manual_manuscript_names")
  expect_identical(
    found$path[found$status == "FAIL"],
    "manuscript/manual/v01_cd.docx"
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

test_that("no_local_copy_of_pbn_function fails a local get_argset and passes the data function get_data of a project", {
  p <- local_project()
  # The project of local_project() defines get_data(scenario, sim).
  expect_true(any(grepl(
    "^get_data <- function",
    readLines(file.path(p$home, "v01_functions", "get_data.R"))
  )))
  expect_identical(failed_rules(project_findings(p$home)), character(0))
  write_lines(
    p$home,
    "v01_functions/step.R",
    "get_argset <- function(p) return(1)"
  )
  found <- project_findings(p$home)
  expect_identical(failed_rules(found), "no_local_copy_of_pbn_function")
  expect_match(
    found$detail[found$status == "FAIL"],
    "v01_functions/step.R: defines get_argset, a pbn export",
    fixed = TRUE
  )
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

# The lines of a function `name` with `ifs` branches. cyclocomp scores it
# ifs + 1.
branchy_function <- function(name, ifs) {
  return(c(
    paste0(name, " <- function(x) {"),
    "  y <- 0",
    paste0("  if (x == ", seq_len(ifs), ") y <- ", seq_len(ifs)),
    "  return(y)",
    "}"
  ))
}

test_that("cyclomatic_complexity passes a function that scores 15 and fails one that scores 16, with the file, the name and the score", {
  p <- local_project()
  write_lines(p$home, "v01_functions/branchy.R", branchy_function("fit_a", 14))
  expect_identical(
    function_scores(file.path(p$home, "v01_functions", "branchy.R")),
    c(fit_a = 15L)
  )
  expect_identical(failed_rules(project_findings(p$home)), character(0))

  write_lines(
    p$home,
    "v01_functions/branchy.R",
    c(branchy_function("fit_a", 15), branchy_function("fit_b", 20))
  )
  write_lines(p$home, "adhoc/more.R", branchy_function("tidy", 16))
  found <- project_findings(p$home)
  expect_identical(failed_rules(found), "cyclomatic_complexity")
  bad <- found[found$status == "FAIL", ]
  # Sorted by the file, then by the score.
  expect_identical(
    bad$detail,
    with_fix(
      "cyclomatic_complexity",
      c(
        "adhoc/more.R: tidy scores 17 (limit 15)",
        "v01_functions/branchy.R: fit_b scores 21 (limit 15)",
        "v01_functions/branchy.R: fit_a scores 16 (limit 15)"
      )
    )
  )
  expect_identical(
    bad$path,
    c("adhoc/more.R", "v01_functions/branchy.R", "v01_functions/branchy.R")
  )
})

test_that("cyclomatic_complexity scores a function in a top-level list, ignores top-level code, and counts a nested function toward its parent", {
  p <- local_project()
  write_lines(
    p$home,
    "v01_functions/zz_cfg.R",
    c(
      "helpers <- list(",
      "  size = 3,",
      "  pick = function(x) {",
      paste0("    if (x == ", 1:15, ") x <- 1"),
      "    return(x)",
      "  }",
      ")",
      "# Top-level code that is not a function is not scored.",
      paste0("if (x == ", 1:30, ") y <- 1"),
      "outer <- function(x) {",
      "  inner <- function(y) {",
      paste0("    if (y == ", 1:8, ") y <- 1"),
      "    return(y)",
      "  }",
      paste0("  if (x == ", 1:8, ") x <- inner(x)"),
      "  return(x)",
      "}"
    )
  )
  expect_identical(
    function_scores(file.path(p$home, "v01_functions", "zz_cfg.R")),
    c("helpers$pick" = 16L, outer = 17L)
  )
  found <- project_findings(p$home)
  expect_identical(
    found$detail[found$rule == "cyclomatic_complexity"],
    with_fix(
      "cyclomatic_complexity",
      c(
        "v01_functions/zz_cfg.R: outer scores 17 (limit 15)",
        "v01_functions/zz_cfg.R: helpers$pick scores 16 (limit 15)"
      )
    )
  )
})

test_that("cyclomatic_complexity skips a file that R cannot parse, which loc_per_file fails, and an allowed file warns", {
  p <- local_project()
  write_lines(p$home, "adhoc/broken.R", "f <- function( {")
  found <- project_findings(p$home)
  expect_identical(failed_rules(found), "loc_per_file")
  expect_identical(
    found$status[found$rule == "cyclomatic_complexity"],
    "PASS"
  )
  unlink(file.path(p$home, "adhoc", "broken.R"))

  write_lines(p$home, "adhoc/more.R", branchy_function("tidy", 16))
  write_pbn_yml(p$home, "allow:", "  cyclomatic_complexity: [adhoc/more.R]")
  found <- project_findings(p$home)
  expect_identical(failed_rules(found), character(0))
  row <- found[found$rule == "cyclomatic_complexity", ]
  expect_identical(row$status, "WARN")
  expect_identical(
    row$detail,
    with_fix(
      "cyclomatic_complexity",
      "adhoc/more.R: tidy scores 17 (limit 15) (allowed)"
    )
  )
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

test_that("no_local_assign fails each construct at top level and inside a function, with the file, the line and the construct", {
  p <- local_project()
  write_lines(
    p$home,
    "v01_functions/zz_bad.R",
    c(
      "a <- local({",
      "  1",
      "})",
      "assign(\"b\", 1)",
      "base::assign(\"c\", 1)",
      "base::local(1)",
      "delayedAssign(\"d\", 1)",
      "utils::assignInNamespace(\"e\", 1, \"stats\")",
      "assignInMyNamespace(\"f\", 1)",
      "g <<- 1",
      "1 ->> h",
      "nested <- function(x) {",
      "  y <- local(x)",
      "  assign(\"z\", x)",
      "  delayedAssign(\"w\", x)",
      "  i <<- x",
      "  x ->> j",
      "  return(y)",
      "}"
    )
  )
  write_lines(p$home, "adhoc/early.R", c("a <- 1", "assign(\"b\", a)"))
  found <- project_findings(p$home)
  expect_identical(failed_rules(found), "no_local_assign")
  expect_identical(
    found$detail[found$rule == "no_local_assign"],
    with_fix(
      "no_local_assign",
      c(
        "adhoc/early.R:2: assign()",
        paste0(
          "v01_functions/zz_bad.R:",
          c(1, 4, 5, 6, 7, 8, 9, 10, 11, 13, 14, 15, 16, 17),
          ": ",
          c(
            "local()",
            "assign()",
            "assign()",
            "local()",
            "delayedAssign()",
            "assignInNamespace()",
            "assignInMyNamespace()",
            "<<-",
            "->>",
            "local()",
            "assign()",
            "delayedAssign()",
            "<<-",
            "->>"
          )
        )
      )
    )
  )
  expect_identical(
    unique(found$path[found$rule == "no_local_assign"]),
    c("adhoc/early.R", "v01_functions/zz_bad.R")
  )
})

test_that("no_local_assign passes the words in a comment or a string, a variable or an argument named local or assign, <-, ->, =, := and a call from another package", {
  p <- local_project()
  write_lines(
    p$home,
    "v01_functions/zz_good.R",
    c(
      "# local() and assign() and <<- and ->> in a comment",
      "note <- \"local(1); assign('a', 1); a <<- 1; 1 ->> a\"",
      "local <- 1",
      "assign <- local + 1",
      "f <- function(x, local = TRUE, assign = FALSE) {",
      "  return(list(local = local, assign = assign))",
      "}",
      "y = f(1, local = FALSE)",
      "2 -> z",
      "dt <- data.table::data.table(a = 1)",
      "dt[, b := a]",
      "other::assign(\"a\", 1)",
      "other::local(1)",
      "assignment <- \"assign(\""
    )
  )
  found <- project_findings(p$home)
  expect_identical(failed_rules(found), character(0))
  expect_identical(found$status[found$rule == "no_local_assign"], "PASS")
})

test_that("no_local_assign gives the right lines in a file with CR LF line ends", {
  p <- local_project()
  con <- file(file.path(p$home, "adhoc_crlf.R"), open = "wb")
  writeBin(
    charToRaw(paste0(
      "x <- 1\r\n",
      "\r\n",
      "f <- function() {\r\n",
      "  # assign() in a comment\r\n",
      "  assign(\"a\", 1)\r\n",
      "}\r\n",
      "y <<- 2\r\n"
    )),
    con
  )
  close(con)
  expect_identical(
    local_assign_hits(file.path(p$home, "adhoc_crlf.R")),
    data.frame(line = c(5L, 7L), construct = c("assign()", "<<-"))
  )
  found <- project_findings(p$home)
  expect_identical(
    found$detail[found$rule == "no_local_assign"],
    with_fix(
      "no_local_assign",
      c("adhoc_crlf.R:5: assign()", "adhoc_crlf.R:7: <<-")
    )
  )
})

test_that("no_local_assign skips a file that R cannot parse, which loc_per_file fails, and an allowed file warns", {
  p <- local_project()
  write_lines(
    p$home,
    "adhoc/broken.R",
    c("assign(\"a\", 1)", "f <- function( {")
  )
  found <- project_findings(p$home)
  expect_identical(failed_rules(found), "loc_per_file")
  expect_identical(found$status[found$rule == "no_local_assign"], "PASS")
  unlink(file.path(p$home, "adhoc", "broken.R"))

  write_lines(
    p$home,
    "adhoc/more.R",
    c("a <- 1", "assign(\"b\", 2)", "c <<- 3")
  )
  write_pbn_yml(p$home, "allow:", "  no_local_assign: [adhoc/more.R]")
  found <- project_findings(p$home)
  expect_identical(failed_rules(found), character(0))
  row <- found[found$rule == "no_local_assign", ]
  expect_identical(row$status, c("WARN", "WARN"))
  expect_identical(
    row$detail,
    with_fix(
      "no_local_assign",
      c("adhoc/more.R:2: assign() (allowed)", "adhoc/more.R:3: <<- (allowed)")
    )
  )
  expect_false(any(found$rule == "allow"))
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
  write_pbn_yml(p$home, "allow:", "  lower_case_names: [adhoc/Notes.txt]")
  found <- project_findings(p$home)
  expect_identical(failed_rules(found), character(0))
  row <- found[found$rule == "lower_case_names", ]
  expect_identical(row$status, "WARN")
  expect_identical(
    row$detail,
    with_fix(
      "lower_case_names",
      "adhoc/Notes.txt: the name has a capital letter (allowed)"
    )
  )
  expect_false("allow" %in% found$rule)
})

test_that("a stale allowlist entry is a WARN", {
  p <- local_project()
  write_pbn_yml(p$home, "allow:", "  loc_per_file: [adhoc/gone.R]")
  found <- project_findings(p$home)
  expect_identical(failed_rules(found), character(0))
  stale <- found[found$rule == "allow", ]
  expect_identical(stale$status, "WARN")
  expect_identical(stale$path, "pbn.yml")
  expect_identical(
    stale$detail,
    with_fix(
      "allow",
      "pbn.yml: the entry allow: loc_per_file: adhoc/gone.R matches no finding"
    )
  )
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
  expect_match(
    ok$stdout,
    "Rules: house_2026_11_17, 0 overrides, 0 allowed paths (pbn.yml)",
    fixed = TRUE
  )
  expect_match(ok$stdout, "18 PASS, 0 WARN, 0 FAIL, 0 OFF", fixed = TRUE)

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
  expect_match(bad$stdout, "17 PASS, 0 WARN, 1 FAIL, 0 OFF", fixed = TRUE)
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

test_that("unknown_path knows the check scripts and pbn.yml, and warns on run_scenarios.R", {
  p <- local_project()
  write_lines(p$home, "v01_check_functions.R", "x <- 1")
  expect_true(file.exists(file.path(p$home, "pbn.yml")))

  found <- project_findings(p$home)
  expect_identical(found$rule, project_rule_names)
  expect_identical(found$status, rep("PASS", length(project_rule_names)))

  # pbn 2026.11.23 has no run_scenarios(), so the layout no longer names
  # the script that called it.
  write_lines(p$home, "run_scenarios.R", "pbn::run_scenarios(\"v01\")")
  found <- project_findings(p$home)
  expect_identical(failed_rules(found), character(0))
  expect_identical(found$path[found$rule == "unknown_path"], "run_scenarios.R")
  expect_identical(found$status[found$rule == "unknown_path"], "WARN")
})

test_that("unknown_path warns on vNN_check_scenarios.R", {
  p <- local_project()
  write_lines(p$home, "v01_check_scenarios.R", "x <- 1")
  found <- project_findings(p$home)
  expect_identical(failed_rules(found), character(0))
  expect_identical(found$status[found$rule == "unknown_path"], "WARN")
  expect_identical(
    found$path[found$rule == "unknown_path"],
    "v01_check_scenarios.R"
  )
})

test_that("results_contract checks the file of each simulation", {
  p <- local_project()
  folder <- file.path(p$home, "v01_results", "null", "sim_0002")
  dir.create(folder, recursive = TRUE)
  x <- valid_results()
  x$manifest$scenario <- "null"
  x$manifest$data_files <- NULL
  x$manifest$fake_md5 <- "fedcba9876543210fedcba9876543210"
  x$manifest$sim <- 2L
  x$manifest$sims <- 3L
  qs2::qs_save(x, file.path(folder, "results.qs2"))
  expect_identical(failed_rules(project_findings(p$home)), character(0))

  x$exclusions$n_after[2] <- 41
  qs2::qs_save(x, file.path(folder, "results.qs2"))
  found <- project_findings(p$home)
  expect_identical(failed_rules(found), "results_contract")
  expect_identical(
    found$path[found$status == "FAIL"],
    "v01_results/null/sim_0002/results.qs2"
  )
  expect_match(
    found$detail[found$status == "FAIL"],
    "exclusions_table",
    fixed = TRUE
  )
})

test_that("results_contract checks each file of sims_gitexclude/ that git ignores, and needs none in a clone", {
  p <- local_project()
  git_in(p$home, "init", "-q")
  write_lines(p$home, ".gitignore", "*gitexclude*")
  folder <- file.path(p$home, "v01_results", "null")
  dir.create(file.path(folder, "sims_gitexclude"), recursive = TRUE)
  x <- valid_results()
  x$manifest$scenario <- "null"
  x$manifest$data_files <- NULL
  x$manifest$fake_md5 <- "fedcba9876543210fedcba9876543210"
  x$manifest$sims <- 3L
  qs2::qs_save(x, file.path(folder, "results.qs2"))
  x$manifest$sim <- 2L
  sim_file <- file.path(folder, "sims_gitexclude", "sim_0002.qs2")
  qs2::qs_save(x, sim_file)
  rule <- function() {
    found <- project_findings(p$home)
    return(found[found$rule == "results_contract", ])
  }
  expect_identical(rule()$status, "PASS")

  x$exclusions$n_after[2] <- 41
  qs2::qs_save(x, sim_file)
  found <- rule()
  expect_identical(found$status, "FAIL")
  expect_identical(found$path, "v01_results/null/sims_gitexclude/sim_0002.qs2")
  expect_match(found$detail, "exclusions_table", fixed = TRUE)

  # The file of the kinds of gitexclude beside it MUST hold the kinds that
  # its manifest names.
  x$exclusions$n_after[2] <- 40
  qs2::qs_save(x, sim_file)
  qs2::qs_save(
    list(models = 1),
    file.path(folder, "sims_gitexclude", "sim_0002_gitexclude.qs2")
  )
  expect_match(
    rule()$detail,
    "the manifest of sim_0002.qs2 names no analysis kind in sim_0002_gitexclude.qs2",
    fixed = TRUE
  )

  found <- project_findings(p$home)
  expect_identical(found$status[found$rule == "unknown_path"], "PASS")

  # A clone has no folder sims_gitexclude/.
  unlink(file.path(folder, "sims_gitexclude"), recursive = TRUE)
  expect_identical(rule()$status, "PASS")
})

write_check_csv <- function(home, scenario, pass) {
  folder <- file.path(home, "v01_results", scenario)
  dir.create(folder, recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(
    data.frame(
      assertion = paste("assertion", seq_along(pass)),
      expected = 1,
      observed = 1,
      pass = pass
    ),
    file.path(folder, "check.csv"),
    row.names = FALSE
  )
  return(invisible(NULL))
}

test_that("scenario_checks passes a check.csv where every pass is TRUE", {
  p <- local_project()
  write_check_csv(p$home, "null", c(TRUE, TRUE))
  found <- project_findings(p$home)
  expect_identical(failed_rules(found), character(0))
  expect_identical(found$status[found$rule == "scenario_checks"], "PASS")
})

test_that("scenario_checks fails on a check.csv with a pass that is not TRUE", {
  for (bad in list(c(TRUE, FALSE), c(TRUE, NA))) {
    p <- local_project()
    write_check_csv(p$home, "null", bad)
    found <- project_findings(p$home)
    expect_identical(failed_rules(found), "scenario_checks")
    expect_identical(
      found$path[found$status == "FAIL"],
      "v01_results/null/check.csv"
    )
    expect_match(
      found$detail[found$status == "FAIL"],
      "these assertions failed: assertion 2",
      fixed = TRUE
    )
  }
})

test_that("scenario_checks fails on a check.csv without a pass column", {
  p <- local_project()
  write_lines(p$home, "v01_results/null/check.csv", c("assertion", "a"))
  found <- project_findings(p$home)
  expect_identical(failed_rules(found), "scenario_checks")
})

test_that("scenario_checks fails on a check.csv with a header and no assertion", {
  p <- local_project()
  write_lines(
    p$home,
    "v01_results/null/check.csv",
    "\"assertion\",\"expected\",\"observed\",\"pass\""
  )
  found <- project_findings(p$home)
  expect_identical(failed_rules(found), "scenario_checks")
  expect_identical(
    found$detail[found$status == "FAIL"],
    with_fix(
      "scenario_checks",
      "v01_results/null/check.csv: it holds no assertion"
    )
  )
})

test_that("manuscript_reads_real passes a call without scenario or with \"real\"", {
  p <- local_project()
  write_lines(
    p$home,
    "manuscript/built/v01/tables.R",
    c(
      "a <- pbn::read_results(\"v01\")",
      "b <- read_results(\"v01\", scenario = \"real\")",
      "c <- pbn::read_results(\"v01\", \"real\")",
      "# pbn::read_results(\"v01\", scenario = \"null\")"
    )
  )
  write_lines(
    p$home,
    "manuscript/built/v01/paper.qmd",
    c(
      "There are `r nrow(pbn::read_results(\"v01\")$estimates)` rows.",
      "",
      "```{r}",
      "x <- pbn::read_results(\"v01\", scenario = \"real\")",
      "```"
    )
  )
  found <- project_findings(p$home)
  expect_identical(failed_rules(found), character(0))
  expect_identical(found$status[found$rule == "manuscript_reads_real"], "PASS")
})

test_that("manuscript_reads_real fails on another scenario in an .R file", {
  calls <- c(
    "x <- pbn::read_results(\"v01\", scenario = \"null\")",
    "x <- read_results(\"v01\", \"known\")",
    "x <- pbn:::read_results(\"v01\", scenario = \"null\")",
    "s <- \"real\"; x <- pbn::read_results(\"v01\", scenario = s)",
    "f <- function() return(pbn::read_results(\"v01\", sim = 1, scenario = \"null\"))"
  )
  for (cl in calls) {
    p <- local_project()
    write_lines(p$home, "manuscript/built/v01/tables.R", cl)
    found <- project_findings(p$home)
    expect_identical(failed_rules(found), "manuscript_reads_real", info = cl)
    expect_identical(
      found$path[found$status == "FAIL"],
      "manuscript/built/v01/tables.R",
      info = cl
    )
  }
})

test_that("manuscript_reads_real fails on another scenario in a .qmd chunk or inline code", {
  docs <- list(
    chunk = c(
      "```{r}",
      "x <- pbn::read_results(\"v01\", scenario = \"null\")",
      "```"
    ),
    options = c(
      "```{r}",
      "#| label: tbl-one",
      "x <- read_results(\"v01\", scenario = \"known\")",
      "```"
    ),
    inline = "The mean is `r pbn::read_results(\"v01\", scenario = \"null\")$estimates`.",
    quarto_inline = "The mean is `{r} read_results(\"v01\", scenario = \"null\")`."
  )
  for (nm in names(docs)) {
    p <- local_project()
    write_lines(p$home, "manuscript/built/v01/paper.qmd", docs[[nm]])
    found <- project_findings(p$home)
    expect_identical(failed_rules(found), "manuscript_reads_real", info = nm)
    expect_match(
      found$detail[found$status == "FAIL"],
      "scenario = ",
      fixed = TRUE,
      info = nm
    )
  }
})

test_that("manuscript_reads_real fails on an .R file that R cannot parse", {
  p <- local_project()
  write_lines(p$home, "manuscript/built/v01/tables.R", "x <- (")
  found <- project_findings(p$home)
  # loc_per_file cannot parse it either.
  expect_setequal(
    failed_rules(found),
    c("manuscript_reads_real", "loc_per_file")
  )
  expect_identical(
    found$detail[found$rule == "manuscript_reads_real"],
    with_fix(
      "manuscript_reads_real",
      "manuscript/built/v01/tables.R: R cannot parse it",
      parse_fix
    )
  )

  write_lines(
    p$home,
    "manuscript/built/v01/paper.qmd",
    c("```{r}", "x <- (", "```")
  )
  write_lines(p$home, "manuscript/built/v01/tables.R", "x <- 1")
  found <- project_findings(p$home)
  expect_identical(failed_rules(found), "manuscript_reads_real")
  expect_identical(
    found$path[found$status == "FAIL"],
    "manuscript/built/v01/paper.qmd"
  )
})

test_that("run_script_initialises fails on a call with an argument initialize_project does not have", {
  p <- local_project()
  lines <- readLines(file.path(p$home, "v01_run.R"))
  i <- init_line(lines)
  lines[i] <- sub("home = ", "scenario = \"real\", home = ", lines[i])
  writeLines(lines, file.path(p$home, "v01_run.R"))
  found <- project_findings(p$home)
  expect_identical(failed_rules(found), "run_script_initialises")
  expect_identical(
    found$detail[found$status == "FAIL"],
    with_fix(
      "run_script_initialises",
      paste0(
        "v01_run.R: no call pbn::initialize_project(version = \"v01\"). ",
        "pbn::initialize_project() has no argument scenario"
      )
    )
  )
})

test_that("unknown_path warns on vNN_output.R", {
  p <- local_project()
  write_lines(p$home, "v01_output.R", "x <- 1")
  found <- project_findings(p$home)
  expect_identical(failed_rules(found), character(0))
  expect_identical(found$status[found$rule == "unknown_path"], "WARN")
  expect_identical(found$path[found$rule == "unknown_path"], "v01_output.R")
})

test_that("fake_data_names passes the output folders of make_outputs and fails a name in the wrong folder", {
  p <- local_project()
  write_lines(p$home, "v01_results/real/output/figure.png", "")
  write_lines(
    p$home,
    "v01_results/known/output_fake_data/fake_data_figure.png",
    ""
  )
  write_lines(
    p$home,
    "v01_results/known/output_fake_data/tables/fake_data_table.csv",
    ""
  )
  found <- project_findings(p$home)
  expect_identical(failed_rules(found), character(0))
  expect_identical(found$status[found$rule == "fake_data_names"], "PASS")
  # unknown_path reads the top level only, so it gives no WARN on them.
  expect_identical(found$status[found$rule == "unknown_path"], "PASS")

  write_lines(p$home, "v01_results/known/output_fake_data/figure.png", "")
  write_lines(
    p$home,
    "v01_results/known/output_fake_data/tables/table.csv",
    ""
  )
  write_lines(p$home, "v01_results/real/output/fake_data_figure.png", "")
  write_lines(p$home, "manuscript/built/v01/fake_data_numbers.txt", "")
  found <- project_findings(p$home)
  expect_identical(failed_rules(found), "fake_data_names")
  failed <- found[found$status == "FAIL", ]
  expect_identical(
    failed$path,
    c(
      "v01_results/known/output_fake_data/figure.png",
      "v01_results/known/output_fake_data/tables/table.csv",
      "manuscript/built/v01/fake_data_numbers.txt",
      "v01_results/real/output/fake_data_figure.png"
    )
  )
  expect_match(failed$detail[1], "its name MUST start with fake_data_")
  expect_match(
    failed$detail[4],
    "fake_data_ is for a file in a folder output_fake_data/ only"
  )
})

test_that("each WARN and FAIL of check_project ends with the fix of its rule, and the rules, levels and columns stay", {
  fixes <- c(
    lower_case_names = "rename it in lower case",
    run_script_initialises = "call pbn::initialize_project(version = \"vNN\") in it",
    version_items_complete = "create the missing vNN_run.R or vNN_functions/, or delete the one that is there",
    frozen_versions_unchanged = "put the files of the version back as they were and make the change in a new version, or run pbn::freeze_version() again to record the change as intended",
    readme_and_changelog = "create the missing file, or add the missing section ## vNN to CHANGELOG.md",
    changelog_with_code = "add what changed and why to CHANGELOG.md, under the section of the version",
    results_contract = "delete the file, and write it again with pbn::run_plan() and pbn::save_results()",
    gitexclude_not_tracked = "remove the path from git with git rm --cached, and add the line *gitexclude* to .gitignore",
    scenario_checks = "correct the analysis, and run the scenario again with pbn::run_plan() and pbn::save_results()",
    manual_manuscript_names = "rename it to vNN_<INITIALS>.docx, for example v01_AB.docx, or vNN_<INITIALS>_<suffix>.docx, for example v01_AB_si.docx",
    manuscript_reads_real = "remove the argument scenario from the call",
    built_reads_own_version = "read the version of the folder, as in read_results(\"vNN\"), or move the file to the folder of the version that it reads",
    built_has_run = "move the folder to the version of the run that it reads, or create the run script of the version",
    manual_not_ahead = "start the next run version with the call of pbn::new_version() in the detail",
    fake_data_names = "write the output of fake data with pbn::make_outputs(), and rename any other file whose name starts with fake_data_",
    no_local_copy_of_pbn_function = "rename the function, or use the pbn export and correct any fault in pbn",
    loc_per_file = "split the file into smaller files",
    cyclomatic_complexity = "split the function into smaller functions",
    comment_share = "move the explanation to README.md (why) or CHANGELOG.md (what changed)",
    no_environment_variables = "put the value in CFG, and the folder of data in the argument folders of pbn::initialize_project()",
    no_cross_version_reads = "move the shared code into the functions folder of this version",
    no_local_assign = "return the value from a function, and assign it with <- where it is used",
    no_escape_hatches = "remove the call and let the warning or the error show, or allow the path in pbn.yml with a reason in a comment",
    no_absolute_paths = "start a path to data from pbn::folder(), and a path for output from the argument dir of the output function of pbn::make_outputs()",
    no_plot_title = "remove the title, because the legend of a figure belongs in the manuscript, or allow the path in pbn.yml",
    no_plot_subtitle = "remove the subtitle, because the legend of a figure belongs in the manuscript, or allow the path in pbn.yml",
    no_plot_caption = "remove the caption, because the legend of a figure belongs in the manuscript, or allow the path in pbn.yml",
    blocks_list = "keep the list blocks and the functions of the blocks in vNN_functions/blocks.R only, and use none of its names in another file",
    real_results_are_fake = "run the scenario \"real\" again with pbn::initialize_project(fake_real_data = FALSE)",
    guidelines_valid = "correct the file to the checklist schema of ?check_project",
    guidelines_covered = "answer the item in the manuscript",
    unknown_path = "move it into a folder of the layout, such as adhoc/, or delete it"
  )
  expect_identical(
    vapply(project_rules(), function(d) return(d[[3]]), character(1)),
    fixes
  )
  expect_identical(parse_fix, "correct the syntax of the file")

  p <- local_project()
  clean <- project_findings(p$home)
  write_lines(
    p$home,
    "v01_functions/paths.R",
    "read_d <- function() return(utils::read.csv(\"/home/ana/project/d.csv\"))"
  )
  write_lines(p$home, "adhoc/broken.R", "f <- function( {")
  write_lines(p$home, "notes.txt", "x")
  write_pbn_yml(p$home, "allow:", "  lower_case_names: [adhoc/Gone.txt]")
  out <- project_report(p$home)
  expect_identical(
    grep("^(WARN|FAIL)", out, value = TRUE),
    c(
      paste0(
        "FAIL  loc_per_file  adhoc/broken.R: R cannot parse it. ",
        "Fix: correct the syntax of the file."
      ),
      paste0(
        "FAIL  no_absolute_paths  v01_functions/paths.R:1: ",
        "\"/home/ana/project/d.csv\". Fix: start a path to data from ",
        "pbn::folder(), and a path for output from the argument dir of the ",
        "output function of pbn::make_outputs()."
      ),
      paste0(
        "WARN  unknown_path  notes.txt: the house layout does not name it. ",
        "Fix: move it into a folder of the layout, such as adhoc/, or ",
        "delete it."
      ),
      paste0(
        "WARN  allow  pbn.yml: the entry allow: lower_case_names: ",
        "adhoc/Gone.txt matches no finding. Fix: remove the entry from ",
        "pbn.yml."
      )
    )
  )
  # The fix changes the text only: the rules, their order, the columns and
  # each status are those without it.
  found <- attr(out, "findings")
  expect_identical(names(found), c("rule", "status", "path", "detail"))
  expect_identical(unique(found$rule), c(project_rule_names, "allow"))
  expect_identical(unique(clean$rule), project_rule_names)
  expect_identical(
    found$status[found$rule %in% c("no_absolute_paths", "loc_per_file")],
    c("FAIL", "FAIL")
  )
  expect_identical(
    clean$detail[clean$status == "PASS"][1],
    "no capital letter in a name"
  )
})
