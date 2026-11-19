# The set house_2026_11_19: the rules frozen_versions_unchanged,
# no_cross_version_reads and no_escape_hatches, freeze_version() and
# verify(). The rules readme_and_changelog, changelog_with_code and
# comment_share are in test-rules_changelog.R. The projects of
# house_2026_11_17 see none of it.

# A synthetic project of local_project() on the set house_2026_11_19.
local_project_19 <- function(env = parent.frame()) {
  p <- local_project(env = env)
  write_lines(p$home, "pbn.yml", "extends: house_2026_11_19")
  return(p)
}

# The error of read_rules() on the project in `home`, whose pbn.yml then
# holds `lines`.
yml_error <- function(home, lines) {
  write_lines(home, "pbn.yml", lines)
  err <- tryCatch(read_rules(home), error = function(e) return(e))
  if (!inherits(err, "error")) {
    stop("read_rules() did not stop")
  }
  return(conditionMessage(err))
}

# The findings of the rule `rule` in the project in `home`.
rule_rows <- function(home, rule) {
  found <- project_findings(home)
  return(found[found$rule == rule, c("status", "path", "detail")])
}

# A second version v02 of the project in `home`, with the run script
# `run` after its initialize_project() line and the functions file
# v02_functions/f.R with the lines `fun`.
add_v02 <- function(
  home,
  run = character(0),
  fun = "f <- function() return(1)"
) {
  # Each version but the newest MUST be frozen.
  freeze_version("v01", home)
  write_lines(
    home,
    "v02_run.R",
    c(
      "pbn::initialize_project(version = \"v02\", home = \".\", folders = c(main = \"x\"))",
      run
    )
  )
  write_lines(home, "v02_functions/f.R", fun)
  cat("\n## v02\n", file = file.path(home, "CHANGELOG.md"), append = TRUE)
  return(invisible(home))
}

test_that("house_2026_11_19 is house_2026_11_17 with six new rules, at their levels", {
  old <- read_house_set("house_2026_11_17")
  new <- read_house_set("house_2026_11_19")
  expect_identical(new[names(old)], old)
  expect_identical(
    vapply(new[rules_2026_11_19], function(r) return(r$level), character(1)),
    c(
      frozen_versions_unchanged = "fail",
      readme_and_changelog = "fail",
      changelog_with_code = "warn",
      comment_share = "warn",
      no_cross_version_reads = "fail",
      no_escape_hatches = "warn"
    )
  )
  expect_identical(new$comment_share$limit, 0.25)
  expect_false(any(vapply(
    new[rules_2026_11_19],
    function(r) return(r$locked),
    logical(1)
  )))
  expect_identical(starter_line(), "extends: house_2026_11_19")
})

test_that("a project on house_2026_11_17 runs none of the new rules, and its pbn.yml cannot name one", {
  p <- local_project()
  write_lines(
    p$home,
    "v01_functions/bad.R",
    c(
      "a <- tryCatch(1, error = function(e) 2)",
      "b <- \"../v02_results/real\""
    )
  )
  writeLines("x", file.path(p$home, "v01_frozen.md5"))
  found <- project_findings(p$home)
  expect_identical(
    unique(found$rule),
    setdiff(names(project_rules()), rules_2026_11_19)
  )
  expect_false(any(rules_2026_11_19 %in% found$rule))
  expect_identical(
    found$detail[found$rule == "unknown_path"],
    with_fix(
      "unknown_path",
      "v01_frozen.md5: the house layout does not name it"
    )
  )
  want <- paste0(
    " is not a rule of the set house_2026_11_17. The oldest set with it is ",
    "house_2026_11_19. To use it, change extends: to that set or a newer one."
  )
  expect_identical(
    yml_error(p$home, c(house_line, "rules:", "  no_escape_hatches: off")),
    paste0("pbn.yml: rules: no_escape_hatches", want)
  )
  expect_identical(
    yml_error(
      p$home,
      c(house_line, "allow:", "  no_cross_version_reads: [adhoc]")
    ),
    paste0("pbn.yml: allow: no_cross_version_reads", want)
  )
})

test_that("no_local_copy_of_pbn_function fails a local verify() on house_2026_11_19 and passes it on house_2026_11_17", {
  p <- local_project()
  write_lines(
    p$home,
    "v01_functions/verify.R",
    c(
      "verify <- function(x) return(x)",
      "freeze_version <- function(x) return(x)"
    )
  )
  expect_identical(failed_rules(project_findings(p$home)), character(0))
  write_lines(p$home, "pbn.yml", "extends: house_2026_11_19")
  expect_identical(
    rule_rows(p$home, "no_local_copy_of_pbn_function")$detail,
    with_fix(
      "no_local_copy_of_pbn_function",
      "v01_functions/verify.R: defines verify, freeze_version, a pbn export"
    )
  )
})

test_that("freeze_version writes the md5 of the run script and of each file under the functions folder, sorted, in the format of md5sum", {
  p <- local_project_19()
  write_lines(p$home, "v01_functions/sub/deep.txt", "x")
  write_lines(p$home, "v01_functions/.hidden", "x")
  write_lines(p$home, "v02_functions/other.R", "x")
  expect_identical(freeze_version("v01", p$home), "v01_frozen.md5")
  lines <- readLines(file.path(p$home, "v01_frozen.md5"))
  paths <- c(
    "v01_functions/.hidden",
    "v01_functions/_cfg.R",
    "v01_functions/analyses.R",
    "v01_functions/double_it.R",
    "v01_functions/expect_known.R",
    "v01_functions/expect_null.R",
    "v01_functions/get_data.R",
    "v01_functions/sub/deep.txt",
    "v01_run.R"
  )
  expect_identical(
    lines,
    paste0(unname(tools::md5sum(file.path(p$home, paths))), "  ", paths)
  )
  bytes <- readBin(file.path(p$home, "v01_frozen.md5"), "raw", 10000)
  expect_false(as.raw(13) %in% bytes)
  expect_error(
    freeze_version("v02", p$home),
    "freeze_version() needs v02_run.R and v02_functions/ in ",
    fixed = TRUE
  )
  expect_error(freeze_version("1", p$home), "`version` must be")
})

test_that("frozen_versions_unchanged passes a frozen version without a change, and a change of another version", {
  p <- local_project_19()
  expect_identical(
    rule_rows(p$home, "frozen_versions_unchanged")$detail,
    paste(
      "every version but the newest has a vNN_frozen.md5, and no file of a",
      "frozen version changed after pbn::freeze_version()"
    )
  )
  add_v02(p$home)
  write_lines(p$home, "v02_functions/g.R", "g <- function() return(2)")
  write_lines(p$home, "adhoc/notes.R", "x <- 1")
  found <- project_findings(p$home)
  expect_identical(failed_rules(found), character(0))
  expect_identical(
    found[found$rule == "frozen_versions_unchanged", "status"],
    "PASS"
  )
  # The file of freeze_version() is in the layout of house_2026_11_19.
  expect_identical(found[found$rule == "unknown_path", "status"], "PASS")
})

test_that("frozen_versions_unchanged fails each version but the newest without its vNN_frozen.md5, also after a delete, with the fix to freeze it", {
  p <- local_project_19()
  add_v02(p$home)
  write_lines(
    p$home,
    "v03_run.R",
    "pbn::initialize_project(version = \"v03\", home = \".\", folders = c(main = \"x\"))"
  )
  write_lines(p$home, "v03_functions/f.R", "f <- function() return(3)")
  cat("\n## v03\n", file = file.path(p$home, "CHANGELOG.md"), append = TRUE)
  # An agent deletes the file of v01 to get rid of a FAIL.
  unlink(file.path(p$home, "v01_frozen.md5"))
  found <- project_findings(p$home)
  expect_identical(failed_rules(found), "frozen_versions_unchanged")
  rows <- found[found$rule == "frozen_versions_unchanged", ]
  expect_identical(rows$path, c("v01_frozen.md5", "v02_frozen.md5"))
  expect_identical(
    rows$detail,
    c(
      paste0(
        "v01_frozen.md5: missing, and v01 is older than the newest version ",
        "v03. Fix: run pbn::freeze_version(\"v01\"), or restore ",
        "v01_frozen.md5 with git if it was deleted."
      ),
      paste0(
        "v02_frozen.md5: missing, and v02 is older than the newest version ",
        "v03. Fix: run pbn::freeze_version(\"v02\"), or restore ",
        "v02_frozen.md5 with git if it was deleted."
      )
    )
  )
  # The newest version needs no file.
  freeze_version("v01", p$home)
  freeze_version("v02", p$home)
  found <- project_findings(p$home)
  expect_identical(failed_rules(found), character(0))
  expect_false(file.exists(file.path(p$home, "v03_frozen.md5")))
})

test_that("frozen_versions_unchanged fails a changed, an added and a removed file of a frozen version, and names each file", {
  p <- local_project_19()
  freeze_version("v01", p$home)
  cat("x <- 2\n", file = file.path(p$home, "v01_run.R"), append = TRUE)
  write_lines(p$home, "v01_functions/new.R", "n <- function() return(1)")
  unlink(file.path(p$home, "v01_functions", "double_it.R"))
  found <- project_findings(p$home)
  expect_identical(failed_rules(found), "frozen_versions_unchanged")
  rows <- found[found$rule == "frozen_versions_unchanged", ]
  expect_identical(
    rows$path,
    c("v01_functions/double_it.R", "v01_functions/new.R", "v01_run.R")
  )
  expect_identical(
    rows$detail,
    with_fix(
      "frozen_versions_unchanged",
      paste0(
        c(
          "v01_functions/double_it.R: removed",
          "v01_functions/new.R: added",
          "v01_run.R: changed"
        ),
        " after pbn::freeze_version() wrote v01_frozen.md5"
      )
    )
  )
  expect_message(
    freeze_version("v01", p$home),
    paste0(
      "v01_frozen.md5 now records these changes as intended:\n",
      "  v01_functions/double_it.R: removed\n",
      "  v01_functions/new.R: added\n",
      "  v01_run.R: changed"
    ),
    fixed = TRUE
  )
  expect_identical(failed_rules(project_findings(p$home)), character(0))

  writeLines(
    c(readLines(file.path(p$home, "v01_frozen.md5")), "not a line"),
    file.path(p$home, "v01_frozen.md5")
  )
  expect_identical(
    rule_rows(p$home, "frozen_versions_unchanged")$detail,
    with_fix(
      "frozen_versions_unchanged",
      "v01_frozen.md5: line 8 is not \"<md5>  <path>\" of pbn::freeze_version()"
    )
  )
})

# Writes the lines of the file `file` again, each with the line end `eol`.
write_line_ends <- function(file, eol) {
  lines <- readLines(file)
  writeBin(charToRaw(paste0(lines, eol, collapse = "")), file)
  return(invisible(file))
}

test_that("frozen_versions_unchanged passes a frozen .R file with CR LF line ends and the same text, and fails a change of the text", {
  p <- local_project_19()
  files <- file.path(p$home, c("v01_run.R", "v01_functions/_cfg.R"))
  for (f in files) {
    write_line_ends(f, "\n")
  }
  freeze_version("v01", p$home)
  frozen <- readLines(file.path(p$home, "v01_frozen.md5"))
  before <- md5_of(files)
  for (f in files) {
    write_line_ends(f, "\r\n")
  }
  # The bytes of each file changed.
  expect_false(any(md5_of(files) == before))
  expect_identical(
    rule_rows(p$home, "frozen_versions_unchanged")$status,
    "PASS"
  )
  # The md5 of a file with CR LF line ends is that of the same text with LF.
  expect_silent(freeze_version("v01", p$home))
  expect_identical(readLines(file.path(p$home, "v01_frozen.md5")), frozen)

  cat("x <- 2\r\n", file = file.path(p$home, "v01_run.R"), append = TRUE)
  expect_identical(
    rule_rows(p$home, "frozen_versions_unchanged")$detail,
    with_fix(
      "frozen_versions_unchanged",
      "v01_run.R: changed after pbn::freeze_version() wrote v01_frozen.md5"
    )
  )
})

test_that("frozen_versions_unchanged hashes each file that is not .R as its bytes, so CR LF line ends in a .csv or a .txt fail", {
  p <- local_project_19()
  files <- c("v01_functions/lookup.csv", "v01_functions/notes.txt")
  write_lines(p$home, files[1], c("code,label", "1,a", "2,b"))
  write_lines(p$home, files[2], c("first line", "second line"))
  for (f in file.path(p$home, files)) {
    write_line_ends(f, "\n")
  }
  freeze_version("v01", p$home)
  expect_identical(
    rule_rows(p$home, "frozen_versions_unchanged")$status,
    "PASS"
  )
  for (f in file.path(p$home, files)) {
    write_line_ends(f, "\r\n")
  }
  expect_identical(
    rule_rows(p$home, "frozen_versions_unchanged")$detail,
    with_fix(
      "frozen_versions_unchanged",
      paste0(
        files,
        ": changed after pbn::freeze_version() wrote v01_frozen.md5"
      )
    )
  )
})

test_that("no_escape_hatches warns on each call in a run script and a functions folder, with the file and the line", {
  p <- local_project_19()
  write_lines(
    p$home,
    "v01_functions/hatches.R",
    c(
      "f <- function(x) {",
      "  y <- suppressWarnings(as.numeric(x))",
      "  base::suppressMessages(message(\"a\"))",
      "  z <- try(log(-1), silent = TRUE)",
      "  `tryCatch`(stop(\"b\"), error = function(e) return(NULL))",
      "  withCallingHandlers(warning(\"c\"), warning = function(w) invokeRestart(\"muffleWarning\"))",
      "  return(y)",
      "}"
    )
  )
  cat(
    "r <- tryCatch(1, error = function(e) 2)\n",
    file = file.path(p$home, "v01_run.R"),
    append = TRUE
  )
  found <- project_findings(p$home)
  expect_identical(failed_rules(found), character(0))
  rows <- found[found$rule == "no_escape_hatches", ]
  expect_identical(rows$status, rep("WARN", 6))
  n <- length(readLines(file.path(p$home, "v01_run.R")))
  expect_identical(
    rows$detail,
    with_fix(
      "no_escape_hatches",
      c(
        "v01_functions/hatches.R:2: suppressWarnings()",
        "v01_functions/hatches.R:3: suppressMessages()",
        "v01_functions/hatches.R:4: try()",
        "v01_functions/hatches.R:5: tryCatch()",
        "v01_functions/hatches.R:6: withCallingHandlers()",
        paste0("v01_run.R:", n, ": tryCatch()")
      )
    )
  )
  write_lines(
    p$home,
    "pbn.yml",
    c(
      "extends: house_2026_11_19",
      "allow:",
      "  # The run script needs tryCatch() for a reason that pbn.yml states.",
      "  no_escape_hatches: [v01_run.R]"
    )
  )
  rows <- rule_rows(p$home, "no_escape_hatches")
  expect_identical(
    rows$detail[rows$path == "v01_run.R"],
    with_fix(
      "no_escape_hatches",
      paste0("v01_run.R:", n, ": tryCatch() (allowed)")
    )
  )
})

test_that("no_escape_hatches passes the words in a comment or a string, a variable named try, a call from another package and a file outside the version", {
  p <- local_project_19()
  write_lines(
    p$home,
    "v01_functions/words.R",
    c(
      "# tryCatch(x) and suppressWarnings(x) in a comment",
      "f <- function(try = 1) {",
      "  msg <- \"use tryCatch() or try()\"",
      "  tries <- try + 1",
      "  other::try(1)",
      "  return(tries)",
      "}"
    )
  )
  write_lines(p$home, "adhoc/explore.R", "x <- try(log(-1))")
  found <- project_findings(p$home)
  expect_identical(failed_rules(found), character(0))
  expect_identical(
    found[found$rule == "no_escape_hatches", c("status", "detail")],
    data.frame(
      status = "PASS",
      detail = paste(
        "no run script or functions folder calls suppressWarnings(),",
        "suppressMessages(), try(), tryCatch() or withCallingHandlers()"
      )
    ),
    ignore_attr = TRUE
  )
})

test_that("no_cross_version_reads fails each string that names another version, an item of one, or adhoc/, with the file and the line", {
  p <- local_project_19()
  add_v02(
    p$home,
    run = c(
      "old <- pbn::read_results(\"v01\")",
      "f <- pbn::folder(\"main\", \"v03_results\")"
    ),
    fun = c(
      "g <- function() {",
      "  source(\"../v01_functions/get_data.R\")",
      "  source(\"..\\\\v01_run.R\")",
      "  readRDS(\"v01_results/real/results.qs2\")",
      "  source(\"adhoc/helpers.R\")",
      "  return(file.path(\"..\", \"v01_cache_gitexclude\"))",
      "}"
    )
  )
  found <- project_findings(p$home)
  expect_identical(
    failed_rules(found),
    c("no_cross_version_reads", "no_absolute_paths")
  )
  rows <- found[found$rule == "no_cross_version_reads", ]
  expect_identical(rows$status, rep("FAIL", 7))
  expect_identical(
    rows$detail,
    with_fix(
      "no_cross_version_reads",
      c(
        "v02_functions/f.R:2: \"../v01_functions/get_data.R\" names v01",
        "v02_functions/f.R:3: \"..\\\\v01_run.R\" names v01",
        "v02_functions/f.R:4: \"v01_results/real/results.qs2\" names v01",
        "v02_functions/f.R:5: \"adhoc/helpers.R\" names adhoc/",
        "v02_functions/f.R:6: \"v01_cache_gitexclude\" names v01",
        "v02_run.R:2: \"v01\" names v01",
        "v02_run.R:3: \"v03_results\" names v03"
      )
    )
  )
})

test_that("no_cross_version_reads passes the own version, a comment, a column such as v01_age, adhoc in a sentence and a file outside the versions", {
  p <- local_project_19()
  add_v02(
    p$home,
    run = c(
      "# v01_results/real and adhoc/ in a comment",
      "r <- pbn::read_results(\"v02\")",
      "x <- pbn::folder(\"main\", \"v02_results\")"
    ),
    fun = c(
      "g <- function(d) {",
      "  d$v01_age <- d[[\"v01_age\"]]",
      "  label <- \"an adhoc analysis of v01\"",
      "  n <- c(\"v1\", \"v100\", \"v01x\", \"version v01\", sprintf(\"v%02d\", 1))",
      "  return(file.path(\"..\", \"v02_results\"))",
      "}"
    )
  )
  write_lines(p$home, "adhoc/compare.R", "a <- pbn::read_results(\"v01\")")
  found <- project_findings(p$home)
  expect_identical(
    found[found$rule == "no_cross_version_reads", c("status", "detail")],
    data.frame(
      status = "PASS",
      detail = paste(
        "no string in a run script or a functions folder names another",
        "version or adhoc/"
      )
    ),
    ignore_attr = TRUE
  )
  expect_identical(failed_rules(found), "no_absolute_paths")
})

test_that("verify() runs a passing version and gives the verdict PASS, with status 0, no failed assertion and no FAIL", {
  skip_if_pbn_not_installed()
  p <- local_project_19()
  out <- utils::capture.output(res <- verify_result("v01", p$home))
  expect_identical(res$status, 0L)
  expect_identical(nrow(res$assertions), 0L)
  expect_identical(
    names(res$assertions),
    c("file", "assertion", "expected", "observed")
  )
  expect_false(any(res$findings$status == "FAIL"))
  expect_identical(attr(res, "verdict"), "PASS")
  expect_identical(
    attr(res, "lines"),
    c(
      "verify(v01): v01_run.R ended with status 0.",
      "check.csv files of this run: 2.",
      "Failed assertions: 0.",
      "check_project(): 0 FAIL, 0 WARN.",
      "Verdict: PASS"
    )
  )
  expect_true(file.exists(file.path(
    p$home,
    "v01_results",
    "known",
    "check.csv"
  )))
})

test_that("verify() gives the verdict FAIL for a run with a failed assertion, with its status, the assertion and the FAIL of scenario_checks", {
  skip_if_pbn_not_installed()
  p <- local_project_19()
  write_lines(
    p$home,
    "v01_functions/expect_known.R",
    expect_known_lines(events_a = 3)
  )
  out <- utils::capture.output(res <- verify_result("v01", p$home))
  expect_identical(res$status, 1L)
  expect_identical(
    res$assertions,
    data.frame(
      file = "v01_results/known/check.csv",
      assertion = "events of x in group a",
      expected = "3",
      observed = "4"
    )
  )
  expect_identical(attr(res, "verdict"), "FAIL")
  fail <- res$findings[res$findings$status == "FAIL", ]
  expect_identical(fail$rule, "scenario_checks")
  expect_identical(
    attr(res, "lines"),
    c(
      "verify(v01): v01_run.R ended with status 1.",
      "check.csv files of this run: 2.",
      "Failed assertions: 1.",
      "  v01_results/known/check.csv: events of x in group a: expected 3, observed 4",
      "check_project(): 1 FAIL, 0 WARN.",
      paste0("FAIL  scenario_checks  ", fail$detail),
      paste0(
        "Verdict: FAIL, because the run ended with status 1, an assertion ",
        "failed and a rule of check_project() failed."
      )
    )
  )
})

test_that("verify() under Rscript prints the verdict and ends with status 1 after a FAIL, and 0 after a PASS", {
  skip_if_pbn_not_installed()
  p <- local_project_19()
  script <- file.path(p$root, "verify.R")
  writeLines("pbn::verify(\"v01\")", script)
  ok <- callr::rscript(
    script,
    wd = p$home,
    show = FALSE,
    fail_on_status = FALSE
  )
  ok_lines <- strsplit(ok$stdout, "\r?\n")[[1]]
  expect_identical(ok$status, 0L)
  expect_identical(ok_lines[length(ok_lines)], "Verdict: PASS")
  write_lines(
    p$home,
    "v01_functions/expect_known.R",
    expect_known_lines(events_a = 3)
  )
  bad <- callr::rscript(
    script,
    wd = p$home,
    show = FALSE,
    fail_on_status = FALSE
  )
  bad_lines <- strsplit(bad$stdout, "\r?\n")[[1]]
  expect_identical(bad$status, 1L)
  expect_true(startsWith(
    bad_lines[length(bad_lines)],
    "Verdict: FAIL, because "
  ))
  expect_true("Failed assertions: 1." %in% bad_lines)
})

# A run script for verify() that starts the project v01 and then runs the
# lines `lines`. It needs no plan.
write_run <- function(home, lines) {
  return(write_lines(
    home,
    "v01_run.R",
    c(
      "pbn::initialize_project(version = \"v01\", home = \".\", folders = c(main = \"x\"))",
      lines
    )
  ))
}

# R code that writes the check.csv of the scenario `scenario` with one
# assertion whose pass is `pass`.
check_csv_lines <- function(scenario, pass) {
  return(c(
    paste0(
      "dir.create(\"v01_results/",
      scenario,
      "\", recursive = TRUE, showWarnings = FALSE)"
    ),
    paste0(
      "utils::write.csv(data.frame(assertion = \"a\", expected = \"1\", ",
      "observed = \"2\", pass = ",
      pass,
      "), \"v01_results/",
      scenario,
      "/check.csv\", row.names = FALSE)"
    )
  ))
}

test_that("verify() gives FAIL for a failed assertion of this run also when scenario_checks is off and the run ends with status 0", {
  skip_if_pbn_not_installed()
  p <- local_project_19()
  write_lines(
    p$home,
    "pbn.yml",
    c(
      "extends: house_2026_11_19",
      "rules:",
      "  scenario_checks: off"
    )
  )
  write_run(p$home, check_csv_lines("known", "FALSE"))
  out <- utils::capture.output(res <- verify_result("v01", p$home))
  expect_identical(res$status, 0L)
  expect_false(any(res$findings$status == "FAIL"))
  expect_identical(res$assertions$file, "v01_results/known/check.csv")
  expect_identical(attr(res, "verdict"), "FAIL")
  expect_identical(
    utils::tail(attr(res, "lines"), 1),
    "Verdict: FAIL, because an assertion failed."
  )
})

test_that("verify() reads only the check.csv files of this run, and passes a run whose own files pass", {
  skip_if_pbn_not_installed()
  p <- local_project_19()
  write_lines(
    p$home,
    "pbn.yml",
    c(
      "extends: house_2026_11_19",
      "rules:",
      "  scenario_checks: warn"
    )
  )
  # A failed check.csv of an earlier run.
  old <- write_lines(
    p$home,
    "v01_results/null/check.csv",
    c(
      "\"assertion\",\"expected\",\"observed\",\"pass\"",
      "\"b\",\"1\",\"2\",FALSE"
    )
  )
  Sys.setFileTime(old, Sys.time() - 3600)
  write_run(p$home, check_csv_lines("known", "TRUE"))
  out <- utils::capture.output(res <- verify_result("v01", p$home))
  expect_identical(res$status, 0L)
  expect_identical(nrow(res$assertions), 0L)
  expect_identical(attr(res, "verdict"), "PASS")
  expect_identical(attr(res, "lines")[2], "check.csv files of this run: 1.")
  # The run wrote no real results, so the PASS covers the fake data only.
  expect_identical(
    utils::tail(attr(res, "lines"), 1),
    "Verdict: PASS on the fake scenarios only. The scenario `real` did not run."
  )
  # The old file is still a finding of check_project().
  expect_identical(
    res$findings$status[res$findings$rule == "scenario_checks"],
    "WARN"
  )
})

test_that("verify() gives FAIL for a run with status 0 that wrote no check.csv", {
  skip_if_pbn_not_installed()
  p <- local_project_19()
  write_run(p$home, "x <- 1")
  out <- utils::capture.output(res <- verify_result("v01", p$home))
  expect_identical(res$status, 0L)
  expect_false(any(res$findings$status == "FAIL"))
  expect_identical(attr(res, "verdict"), "FAIL")
  expect_identical(
    attr(res, "lines"),
    c(
      "verify(v01): v01_run.R ended with status 0.",
      "check.csv files of this run: 0. The run wrote no check of a fake scenario.",
      "Failed assertions: 0.",
      "check_project(): 0 FAIL, 0 WARN.",
      "Verdict: FAIL, because the run wrote no check.csv."
    )
  )
})

test_that("verify() does not count a check.csv of an earlier run in the same second as its call", {
  skip_if_pbn_not_installed()
  p <- local_project_19()
  write_run(p$home, "x <- 1")
  # Start at the beginning of a second, so that the call of verify_result()
  # below comes in the same second as the time of the old file.
  now <- as.numeric(Sys.time())
  Sys.sleep(ceiling(now) - now + 0.01)
  second <- floor(as.numeric(Sys.time()))
  old <- write_lines(
    p$home,
    "v01_results/known/check.csv",
    c(
      "\"assertion\",\"expected\",\"observed\",\"pass\"",
      "\"a\",\"1\",\"1\",TRUE"
    )
  )
  Sys.setFileTime(old, as.POSIXct(second, origin = "1970-01-01"))
  # The test proves the same-second case only when the call starts in that
  # second. Otherwise it would pass without the case, so it skips.
  if (floor(as.numeric(Sys.time())) != second) {
    skip(
      "the setup crossed a second boundary, so the call is not in the same second"
    )
  }
  out <- utils::capture.output(res <- verify_result("v01", p$home))
  expect_identical(res$status, 0L)
  expect_identical(
    attr(res, "lines")[2],
    "check.csv files of this run: 0. The run wrote no check of a fake scenario."
  )
  expect_identical(
    utils::tail(attr(res, "lines"), 1),
    "Verdict: FAIL, because the run wrote no check.csv."
  )
})

test_that("verify() stops without the run script", {
  p <- local_project_19()
  expect_error(
    verify_result("v02", p$home),
    "verify() needs v02_run.R in ",
    fixed = TRUE
  )
})
