project_rule_names <- c(
  "lower_case_names",
  "run_script_initialises",
  "no_active_use_scenario",
  "use_scenario_lines",
  "version_items_complete",
  "results_contract",
  "scenario_checks",
  "manual_manuscript_names",
  "manuscript_reads_real",
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
  expect_identical(out[length(out)], "13 PASS, 0 WARN, 0 FAIL")
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
    c(lines[1:4], "pbn::initialize_project(\"v01\", \".\", \"x\")"),
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
  expect_match(ok$stdout, "13 PASS, 0 WARN, 0 FAIL", fixed = TRUE)

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
  expect_match(bad$stdout, "12 PASS, 0 WARN, 1 FAIL", fixed = TRUE)
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

test_that("unknown_path knows run_scenarios.R, the check scripts and pbn_allow.txt", {
  p <- local_project()
  expect_true(file.exists(file.path(p$home, "run_scenarios.R")))
  write_lines(p$home, "v01_check_functions.R", "x <- 1")
  write_lines(p$home, "pbn_allow.txt", character(0))

  found <- project_findings(p$home)
  expect_identical(found$rule, project_rule_names)
  expect_identical(found$status, rep("PASS", length(project_rule_names)))
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
  x$manifest$data_md5 <- NULL
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

test_that("results_contract and unknown_path pass the run artefacts run.log and run.ok, also in a git repository", {
  p <- local_project()
  folder <- file.path(p$home, "v01_results", "real")
  dir.create(folder, recursive = TRUE)
  qs2::qs_save(valid_results(), file.path(folder, "results.qs2"))
  # Text that is not a results object, as run_scenarios() writes it.
  writeLines("Error: planted\nExecution halted", file.path(folder, "run.log"))
  writeLines("0123456789abcdef0123456789abcdef", file.path(folder, "run.ok"))
  found <- project_findings(p$home)
  expect_identical(failed_rules(found), character(0))
  expect_identical(found$status[found$rule == "results_contract"], "PASS")
  expect_identical(found$status[found$rule == "unknown_path"], "PASS")

  # In a git repository that tracks v01_results/ and ignores the artefacts
  # with the lines of the helper .gitignore.
  skip_if(!nzchar(Sys.which("git")), "git is not installed")
  writeLines(c("run.log", "run.ok"), file.path(p$home, ".gitignore"))
  system2("git", c("-C", shQuote(p$home), "init", "-q"))
  expect_false(any(grepl("run\\.(log|ok)$", git_files(p$home))))
  found <- project_findings(p$home)
  expect_identical(failed_rules(found), character(0))
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
    "manuscript/built/v01/tables.R: R cannot parse it"
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

test_that("no_active_use_scenario passes the commented lines of the skeleton", {
  p <- local_project()
  lines <- readLines(file.path(p$home, "v01_run.R"))
  expect_identical(
    lines[2:4],
    c(
      "# pbn::use_scenario(\"real\", sim = 1)",
      "# pbn::use_scenario(\"known\", sim = 1)",
      "# pbn::use_scenario(\"null\", sim = 1)"
    )
  )
  found <- project_findings(p$home)
  row <- found[found$rule == "no_active_use_scenario", ]
  expect_identical(row$status, "PASS")
})

test_that("no_active_use_scenario fails on an active call, with or without pbn::", {
  for (call in c(
    "pbn::use_scenario(\"known\", sim = 1)",
    "use_scenario(\"known\")",
    "pbn:::use_scenario(\"null\", sim = 2)"
  )) {
    p <- local_project()
    lines <- readLines(file.path(p$home, "v01_run.R"))
    lines <- append(lines, call, after = 2)
    writeLines(lines, file.path(p$home, "v01_run.R"))
    found <- project_findings(p$home)
    expect_identical(failed_rules(found), "no_active_use_scenario", info = call)
    expect_identical(
      found$detail[found$status == "FAIL"],
      paste0(
        "v01_run.R: an active pbn::use_scenario() in the expression on ",
        "line 3. It is for stepping through the script by hand: comment it out"
      ),
      info = call
    )
  }
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
    paste0(
      "v01_run.R: no call pbn::initialize_project(version = \"v01\"). ",
      "pbn::initialize_project() has no argument scenario"
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

# The run script with `lines` from line 3, above the commented line of
# known.
with_use_scenario_lines <- function(p, lines) {
  file <- file.path(p$home, "v01_run.R")
  old <- readLines(file)
  writeLines(c(old[1:2], lines, old[-(1:2)]), file)
  return(invisible(file))
}

test_that("no_active_use_scenario fails in a function body, in an if, and on the reference pbn::use_scenario", {
  cases <- list(
    function_body = c(
      "step <- function() {",
      "  pbn::use_scenario(\"known\", sim = 1)",
      "}"
    ),
    lambda = "step <- function() use_scenario(\"known\")",
    if_interactive = c(
      "if (interactive()) {",
      "  pbn::use_scenario(\"known\", sim = 1)",
      "}"
    ),
    reference = "f <- pbn::use_scenario",
    reference_three_colons = "f <- pbn:::use_scenario"
  )
  for (nm in names(cases)) {
    p <- local_project()
    with_use_scenario_lines(p, cases[[nm]])
    found <- project_findings(p$home)
    expect_identical(failed_rules(found), "no_active_use_scenario", info = nm)
    expect_match(
      found$detail[found$status == "FAIL"],
      "v01_run.R: an active pbn::use_scenario() in the expression on line 3.",
      fixed = TRUE,
      info = nm
    )
  }
})

test_that("no_active_use_scenario passes a comment in a function body, a string and another use_scenario", {
  cases <- list(
    comment_in_body = c(
      "step <- function() {",
      "  # pbn::use_scenario(\"known\", sim = 1)",
      "  return(1)",
      "}"
    ),
    comment_in_if = c(
      "if (interactive()) {",
      "  # pbn::use_scenario(\"known\", sim = 1)",
      "}"
    ),
    string = "note <- \"pbn::use_scenario(\\\"known\\\")\"",
    other_package = "f <- other::use_scenario"
  )
  for (nm in names(cases)) {
    p <- local_project()
    with_use_scenario_lines(p, cases[[nm]])
    found <- project_findings(p$home)
    expect_identical(failed_rules(found), character(0), info = nm)
    expect_identical(
      found$status[found$rule == "no_active_use_scenario"],
      "PASS",
      info = nm
    )
  }
})

# The run script with `lines` in place of the three commented lines of the
# skeleton, lines 2 to 4.
with_scenario_comments <- function(p, lines) {
  file <- file.path(p$home, "v01_run.R")
  old <- readLines(file)
  writeLines(c(old[1], lines, old[-(1:4)]), file)
  return(invisible(file))
}

use_scenario_row <- function(found) {
  return(found[found$rule == "use_scenario_lines", , drop = FALSE])
}

test_that("use_scenario_lines passes the lines in another order and with any whitespace, and writes nothing", {
  cases <- list(
    other_order = c(
      "# pbn::use_scenario(\"null\", sim = 1)",
      "# pbn::use_scenario(\"real\", sim = 1)",
      "# pbn::use_scenario(\"known\", sim = 1)"
    ),
    whitespace = c(
      "#pbn::use_scenario(\"real\",sim=1)",
      "#   pbn :: use_scenario ( \"known\" ,  sim  =  1 )   ",
      "#\tpbn::use_scenario(\"null\",\tsim = 1)"
    ),
    # An indented comment at the top level counts, and a second line of
    # null in a function body does not.
    indented_and_body = c(
      "  # pbn::use_scenario(\"real\", sim = 1)",
      "# pbn::use_scenario(\"known\", sim = 1)",
      "# pbn::use_scenario(\"null\", sim = 1)",
      "step <- function() {",
      "  # pbn::use_scenario(\"null\", sim = 1)",
      "  return(1)",
      "}"
    )
  )
  for (nm in names(cases)) {
    p <- local_project()
    with_scenario_comments(p, cases[[nm]])
    before <- file_states(p$root)
    found <- project_findings(p$home)
    expect_identical(failed_rules(found), character(0), info = nm)
    expect_identical(use_scenario_row(found)$status, "PASS", info = nm)
    # Reading CFG$scenarios sources the functions folder and writes nothing.
    expect_identical(file_states(p$root), before, info = nm)
  }
})

test_that("use_scenario_lines fails a missing line and names the scenario", {
  p <- local_project()
  with_scenario_comments(
    p,
    c(
      "# pbn::use_scenario(\"real\", sim = 1)",
      "# pbn::use_scenario(\"known\", sim = 1)"
    )
  )
  found <- project_findings(p$home)
  expect_identical(failed_rules(found), "use_scenario_lines")
  row <- use_scenario_row(found)
  expect_identical(row$path, "v01_run.R")
  expect_identical(
    row$detail,
    paste0(
      "v01_run.R: no line # pbn::use_scenario(\"null\", sim = 1) above ",
      "pbn::initialize_project()"
    )
  )
})

test_that("use_scenario_lines fails a line for a scenario that is not in CFG$scenarios", {
  p <- local_project()
  lines <- readLines(file.path(p$home, "v01_run.R"))
  writeLines(
    append(lines, "# pbn::use_scenario(\"other\", sim = 1)", after = 4),
    file.path(p$home, "v01_run.R")
  )
  found <- project_findings(p$home)
  expect_identical(failed_rules(found), "use_scenario_lines")
  expect_identical(
    use_scenario_row(found)$detail,
    paste0(
      "v01_run.R: the line # pbn::use_scenario(\"other\", sim = 1) names a ",
      "scenario that is not in CFG$scenarios"
    )
  )
})

test_that("use_scenario_lines does not count a line with another sim or a line below initialize_project()", {
  cases <- list(
    other_sim = c(
      "# pbn::use_scenario(\"real\", sim = 1)",
      "# pbn::use_scenario(\"known\", sim = 1)",
      "# pbn::use_scenario(\"null\", sim = 2)"
    ),
    no_sim = c(
      "# pbn::use_scenario(\"real\", sim = 1)",
      "# pbn::use_scenario(\"known\", sim = 1)",
      "# pbn::use_scenario(\"null\")"
    ),
    no_pbn = c(
      "# pbn::use_scenario(\"real\", sim = 1)",
      "# pbn::use_scenario(\"known\", sim = 1)",
      "# use_scenario(\"null\", sim = 1)"
    )
  )
  for (nm in names(cases)) {
    p <- local_project()
    with_scenario_comments(p, cases[[nm]])
    found <- project_findings(p$home)
    expect_identical(failed_rules(found), "use_scenario_lines", info = nm)
    expect_match(
      use_scenario_row(found)$detail,
      "no line # pbn::use_scenario(\"null\", sim = 1) above",
      fixed = TRUE,
      info = nm
    )
  }

  p <- local_project()
  lines <- readLines(file.path(p$home, "v01_run.R"))
  i <- init_line(lines)
  lines <- append(lines[-4], lines[4], after = i - 1)
  expect_identical(lines[i], "# pbn::use_scenario(\"null\", sim = 1)")
  expect_identical(init_line(lines), i - 1L)
  writeLines(lines, file.path(p$home, "v01_run.R"))
  found <- project_findings(p$home)
  expect_identical(failed_rules(found), "use_scenario_lines")
  expect_match(
    use_scenario_row(found)$detail,
    "no line # pbn::use_scenario(\"null\", sim = 1) above",
    fixed = TRUE
  )
})

test_that("use_scenario_lines is a WARN with the reason when pbn cannot read CFG$scenarios", {
  cfgs <- list(
    no_scenarios = list(
      lines = "CFG <- list(vars = 1)",
      reason = "CFG$scenarios in v01_functions/ must be a data.frame"
    ),
    error = list(
      lines = "stop(\"planted cfg failure\")",
      reason = "planted cfg failure"
    )
  )
  for (nm in names(cfgs)) {
    p <- local_project()
    write_lines(p$home, "v01_functions/_cfg.R", cfgs[[nm]]$lines)
    found <- project_findings(p$home)
    expect_identical(failed_rules(found), character(0), info = nm)
    row <- use_scenario_row(found)
    expect_identical(row$status, "WARN", info = nm)
    expect_match(
      row$detail,
      "v01_run.R: pbn cannot read CFG$scenarios: ",
      fixed = TRUE,
      info = nm
    )
    expect_match(row$detail, cfgs[[nm]]$reason, fixed = TRUE, info = nm)
  }

  # No pbn::initialize_project() call: run_script_initialises fails it.
  p <- local_project()
  lines <- readLines(file.path(p$home, "v01_run.R"))
  i <- init_line(lines)
  lines[i] <- paste("#", lines[i])
  writeLines(
    append(lines, "p <- list(data_raw = \".\")", after = i),
    file.path(p$home, "v01_run.R")
  )
  found <- project_findings(p$home)
  expect_identical(failed_rules(found), "run_script_initialises")
  expect_identical(
    use_scenario_row(found)$detail,
    paste0(
      "v01_run.R: there is no call pbn::initialize_project() to find the ",
      "lines above"
    )
  )
  expect_identical(use_scenario_row(found)$status, "WARN")
})

test_that("use_scenario_lines fails a scenario with more than one line and names the count", {
  p <- local_project()
  with_scenario_comments(
    p,
    c(
      "# pbn::use_scenario(\"real\", sim = 1)",
      "# pbn::use_scenario(\"known\", sim = 1)",
      "# pbn::use_scenario(\"null\", sim = 1)",
      "#pbn::use_scenario( \"null\" , sim = 1 )"
    )
  )
  found <- project_findings(p$home)
  expect_identical(failed_rules(found), "use_scenario_lines")
  expect_identical(
    use_scenario_row(found)$detail,
    paste0(
      "v01_run.R: the scenario null has 2 lines ",
      "# pbn::use_scenario(\"null\", sim = 1); it MUST have 1"
    )
  )
})

test_that("use_scenario_lines counts only a comment of its own line at the top level", {
  top <- c(
    "# pbn::use_scenario(\"real\", sim = 1)",
    "# pbn::use_scenario(\"known\", sim = 1)"
  )
  cases <- list(
    function_body = c(
      "step <- function() {",
      "  # pbn::use_scenario(\"null\", sim = 1)",
      "  return(1)",
      "}"
    ),
    if_block = c(
      "if (interactive()) {",
      "  # pbn::use_scenario(\"null\", sim = 1)",
      "}"
    ),
    braced_block = c("{", "  # pbn::use_scenario(\"null\", sim = 1)", "}"),
    after_code = "x <- 1 # pbn::use_scenario(\"null\", sim = 1)"
  )
  for (nm in names(cases)) {
    p <- local_project()
    with_scenario_comments(p, c(top, cases[[nm]]))
    found <- project_findings(p$home)
    expect_identical(failed_rules(found), "use_scenario_lines", info = nm)
    expect_identical(
      use_scenario_row(found)$detail,
      paste0(
        "v01_run.R: no line # pbn::use_scenario(\"null\", sim = 1) above ",
        "pbn::initialize_project()"
      ),
      info = nm
    )
  }
})
