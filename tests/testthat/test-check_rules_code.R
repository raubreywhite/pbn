# The rules no_absolute_paths and blocks_list of check_project(), and the
# file of to_one_file() among the code rules.

findings_of <- function(home, rule) {
  found <- project_findings(home)
  return(found[found$rule == rule, , drop = FALSE])
}

test_that("is_absolute_path() fails absolute paths and passes regular expressions, URLs, formats, fractions, formulas and relative paths", {
  absolute <- c(
    "/home/a/x.csv",
    "/data/",
    "/Users/me/project",
    "/tmp/x",
    "~/data/x.csv",
    "~\\data",
    "C:/data/x.csv",
    "C:\\data\\x.csv",
    "N:/durable/project",
    "c:/x",
    "\\\\server\\share\\x"
  )
  not_absolute <- c(
    "",
    "/",
    "~",
    "^/",
    "/$",
    "/.*$",
    "/[0-9]+/",
    "/month",
    "/1000/",
    "//cdn.example.com/x.js",
    "https://example.com/a/b",
    "file:///x",
    "d.csv",
    "data/x.csv",
    "./x",
    "../x",
    "../../data/x.csv",
    "%s/%s",
    "%Y/%m/%d",
    "1/2",
    "y ~ x",
    "~ x + z",
    "12:30",
    "a\\b"
  )
  expect_identical(is_absolute_path(absolute), rep(TRUE, length(absolute)))
  expect_identical(
    is_absolute_path(not_absolute),
    rep(FALSE, length(not_absolute))
  )
})

test_that("no_absolute_paths fails each absolute string with its file and line, cut at 40 characters, and passes comments and the arguments of initialize_project() and to_one_file()", {
  p <- local_project()
  long <- paste0("/home/someone/", strrep("a", 40), "/x.csv")
  write_lines(
    p$home,
    "v01_functions/paths.R",
    c(
      "# A comment with /home/a/b does not count.",
      "read_codes <- function() {",
      "  x <- utils::read.csv(\"/home/a/codes.csv\")",
      "  y <- file.path(pbn::folder(\"main\"), \"d.csv\")",
      "  z <- sub(\"/.*$\", \"\", \"a/b\")",
      paste0("  w <- \"", long, "\""),
      "  return(list(x, y, z, w, 'C:\\\\data'))",
      "}"
    )
  )
  write_lines(
    p$home,
    "adhoc/server.R",
    c(
      "pbn::to_one_file(",
      "  \"v01\",",
      "  home = \"N:/durable/project\",",
      "  folders = c(main = \"N:/durable/data\")",
      ")",
      "to_one_file(\"v01\", \"N:/durable/other\")"
    )
  )
  found <- findings_of(p$home, "no_absolute_paths")
  expect_identical(found$status, rep("FAIL", 4))
  expect_identical(
    found$detail,
    c(
      "adhoc/server.R:6: \"N:/durable/other\"",
      "v01_functions/paths.R:3: \"/home/a/codes.csv\"",
      paste0("v01_functions/paths.R:6: \"", substr(long, 1, 40), "\"..."),
      "v01_functions/paths.R:7: \"C:\\\\data\""
    )
  )
  expect_identical(
    found$path,
    c("adhoc/server.R", rep("v01_functions/paths.R", 3))
  )
  # The run script holds the absolute path of the data in the arguments of
  # pbn::initialize_project(), and it passes.
  expect_match(
    readLines(file.path(p$home, "v01_run.R"))[1],
    p$data_default,
    fixed = TRUE
  )
  unlink(file.path(p$home, c("v01_functions/paths.R", "adhoc/server.R")))
  found <- findings_of(p$home, "no_absolute_paths")
  expect_identical(found$status, "PASS")
})

test_that("the file vNN_one_file.R is a known path, no code rule reads it, and no rule takes it for a run script", {
  p <- local_project()
  write_lines(
    p$home,
    "v01_one_file.R",
    c(
      "setwd(\"N:/durable/x\")",
      "f <- function() local({ x <<- 1 })",
      paste0("g <- function(x) {", strrep(" if (x) x <- 1;", 20), " x }"),
      paste0("x", seq_len(1001), " <- 1")
    )
  )
  write_lines(p$home, "v02_one_file.R", "setwd(\"/home/a/b\")")
  found <- project_findings(p$home)
  for (rule in c(
    "lower_case_names",
    "run_script_initialises",
    "version_items_complete",
    "loc_per_file",
    "cyclomatic_complexity",
    "no_local_assign",
    "no_absolute_paths",
    "unknown_path"
  )) {
    expect_identical(found$status[found$rule == rule], "PASS", info = rule)
  }
  # The same code under another name fails each code rule.
  file.rename(
    file.path(p$home, "v01_one_file.R"),
    file.path(p$home, "adhoc", "one_file.R")
  )
  found <- project_findings(p$home)
  for (rule in c(
    "loc_per_file",
    "cyclomatic_complexity",
    "no_local_assign",
    "no_absolute_paths"
  )) {
    expect_identical(
      unique(found$status[found$rule == rule]),
      "FAIL",
      info = rule
    )
  }
})

test_that("blocks_list passes no blocks and a named list of functions, and fails every other blocks", {
  p <- local_project()
  expect_identical(findings_of(p$home, "blocks_list")$status, "PASS")
  write_lines(
    p$home,
    "v01_functions/blocks.R",
    c(
      "table_one <- function(results) return(results$table1)",
      "blocks <- list(",
      "  \"Table 1\" = table_one,",
      "  \"Results para 1: models\" = function(results) results$models",
      ")"
    )
  )
  expect_identical(findings_of(p$home, "blocks_list")$status, "PASS")
  bad <- list(
    not_list = c(
      "blocks <- c(a = function(results) 1)",
      "v01_functions/blocks.R:1: blocks MUST be list(...), one function of results for each place in the manuscript"
    ),
    empty = c(
      "blocks <- list()",
      "v01_functions/blocks.R:1: blocks is an empty list"
    ),
    unnamed = c(
      "blocks <- list(a = function(results) 1, function(results) 2)",
      "v01_functions/blocks.R:1: element 2 of blocks has no name"
    ),
    twice = c(
      "blocks <- list(a = function(results) 1, a = function(results) 2)",
      "v01_functions/blocks.R:1: the name \"a\" comes more than once in blocks"
    ),
    not_function = c(
      "blocks <- list(a = function(results) 1, b = 2)",
      paste0(
        "v01_functions/blocks.R:1: the element \"b\" of blocks is not a ",
        "function, or the name of a function of the functions folder"
      )
    ),
    unknown_name = c(
      "blocks <- list(a = some_function)",
      paste0(
        "v01_functions/blocks.R:1: the element \"a\" of blocks is not a ",
        "function, or the name of a function of the functions folder"
      )
    )
  )
  for (case in names(bad)) {
    write_lines(p$home, "v01_functions/blocks.R", bad[[case]][1])
    found <- findings_of(p$home, "blocks_list")
    expect_identical(unique(found$status), "FAIL", info = case)
    expect_identical(found$detail[1], bad[[case]][2], info = case)
    expect_identical(unique(found$path), "v01_functions/blocks.R", info = case)
  }
  write_lines(
    p$home,
    "v01_functions/blocks.R",
    "blocks <- list(a = function(results) 1)"
  )
  write_lines(
    p$home,
    "v01_functions/more.R",
    c("", "blocks = list(b = function(results) 2)")
  )
  found <- findings_of(p$home, "blocks_list")
  expect_identical(
    found$detail,
    paste0(
      "v01_functions/more.R:2: blocks is assigned here. Assign it in ",
      "v01_functions/blocks.R only, because pbn leaves that file out of ",
      "manifest$source_md5"
    )
  )
  expect_identical(found$path, "v01_functions/more.R")
  unlink(file.path(p$home, "v01_functions", "more.R"))
  write_lines(
    p$home,
    "v01_functions/blocks.R",
    c(
      "blocks <- list(a = function(results) 1)",
      "blocks = list(b = function(results) 2)"
    )
  )
  expect_identical(
    findings_of(p$home, "blocks_list")$detail,
    paste0(
      "v01_functions/blocks.R:1: blocks is assigned 2 times, at ",
      "v01_functions/blocks.R:1, v01_functions/blocks.R:2. Assign it once"
    )
  )
})

test_that("no_absolute_paths fails file.path() and fs::path() that start from a string, and passes those that start from a name or a call", {
  p <- local_project()
  long <- strrep("d", 50)
  write_lines(
    p$home,
    "v01_functions/paths.R",
    c(
      "make_paths <- function(dir) {",
      "  a <- file.path(\"/\", \"data\")",
      "  b <- file.path(\"data\", \"x.csv\")",
      "  c <- base::file.path(\"results\", \"a.csv\")",
      "  d <- fs::path(\"results\", \"a.csv\")",
      paste0("  e <- file.path(\"", long, "\")"),
      "  f <- file.path(fsep = \"/\", \"out\", \"x\")",
      "  g <- file.path(dir, \"fig2.png\")",
      "  h <- file.path(pbn::folder(\"main\"), \"x\")",
      "  i <- file.path(tempdir(), \"x\")",
      "  j <- other::path(\"a\", \"b\")",
      "  k <- path(\"a\", \"b\")",
      "  return(list(a, b, c, d, e, f, g, h, i, j, k))",
      "}"
    )
  )
  write_lines(
    p$home,
    "adhoc/server.R",
    c(
      "pbn::to_one_file(\"v01\", home = file.path(\"N:\", \"x\"), folders = c(main = \"N:/d\"))"
    )
  )
  found <- findings_of(p$home, "no_absolute_paths")
  expect_identical(
    found$detail,
    c(
      "v01_functions/paths.R:2: file.path(\"/\", ...)",
      "v01_functions/paths.R:3: file.path(\"data\", ...)",
      "v01_functions/paths.R:4: base::file.path(\"results\", ...)",
      "v01_functions/paths.R:5: fs::path(\"results\", ...)",
      paste0(
        "v01_functions/paths.R:6: file.path(\"",
        strrep("d", 40),
        "\"..., ...)"
      ),
      "v01_functions/paths.R:7: file.path(\"out\", ...)"
    )
  )
  unlink(file.path(p$home, "v01_functions", "paths.R"))
  expect_identical(findings_of(p$home, "no_absolute_paths")$status, "PASS")
})

test_that("blocks_list fails a name of blocks.R that another file of the folder or the run script uses, with its file and line, and passes a name after $ and an argument name", {
  p <- local_project()
  write_lines(
    p$home,
    "v01_functions/blocks.R",
    c(
      "fmt <- function(x) return(sprintf(\"%.2f\", x))",
      "blocks <- list(\"Table 1\" = function(results) fmt(results$table1$n))"
    )
  )
  expect_identical(findings_of(p$home, "blocks_list")$status, "PASS")
  write_lines(
    p$home,
    "v01_functions/report.R",
    c(
      "report <- function(results) {",
      "  a <- results$fmt",
      "  b <- list(fmt = 1)",
      "  return(fmt(1))",
      "}"
    )
  )
  found <- findings_of(p$home, "blocks_list")
  expect_identical(found$path, "v01_functions/report.R")
  expect_identical(
    found$detail,
    paste0(
      "v01_functions/report.R:4: uses fmt, which v01_functions/blocks.R ",
      "assigns. pbn leaves that file out of manifest$source_md5, so the run ",
      "MUST NOT use it. Move fmt to another file"
    )
  )
  unlink(file.path(p$home, "v01_functions", "report.R"))
  found <- findings_of(p$home, "blocks_list")
  expect_identical(found$status, "PASS")
  # CFG, which blocks.R changes, is a name that the other files use.
  write_lines(
    p$home,
    "v01_functions/blocks.R",
    c(
      "CFG$labels <- c(x = \"X\")",
      "blocks <- list(\"Table 1\" = function(results) results$table1)"
    )
  )
  found <- findings_of(p$home, "blocks_list")
  expect_true(all(found$status == "FAIL"))
  expect_true("v01_run.R" %in% found$path)
  expect_true(any(startsWith(
    found$detail,
    "v01_functions/analyses.R:2: uses CFG"
  )))
})

test_that("blocks_list fails each top-level expression of blocks.R that is not an assignment of a name to function(...) or of blocks to list(...), and a name that another file assigns too, with both places", {
  p <- local_project()
  good <- "blocks <- list(\"Table 1\" = function(results) results$table1)"
  bad <- list(
    library = "library(stats4)",
    options = "options(contrasts = c(\"contr.sum\", \"contr.poly\"))",
    seed = "set.seed(1)",
    branch = "if (TRUE) x <- 1",
    bare = "1 + 1",
    not_function = "fmt <- sprintf",
    superassign = "fmt <<- function(x) return(x)"
  )
  for (case in names(bad)) {
    write_lines(p$home, "v01_functions/blocks.R", c(good, bad[[case]]))
    found <- findings_of(p$home, "blocks_list")
    expect_identical(found$status, "FAIL", info = case)
    expect_identical(found$path, "v01_functions/blocks.R", info = case)
    expect_match(
      found$detail,
      "^v01_functions/blocks\\.R:2: the top level of blocks\\.R holds `",
      info = case
    )
  }
  write_lines(
    p$home,
    "v01_functions/blocks.R",
    c(
      good,
      "",
      "double_it <- function(x) return(3 * x)"
    )
  )
  found <- findings_of(p$home, "blocks_list")
  expect_identical(
    found$detail,
    paste0(
      "v01_functions/blocks.R:3: double_it is assigned here and at ",
      "v01_functions/double_it.R:1. A name of blocks.R MUST NOT be assigned ",
      "in another file, because pbn leaves blocks.R out of ",
      "manifest$source_md5"
    )
  )
  write_lines(
    p$home,
    "v01_functions/blocks.R",
    c(good, "fmt = function(x) return(x)")
  )
  expect_identical(findings_of(p$home, "blocks_list")$status, "PASS")
})

test_that("initialize_project leaves blocks.R out of source_md5 only when it cannot change the results, also with the rule off, and check_one_file then finds results older than a changed blocks.R", {
  p <- local_project()
  conform <- "blocks <- list(\"Table 1\" = function(results) results$table1)"
  write_lines(p$home, "v01_functions/blocks.R", conform)
  init(p)
  expect_true("v01_functions/blocks.R" %in% names(state$source_md5))
  expect_false("v01_functions/blocks.R" %in% names(state$manifest_md5))
  cases <- list(
    side_effect = c(conform, "options(digits = 3)"),
    redefinition = c(conform, "double_it <- function(x) return(3 * x)"),
    use = conform
  )
  write_pbn_yml(p$home, "rules:", "  blocks_list: off")
  for (case in names(cases)) {
    write_lines(p$home, "v01_functions/blocks.R", cases[[case]])
    if (case == "use") {
      write_lines(
        p$home,
        "v01_functions/uses.R",
        "n_blocks <- function() return(length(blocks))"
      )
    }
    init(p)
    expect_true(
      "v01_functions/blocks.R" %in% names(state$manifest_md5),
      info = case
    )
  }
  unlink(file.path(p$home, "v01_functions", "uses.R"))
  # A blocks.R with a side effect counts in source_md5, so a change of it
  # makes the results older than the code.
  write_lines(
    p$home,
    "v01_functions/blocks.R",
    c(conform, "options(digits = 3)")
  )
  save_run(p, only = c("real", "known"))
  real <- qs2::qs_read(file.path(p$home, "v01_results", "real", "results.qs2"))
  expect_true("v01_functions/blocks.R" %in% names(real$manifest$source_md5))
  write_lines(
    p$home,
    "v01_functions/blocks.R",
    c(conform, "options(digits = 4)")
  )
  init(p)
  utils::capture.output(to_one_file("v01", "N:/x", c(main = "N:/d")))
  out <- utils::capture.output(ok <- check_one_file("v01"))
  expect_false(ok)
  expect_identical(
    out,
    paste0(
      "FAIL: the results of the project are older than its code. These ",
      "files changed after the run that made them: ",
      "v01_functions/blocks.R. Run v01_run.R again, then ",
      "pbn::check_one_file()."
    )
  )
  # A blocks.R of the shape stays out, so a change of it is not stale.
  write_lines(p$home, "v01_functions/blocks.R", conform)
  save_run(p, only = c("real", "known"))
  write_lines(
    p$home,
    "v01_functions/blocks.R",
    c(conform, "fmt <- function(x) return(x)")
  )
  init(p)
  expect_null(stale_results(file.path(p$home, "v01_results", "real"), "v01"))
})
