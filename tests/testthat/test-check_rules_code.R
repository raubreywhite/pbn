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
    expect_identical(found$status, "FAIL", info = case)
    expect_identical(found$detail, bad[[case]][2], info = case)
    expect_identical(found$path, "v01_functions/blocks.R", info = case)
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
      "blocks is assigned 2 times, at v01_functions/blocks.R:1, ",
      "v01_functions/more.R:2. Assign it once"
    )
  )
})

test_that("no_absolute_paths fails file.path() and fs::path() that start from a string, and passes those that start from a name or a call", {
  p <- local_project()
  long <- strrep("d", 50)
  write_lines(p$home, "v01_functions/paths.R", c(
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
  ))
  write_lines(p$home, "adhoc/server.R", c(
    "pbn::to_one_file(\"v01\", home = file.path(\"N:\", \"x\"), folders = c(main = \"N:/d\"))"
  ))
  found <- findings_of(p$home, "no_absolute_paths")
  expect_identical(
    found$detail,
    c(
      "v01_functions/paths.R:2: file.path(\"/\", ...)",
      "v01_functions/paths.R:3: file.path(\"data\", ...)",
      "v01_functions/paths.R:4: base::file.path(\"results\", ...)",
      "v01_functions/paths.R:5: fs::path(\"results\", ...)",
      paste0("v01_functions/paths.R:6: file.path(\"", strrep("d", 40), "\"..., ...)"),
      "v01_functions/paths.R:7: file.path(\"out\", ...)"
    )
  )
  unlink(file.path(p$home, "v01_functions", "paths.R"))
  expect_identical(findings_of(p$home, "no_absolute_paths")$status, "PASS")
})
