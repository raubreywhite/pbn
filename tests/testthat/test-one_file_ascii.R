# to_one_file() writes an ASCII file from code that holds characters that
# are not ASCII, and the file parses to the code of the project.

# Writes the lines `lines` as UTF-8 to the file `rel` of `home`.
write_utf8 <- function(home, rel, lines) {
  file <- file.path(home, rel)
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  writeLines(enc2utf8(lines), file, useBytes = TRUE)
  return(invisible(file))
}

# Adds labels that are not ASCII to the project `p`: a file of labels with
# strings, a raw string, a string with escapes and a name of a list
# element, an analysis kind that returns them, a block that shows them, and
# a run script with a comment and a string that are not ASCII.
add_label_files <- function(p) {
  add_one_file_files(p)
  write_utf8(
    p$home,
    "v01_functions/labels.R",
    c(
      "# The labels of the manuscript: \u2265, \u00b2 and \u00e6\u00f8\u00e5.",
      "LABELS <- c(",
      "  age = \"age \u2265 18\",",
      "  r2 = \"R\u00b2\",",
      "  minus = \"\u22121\",",
      "  prime = \"x\u2032\",",
      "  nordic = \"\u00e6\u00f8\u00e5\",",
      "  smile = \"\U0001F600 1\",",
      "  raw = r\"(C:\\path \u2265 \"q\")\",",
      "  escapes = \"tab\\t\u2265\\\\ end \\u00e9\"",
      ")",
      "NAMED <- list(\"\u2265 x\" = 1)",
      "label_table <- function(data, argset) {",
      "  return(data.frame(key = names(LABELS), label = unname(LABELS)))",
      "}"
    )
  )
  write_lines(
    p$home,
    "v01_functions/blocks.R",
    c(
      "blocks <- list(",
      "  \"Table 1\" = function(results) results$table1,",
      "  \"Models\" = function(results) results$models,",
      "  \"Labels\" = function(results) results$labels",
      ")"
    )
  )
  run <- run_script_lines("v01", p$data_default)
  at <- which(startsWith(run, "r <- pbn::run_plan("))
  write_utf8(
    p$home,
    "v01_run.R",
    c(
      run[seq_len(at - 1)],
      "# The labels kind, with x\u2032 in a comment.",
      "pbn::add_analyses(p, scenarios, name = \"labels\", fn = label_table)",
      run[at:length(run)],
      "note <- \"\u2265 done\""
    )
  )
  return(invisible(p))
}

# TRUE for each top-level expression of `exprs` that assigns a function
# expect_<fake scenario> of the project of the tests.
is_fake_expect <- function(exprs) {
  return(vapply(
    exprs,
    function(e) {
      return(
        is.call(e) &&
          identical(e[[1]], as.name("<-")) &&
          as.character(e[[2]]) %in% c("expect_known", "expect_null")
      )
    },
    logical(1)
  ))
}

test_that("to_one_file writes each character that is not ASCII of a string as an escape and of a comment as <U+XXXX>, and the file parses to the code of the project", {
  p <- local_project()
  add_label_files(p)
  labels <- file.path(p$home, "v01_functions", "labels.R")
  md5_before <- md5_of(labels)
  bytes <- readBin(labels, "raw", file.size(labels))
  expect_true(any(bytes > as.raw(127)))
  init(p)
  utils::capture.output(file <- to_one_file("v01", "N:/x", c(main = "N:/d")))
  # The project keeps its files, and the file is ASCII.
  expect_identical(md5_of(labels), md5_before)
  out <- readBin(file, "raw", file.size(file))
  expect_true(all(out < as.raw(128)))
  lines <- readLines(file)
  expect_true(any(
    lines ==
      "# The labels of the manuscript: <U+2265>, <U+00B2> and <U+00E6><U+00F8><U+00E5>."
  ))
  expect_true(any(lines == "  age = \"age \\u2265 18\","))
  expect_true(any(lines == "  smile = \"\\U0001f600 1\","))
  # The code of each file of the functions folder, in the order of org, is
  # identical to the code in the file, without the fake expect_ functions.
  gen <- parse(file, keep.source = FALSE, encoding = "UTF-8")
  is_init <- vapply(
    as.list(gen),
    function(e) {
      return(is.call(e) && identical(e[[1]], quote(pbn::initialize_project)))
    },
    logical(1)
  )
  one <- eval(gen[[which(is_init)]]$one_file)
  in_file <- as.list(one$code)[-1]
  from_project <- list()
  for (f in org_order(p)) {
    e <- as.list(parse(
      file.path(p$home, f),
      keep.source = FALSE,
      encoding = "UTF-8"
    ))
    from_project <- c(from_project, e[!is_fake_expect(e)])
  }
  expect_identical(in_file, from_project)
  # The run script, but the two calls that to_one_file() changes.
  run <- as.list(parse(
    file.path(p$home, "v01_run.R"),
    keep.source = FALSE,
    encoding = "UTF-8"
  ))
  changed <- c(
    which(vapply(
      run,
      function(e) return(identical(e[[1]], quote(pbn::initialize_project))),
      logical(1)
    )),
    which(vapply(
      run,
      function(e) return(identical(e, quote(scenarios <- pbn::scenarios(CFG)))),
      logical(1)
    ))
  )
  expect_length(changed, 2)
  expect_identical(as.list(gen)[-1][-changed], run[-changed])
  # The values of the strings, also of the raw string and of the escapes.
  env <- new.env()
  eval(one$code, envir = env)
  expect_identical(
    unname(env$LABELS),
    c(
      "age \u2265 18",
      "R\u00b2",
      "\u22121",
      "x\u2032",
      "\u00e6\u00f8\u00e5",
      "\U0001F600 1",
      "C:\\path \u2265 \"q\"",
      "tab\t\u2265\\ end \u00e9"
    )
  )
  expect_identical(names(env$NAMED), "\u2265 x")
})

test_that("to_one_file stops on a name that is not ASCII, with its file and line, and writes no file", {
  p <- local_project()
  write_utf8(
    p$home,
    "v01_functions/odd.R",
    c(
      "x <- 1",
      "`\u00e6x` <- function() return(2)"
    )
  )
  init(p)
  expect_error(
    to_one_file("v01", "N:/x", c(main = "N:/d")),
    "v01_functions/odd.R:2 has the name `\u00e6x`, which is not ASCII.",
    fixed = TRUE
  )
  expect_false(file.exists(file.path(p$home, "v01_one_file.R")))
})

test_that("to_one_file drops the comment block directly above a dropped expect_ function, and keeps a comment after a blank line", {
  p <- local_project()
  write_lines(
    p$home,
    "v01_functions/expect_null.R",
    c(
      "# A heading that stays, because a blank line follows it.",
      "",
      "# ---- the null check ----",
      "# It checks the mean over the simulations.",
      expect_null_lines()
    )
  )
  init(p)
  utils::capture.output(file <- to_one_file("v01", "N:/x", c(main = "N:/d")))
  lines <- readLines(file)
  at <- which(lines == "# ---- v01_functions/expect_null.R ----")
  expect_identical(
    lines[at + 1:2],
    c("# A heading that stays, because a blank line follows it.", "")
  )
  expect_false(any(lines == "# ---- the null check ----"))
  expect_false(any(lines == "# It checks the mean over the simulations."))
})

test_that("check_one_file passes a project whose labels are not ASCII, and compares the labels of a block", {
  skip_if_pbn_not_installed()
  p <- local_project()
  add_label_files(p)
  res <- run_script(p$home)
  expect_identical(res$status, 0L, info = res$stdout)
  init(p)
  out <- utils::capture.output(ok <- check_one_file("v01"))
  expect_identical(
    out,
    paste0(
      "PASS: the file of pbn::to_one_file() gives the real results of the ",
      "project, apart from manifest$run_date, manifest$packages, ",
      "manifest$data_cache$from_cache and manifest$fake_real_data. Each ",
      "page of the 3 blocks prints the same for both."
    )
  )
  expect_true(ok)
})

test_that("to_one_file stops with the file and the line on a string that is not valid UTF-8 after its escapes", {
  p <- local_project()
  write_utf8(
    p$home,
    "v01_functions/odd.R",
    c(
      "x <- 1",
      "y <- \"caf\\xE9 \u2265 1\""
    )
  )
  init(p)
  expect_error(
    to_one_file("v01", "N:/x", c(main = "N:/d")),
    "v01_functions/odd.R:2 holds a string that is not valid UTF-8",
    fixed = TRUE
  )
  expect_false(file.exists(file.path(p$home, "v01_one_file.R")))
})
