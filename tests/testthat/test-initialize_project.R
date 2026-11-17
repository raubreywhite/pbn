test_that("initialize_project sources the functions folder, returns the paths and creates no folder", {
  p <- local_project()
  before <- sort(list.files(p$root, recursive = TRUE, include.dirs = TRUE))
  local_clean_state()
  sourced <- new.env()
  out <- suppressMessages(
    initialize_project("v01", p$home, p$data_default, env = sourced)
  )
  home <- normalizePath(p$home, winslash = "/")
  expect_identical(
    out,
    list(
      version = "v01",
      home = home,
      results = file.path(home, "v01_results"),
      data_raw = p$data_default
    )
  )
  expect_true(is.function(sourced$get_data))
  expect_identical(sourced$CFG$outcomes, c("y", "z"))
  # No results folder, no folder with today's date, no other folder.
  after <- sort(list.files(p$root, recursive = TRUE, include.dirs = TRUE))
  expect_identical(after, before)
  expect_false(dir.exists(file.path(p$home, "v01_results")))
  # A data function finds the real data folder in org::project.
  expect_identical(
    normalizePath(org::project$data_raw, winslash = "/"),
    p$data_default
  )
  expect_identical(state$env, sourced)
})

test_that("initialize_project sets no seed and reads no command-line argument", {
  p <- local_project()
  set.seed(42)
  before <- .Random.seed
  init(p)
  expect_identical(.Random.seed, before)
  expect_identical(
    names(formals(initialize_project)),
    c("version", "home", "data_raw", "env")
  )
  expect_false(any(grepl("commandArgs", deparse(initialize_project))))
})

test_that("initialize_project stops on a bad version, a bad home, a bad data_raw or a missing functions folder, and leaves no folder", {
  p <- local_project()
  before <- sort(list.files(p$root, recursive = TRUE, include.dirs = TRUE))
  local_clean_state()
  quiet <- function(...) return(suppressMessages(initialize_project(...)))
  expect_error(
    quiet("v1", p$home, p$data_default, env = new.env()),
    "`version` must be"
  )
  expect_error(
    quiet("v01", file.path(p$root, "absent"), p$data_default, env = new.env()),
    "`home` must be the path of one existing folder"
  )
  expect_error(
    quiet("v01", p$home, c("a", "b"), env = new.env()),
    "`data_raw` must be the path of one folder"
  )
  expect_error(
    quiet("v02", p$home, p$data_default, env = new.env()),
    "There is no functions folder .*v02_functions"
  )
  expect_null(state$results)
  after <- sort(list.files(p$root, recursive = TRUE, include.dirs = TRUE))
  expect_identical(after, before)
})

test_that("data_md5 lists the files of data_raw at initialize_project(), and an absent data_raw gives none", {
  p <- local_project()
  init(p)
  expect_identical(names(state$data_md5), "d.csv")
  expect_identical(
    unname(state$data_md5),
    md5_of(file.path(p$data_default, "d.csv"))
  )
  # A file written after the call is not in the list.
  writeLines("x", file.path(p$data_default, "made_by_run.txt"))
  expect_identical(names(state$data_md5), "d.csv")

  absent <- file.path(p$root, "no_data_here")
  init(p, data_raw = absent)
  expect_null(state$data_md5)
  expect_identical(state$data_raw, absent)
  expect_false(dir.exists(absent))
})

test_that("source_md5 holds the run script and each file of the functions folder, taken before sourcing", {
  p <- local_project()
  init(p)
  expect_identical(
    names(state$source_md5),
    c(
      "v01_functions/_cfg.R",
      "v01_functions/analyses.R",
      "v01_functions/double_it.R",
      "v01_functions/expect_known.R",
      "v01_functions/expect_null.R",
      "v01_functions/get_data.R",
      "v01_run.R"
    )
  )
  expect_identical(
    unname(state$source_md5[["v01_run.R"]]),
    md5_of(file.path(p$home, "v01_run.R"))
  )
})

test_that("fake_md5 is the same with keep.source TRUE and FALSE, and changes with the code", {
  p <- local_project()
  source_fn <- function(keep_source) {
    env <- new.env()
    sys.source(
      file.path(p$home, "v01_functions", "get_data.R"),
      envir = env,
      keep.source = keep_source
    )
    return(env$get_data)
  }
  with_source <- source_fn(TRUE)
  without_source <- source_fn(FALSE)
  # The planted difference: one function has a source reference.
  expect_false(is.null(attr(with_source, "srcref")))
  expect_null(attr(without_source, "srcref"))
  md5 <- function_md5(list(d = with_source))
  expect_match(md5, "^[0-9a-f]{32}$")
  expect_identical(function_md5(list(d = without_source)), md5)
  # Another name, another body or a second function gives another value.
  expect_false(identical(function_md5(list(e = with_source)), md5))
  other <- without_source
  body(other) <- quote(return(NULL))
  expect_false(identical(function_md5(list(d = other)), md5))
  expect_false(identical(
    function_md5(list(d = with_source, e = other)),
    md5
  ))
})

test_that("read_results reads the direct file, the stacked file or one simulation", {
  p <- local_project()
  folder <- file.path(p$home, "v01_results", "null")
  dir.create(file.path(folder, "sim_0002"), recursive = TRUE)
  qs2::qs_save(list(a = 1), file.path(folder, "results.qs2"))
  qs2::qs_save(list(a = 2), file.path(folder, "sim_0002", "results.qs2"))
  expect_identical(read_results("v01", "null", home = p$home), list(a = 1))
  expect_identical(
    read_results("v01", "null", sim = 2, home = p$home),
    list(a = 2)
  )
  expect_error(
    read_results("v01", "null", sim = 3, home = p$home),
    "There is no results file"
  )
  expect_error(read_results("v01", "Null", home = p$home), "`scenario` must")
  expect_error(read_results("v01", "null", sim = 0, home = p$home), "`sim`")
})
