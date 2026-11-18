# The paths of the server in initialize_project(): their checks, the
# argument-free to_one_file() and check_one_file(), and the rule that the
# paths change no results.

# The run script `run` of run_script_lines() with the paths of the server in
# its call of pbn::initialize_project(), and the two commented calls.
with_server_lines <- function(run) {
  i <- init_line(run)
  call <- sub(
    "\\)$",
    ", one_file_home = \"N:/x\", one_file_folders = c(main = \"N:/d\"))",
    run[i]
  )
  return(c(
    run[seq_len(i - 1)],
    call,
    "# pbn::to_one_file()",
    run[-seq_len(i)],
    "# pbn::check_one_file()"
  ))
}

test_that("initialize_project stops on one path of the server without the other, on a folder that one_file_folders misses, adds or repeats, and on the argument one_file of an older file", {
  p <- local_project()
  local_clean_state()
  two <- c(main = p$data_default, lookup = file.path(p$root, "lookup_absent"))
  q <- function(...) {
    return(suppressMessages(initialize_project(
      "v01",
      p$home,
      two,
      env = new.env(),
      ...
    )))
  }
  expect_error(
    q(one_file_home = "N:/x"),
    paste0(
      "Give one_file_home and one_file_folders together, or neither. This ",
      "call gives one_file_home only."
    ),
    fixed = TRUE
  )
  expect_error(
    q(one_file_folders = c(main = "N:/d", lookup = "N:/l")),
    "This call gives one_file_folders only.",
    fixed = TRUE
  )
  expect_error(
    q(one_file_home = "N:/x", one_file_folders = c(main = "N:/d")),
    paste0(
      "`one_file_folders` must name exactly the folders of `folders`: main, ",
      "lookup. Missing: lookup."
    ),
    fixed = TRUE
  )
  expect_error(
    q(
      one_file_home = "N:/x",
      one_file_folders = c(main = "N:/d", lookup = "N:/l", codes = "N:/c")
    ),
    "lookup. Not of the project: codes.",
    fixed = TRUE
  )
  expect_error(
    q(
      one_file_home = "N:/x",
      one_file_folders = c(main = "N:/d", main = "N:/e", lookup = "N:/l")
    ),
    "Each name of `one_file_folders` must come once. These come more than once: main.",
    fixed = TRUE
  )
  expect_error(
    q(one_file_home = c("N:/x", "N:/y"), one_file_folders = two),
    paste0(
      "`one_file_home` must be one path, the project folder on the server, ",
      "for example \"N:/durable/project\"."
    ),
    fixed = TRUE
  )
  expect_error(
    q(one_file = list()),
    paste0(
      "pbn::initialize_project() has no argument one_file. An older ",
      "pbn::to_one_file() wrote this file. Generate the file again with ",
      "pbn::to_one_file() in the project folder."
    ),
    fixed = TRUE
  )
  expect_null(state$results)
  # The paths are text: pbn checks no folder and normalises no path.
  q(
    one_file_home = "N:/no/such/../folder",
    one_file_folders = c(lookup = "relative/l", main = "N:/d")
  )
  expect_identical(state$one_file_home, "N:/no/such/../folder")
  expect_identical(
    state$one_file_folders,
    c(main = "N:/d", lookup = "relative/l")
  )
  # One folder without a name is main on both sides.
  suppressMessages(initialize_project(
    "v01",
    p$home,
    p$data_default,
    env = new.env(),
    one_file_home = "N:/x",
    one_file_folders = "N:/d"
  ))
  expect_identical(state$one_file_folders, c(main = "N:/d"))
  # Without them, the session has no paths of the server.
  init(p)
  expect_null(state$one_file_home)
  expect_null(state$one_file_folders)
  # The rule run_script_initialises accepts them in the run script.
  writeLines(
    with_server_lines(readLines(file.path(p$home, "v01_run.R"))),
    file.path(p$home, "v01_run.R")
  )
  found <- project_findings(p$home)
  expect_identical(
    found$status[found$rule == "run_script_initialises"],
    "PASS"
  )
})

test_that("to_one_file and check_one_file take no argument, and without the paths of the server they print the two arguments to add", {
  p <- local_project()
  init_server(p)
  for (fn in c("to_one_file", "check_one_file")) {
    said <- paste0(
      "pbn::",
      fn,
      "() takes no argument. It takes the version and the paths of the ",
      "server from the last pbn::initialize_project() of this R session. ",
      "Give the paths of the server there, as one_file_home and ",
      "one_file_folders."
    )
    f <- get(fn)
    expect_error(f("v01"), said, fixed = TRUE)
    expect_error(
      f(home = "N:/x", folders = c(main = "N:/d")),
      said,
      fixed = TRUE
    )
  }
  expect_false(file.exists(file.path(p$home, "v01_one_file.R")))
  init(
    p,
    folders = c(main = p$data_default, lookup = file.path(p$root, "absent"))
  )
  for (fn in c("to_one_file", "check_one_file")) {
    expect_error(
      get(fn)(),
      paste0(
        "pbn::",
        fn,
        "() needs the paths of the server, and the last ",
        "pbn::initialize_project() of this R session has none. Add these two ",
        "arguments to the call pbn::initialize_project() of v01_run.R, with ",
        "the paths on the server, and run the call again:\n",
        "  one_file_home = \"<the project folder on the server>\",\n",
        "  one_file_folders = c(main = \"<the folder main on the server>\", ",
        "lookup = \"<the folder lookup on the server>\")"
      ),
      fixed = TRUE
    )
  }
  # A change of the paths of the server in the run script after
  # initialize_project() stops to_one_file(), which would write the old
  # paths.
  init_server(p)
  writeLines(
    with_server_lines(readLines(file.path(p$home, "v01_run.R"))),
    file.path(p$home, "v01_run.R")
  )
  expect_error(
    to_one_file(),
    "These files changed after pbn::initialize_project() ran: v01_run.R.",
    fixed = TRUE
  )
  expect_false(file.exists(file.path(p$home, "v01_one_file.R")))
})

test_that("the file of to_one_file leaves out each line that holds only a commented call of to_one_file() or check_one_file(), and keeps every other comment", {
  p <- local_project()
  run <- readLines(file.path(p$home, "v01_run.R"))
  i <- init_line(run)
  dropped <- c(
    "# pbn::to_one_file()",
    "#pbn::check_one_file( )",
    "  ##  to_one_file()",
    "# pbn :: check_one_file ()"
  )
  kept <- c(
    "# pbn::to_one_file(\"v01\")",
    "# Run pbn::to_one_file() on this computer.",
    "x_note <- 1 # pbn::to_one_file()"
  )
  # A line of a string is no comment.
  in_string <- c("x_text <- \"a", "#  pbn::to_one_file()", "\"")
  writeLines(
    c(run[seq_len(i)], dropped, kept, in_string, run[-seq_len(i)]),
    file.path(p$home, "v01_run.R")
  )
  write_lines(
    p$home,
    "v01_functions/notes.R",
    c("# pbn::check_one_file()", "notes <- 1")
  )
  init_server(p)
  utils::capture.output(file <- to_one_file())
  lines <- readLines(file)
  for (x in c(dropped, "# pbn::check_one_file()")) {
    expect_false(any(lines == x), info = x)
  }
  for (x in c(kept, in_string)) {
    expect_identical(sum(lines == x), 1L, info = x)
  }
  expect_true(any(lines == "notes <- 1"))
  exprs <- parse(file, keep.source = FALSE)
  at <- vapply(
    as.list(exprs),
    function(e) return(identical(e[[2]], quote(x_text))),
    logical(1)
  )
  expect_identical(eval(exprs[[which(at)]][[3]]), "a\n#  pbn::to_one_file()\n")
})

test_that("the md5 of the run script leaves out the paths of the server and the commented calls, also with CR LF, and counts every other change", {
  dir <- withr::local_tempdir()
  write_bytes <- function(name, lines, eol = "\n") {
    path <- file.path(dir, name)
    writeBin(charToRaw(paste0(paste(lines, collapse = eol), eol)), path)
    return(path)
  }
  plain <- c(
    "pbn::initialize_project(",
    "  version = \"v04\",",
    "  home = \".\",",
    "  folders = c(ssb_raw = \"fake/ssb_raw\", msis_raw = \"fake/msis_raw\"),",
    "  fake_real_data = TRUE",
    ")",
    "scenarios <- pbn::scenarios(CFG)",
    "r <- pbn::run_plan(p)"
  )
  server <- c(
    plain[1:4],
    "  fake_real_data = TRUE,",
    "  one_file_home = \"N:/durable/project\",",
    paste0(
      "  one_file_folders = c(ssb_raw = \"N:/durable/data/ssb\", ",
      "msis_raw = \"N:/durable/data/msis\")"
    ),
    ")",
    "# pbn::to_one_file()",
    plain[7:8],
    "# pbn::check_one_file()"
  )
  md5 <- function(path) return(run_script_md5(path, md5_of(path)))
  for (eol in c("\n", "\r\n")) {
    a <- write_bytes("a.R", plain, eol)
    b <- write_bytes("b.R", server, eol)
    expect_false(identical(md5_of(b), md5_of(a)))
    expect_identical(md5(b), md5_of(a))
    expect_identical(md5(a), md5_of(a))
  }
  # In the middle of the call too.
  a <- write_bytes("a.R", plain)
  middle <- c(
    plain[1:4],
    "  one_file_home = \"N:/x\",",
    "  one_file_folders = c(ssb_raw = \"N:/s\", msis_raw = \"N:/m\"),",
    plain[5:8]
  )
  expect_identical(md5(write_bytes("m.R", middle)), md5_of(a))
  # Each other change counts.
  other <- server
  other[length(other) - 1] <- "r <- pbn::run_plan(p, workers = 2)"
  expect_false(identical(md5(write_bytes("o.R", other)), md5_of(a)))
  # An argument without a comma before it stays.
  first <- c(
    "pbn::initialize_project(one_file_home = \"N:/x\",",
    "  version = \"v04\",",
    plain[3:8]
  )
  expect_identical(
    md5(write_bytes("f.R", first)),
    md5_of(file.path(dir, "f.R"))
  )
})

test_that("the paths of the server and the commented calls change no results, no run log and no source_md5, keep the results current, and change only the md5 of the bytes of the run script", {
  p <- local_project()
  files <- c("real/results.qs2", "known/results.qs2")
  read_all <- function() {
    out <- lapply(files, function(f) {
      x <- qs2::qs_read(file.path(p$home, "v01_results", f))
      # The one field that MUST differ between two runs.
      x$manifest$run_date <- NULL
      return(x)
    })
    return(list(
      results = out,
      check = md5_of(file.path(p$home, "v01_results", "known", "check.csv")),
      log = run_log_stable(readLines(file.path(
        p$home,
        "v01_results",
        "run.md"
      )))
    ))
  }
  first <- save_run(p, only = c("real", "known"))
  expect_false(inherits(first$err, "error"))
  before <- read_all()
  raw_before <- state$source_raw_md5[["v01_run.R"]]
  md5_before <- state$source_md5[["v01_run.R"]]
  run <- readLines(file.path(p$home, "v01_run.R"))
  writeLines(with_server_lines(run), file.path(p$home, "v01_run.R"))
  # The results of the run before the edit stay current.
  init_server(p)
  expect_null(stale_results(file.path(state$results, "real"), "v01"))
  second <- save_run(
    p,
    only = c("real", "known"),
    one_file_home = "N:/x",
    one_file_folders = c(main = "N:/d")
  )
  expect_false(inherits(second$err, "error"))
  expect_identical(state$one_file_home, "N:/x")
  after <- read_all()
  expect_identical(after, before)
  expect_identical(state$source_md5[["v01_run.R"]], md5_before)
  expect_false(identical(state$source_raw_md5[["v01_run.R"]], raw_before))
  expect_false(any(grepl("N:/", unlist(after$results), fixed = TRUE)))
  expect_false(any(grepl("N:/", after$log, fixed = TRUE)))
})

test_that("the paths of the server do not enter the cache key", {
  p <- local_project()
  cached_plan <- function(sourced) {
    plan <- plnr::Plan$new()
    fn <- function(scenario, sim) return(sourced$get_data(scenario, sim))
    environment(fn) <- list2env(list(sourced = sourced), parent = globalenv())
    add_data(plan, "d", fn = fn, cache = TRUE)
    add_analyses(
      plan,
      scenarios(sourced$CFG, only = "real"),
      "a",
      fn = function(data, argset) return(nrow(data$d))
    )
    return(plan)
  }
  sourced <- init(p)
  out <- utils::capture.output(run_plan(cached_plan(sourced)))
  expect_identical(
    out,
    "pbn: data d, real sim_0001: computed, because there is no cache file."
  )
  sourced <- init_server(p)
  out <- utils::capture.output(run_plan(cached_plan(sourced)))
  expect_match(out, "^pbn: data d, real sim_0001: reused the cache of ")
})

test_that("check_one_file finds the file older than the run script when the paths of the server changed, or when an older pbn wrote it, and runs nothing", {
  p <- local_project()
  save_run(p, only = c("real", "known"))
  init_server(p)
  utils::capture.output(to_one_file())
  file <- file.path(p$home, "v01_one_file.R")
  lines <- readLines(file)
  local_mocked_bindings(run_rscript = function(...) stop("no run expected"))
  init(p, one_file_home = "N:/y", one_file_folders = c(main = "N:/d"))
  out <- utils::capture.output(ok <- check_one_file())
  expect_false(ok)
  i <- grep("N:/x", lines, fixed = TRUE)[1]
  expect_identical(
    out,
    paste0(
      "FAIL: the file v01_one_file.R in the project is older than the code ",
      "of the project: run pbn::to_one_file() again, then check. The first ",
      "line that differs is line ",
      i,
      ". The file has \"",
      lines[i],
      "\", and pbn::to_one_file() writes \"",
      sub("N:/x", "N:/y", lines[i], fixed = TRUE),
      "\" now."
    )
  )
  # The file of an older pbn, which wrote the argument one_file.
  init_server(p)
  j <- which(lines == "  generated = list(")
  expect_length(j, 1)
  old <- lines
  old[j] <- "  one_file = list("
  writeLines(old, file)
  out <- utils::capture.output(ok <- check_one_file())
  expect_false(ok)
  expect_identical(
    out,
    paste0(
      "FAIL: the file v01_one_file.R in the project is older than the code ",
      "of the project: run pbn::to_one_file() again, then check. An older ",
      "pbn wrote it, with the argument one_file that this pbn does not read. ",
      "The first line that differs is line ",
      j,
      ". The file has \"  one_file = list(\", and pbn::to_one_file() writes ",
      "\"  generated = list(\" now."
    )
  )
})

test_that("the md5 of the run script leaves out an argument only when its value is literal strings, and keeps a value that is code", {
  dir <- withr::local_tempdir()
  head <- c(
    "pbn::initialize_project(",
    "  version = \"v01\",",
    "  home = \".\",",
    "  folders = c(main = \"../d\", lookup = \"../l\")"
  )
  tail <- c(")", "r <- pbn::run_plan(p)")
  plain <- file.path(dir, "plain.R")
  writeLines(c(head, tail), plain)
  md5_with <- function(home, folders) {
    path <- tempfile(tmpdir = dir, fileext = ".R")
    writeLines(
      c(
        head[1:3],
        paste0(head[4], ","),
        paste0("  one_file_home = ", home, ","),
        paste0("  one_file_folders = ", folders),
        tail
      ),
      path
    )
    return(run_script_md5(path, md5_of(path)))
  }
  literal <- list(
    c("\"N:/x\"", "c(main = \"N:/d\", lookup = \"N:/l\")"),
    c("'N:/x'", "c(\"N:/d\", lookup = 'N:/l')"),
    c("\"N:/x\"", "c(main = \"N:/d\", # the data\n    lookup = \"N:/l\")")
  )
  for (x in literal) {
    expect_identical(md5_with(x[1], x[2]), md5_of(plain), info = x[1])
  }
  folders <- "c(main = \"N:/d\", lookup = \"N:/l\")"
  code <- list(
    block = c("{ x <- 5; \"N:/a\" }", folders),
    block_changed = c("{ x <- 6; \"N:/a\" }", folders),
    call = c("paste0(base, \"/a\")", folders),
    call_changed = c("paste0(base, \"/b\")", folders),
    variable = c("srv", folders),
    variable_changed = c("srv2", folders),
    home_in_c = c("c(\"N:/x\")", folders),
    one_element = c(
      "\"N:/x\"",
      "c(main = paste0(\"N:/\", \"d\"), lookup = \"N:/l\")"
    ),
    one_element_changed = c(
      "\"N:/x\"",
      "c(main = paste0(\"N:/\", \"e\"), lookup = \"N:/l\")"
    ),
    folders_variable = c("\"N:/x\"", "srv_folders")
  )
  got <- vapply(code, function(x) return(md5_with(x[1], x[2])), character(1))
  for (nm in names(got)) {
    expect_false(identical(got[[nm]], md5_of(plain)), info = nm)
  }
  for (nm in c("block", "call", "variable", "one_element")) {
    expect_false(
      identical(got[[nm]], got[[paste0(nm, "_changed")]]),
      info = nm
    )
  }
  # to_one_file() works with a value that is code.
  p <- local_project()
  run <- readLines(file.path(p$home, "v01_run.R"))
  i <- init_line(run)
  run[i] <- sub(
    "\\)$",
    ", one_file_home = paste0(\"N:/\", \"x\"), one_file_folders = c(main = \"N:/d\"))",
    run[i]
  )
  writeLines(run, file.path(p$home, "v01_run.R"))
  init_server(p)
  utils::capture.output(file <- to_one_file())
  lines <- readLines(file)
  expect_true(any(lines == "  home = \"N:/x\","))
  expect_false(any(grepl("paste0(\"N:/\"", lines, fixed = TRUE)))
})
