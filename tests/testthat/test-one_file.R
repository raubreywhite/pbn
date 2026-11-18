# to_one_file() and check_one_file(): the run of the real data as one file.

test_that("to_one_file writes the header, the server paths as given, the code in the order of org without the fake expect_ functions, and the run script for real", {
  p <- local_project()
  add_one_file_files(p)
  sourced <- init(
    p,
    one_file_home = "N:/durable/project",
    one_file_folders = c(main = "N:/durable/data")
  )
  out <- utils::capture.output(file <- to_one_file())
  expect_identical(
    file,
    file.path(normalizePath(p$home, winslash = "/"), "v01_one_file.R")
  )
  bytes <- readBin(file, "raw", file.size(file))
  expect_false(any(bytes == as.raw(13)))
  expect_true(all(bytes < as.raw(128)))
  lines <- readLines(file)
  expect_identical(
    out,
    paste0(
      "pbn: wrote v01_one_file.R in the project folder ",
      normalizePath(p$home, winslash = "/"),
      ": ",
      length(lines),
      " lines, ",
      file.size(file),
      " bytes, the longest line ",
      max(nchar(lines)),
      " characters."
    )
  )
  expect_identical(
    lines[1:5],
    c(
      "# v01_one_file.R: the run of the real data of the project project, version v01,",
      "# as one file for a computer without the project folder.",
      "#",
      paste0(
        "# pbn ",
        getNamespaceVersion("pbn"),
        " generated it on ",
        format(Sys.Date(), "%Y-%m-%d"),
        " from the project folder project."
      ),
      "# Do not edit: change the project and generate again with pbn::to_one_file()."
    )
  )
  expect_true(any(
    lines ==
      paste0(
        "#   v01_run.R                     ",
        state$source_md5[["v01_run.R"]]
      )
  ))
  expect_true(any(
    lines ==
      "# Workers: pbn::run_plan() below runs on 1 worker. To use more, add"
  ))
  expect_true(any(lines == "setwd(\"N:/durable/project\")"))
  expect_true(any(lines == "  home = \"N:/durable/project\","))
  expect_true(any(lines == "    main = \"N:/durable/data\""))
  # The code of each file in the order that org sources the files.
  marks <- sub(
    "^# ---- (.*) ----$",
    "\\1",
    grep("^# ---- v01_functions/", lines, value = TRUE)
  )
  expect_identical(marks, org_order(p))
  expect_identical(
    marks[1:3],
    c(
      "v01_functions/_cfg.R",
      "v01_functions/01_first.R",
      "v01_functions/analyses.R"
    )
  )
  # The run script, with the real data only.
  expect_identical(
    sum(lines == "scenarios <- pbn::scenarios(CFG, only = \"real\")"),
    1L
  )
  expect_false(any(grepl("pbn::scenarios(CFG)", lines, fixed = TRUE)))
  expect_identical(sum(startsWith(lines, "pbn::initialize_project(")), 1L)
  # The fake expect_ functions are left out, and expect_real() stays.
  expect_false(any(grepl("expect_known <-", lines, fixed = TRUE)))
  expect_false(any(grepl("expect_null <-", lines, fixed = TRUE)))
  expect_true(any(lines == "expect_real <- function(results) {"))
  # The end says what to type next.
  expect_identical(
    utils::tail(lines, 6),
    c(
      "# ---- The end of the run ----",
      "# The real results are in N:/durable/project/v01_results/real/.",
      "# To show the numbers on the screen, type:",
      "#   pbn::show_blocks()",
      "#   pbn::show_block(1)",
      "#   pbn::show_block(1, page = 2)"
    )
  )
  # The file parses, and the call holds the md5 of the project.
  exprs <- parse(file, keep.source = FALSE)
  init_call <- exprs[[which(vapply(
    as.list(exprs),
    function(e) {
      return(is.call(e) && identical(e[[1]], quote(pbn::initialize_project)))
    },
    logical(1)
  ))]]
  # The call has the paths of the server as home and folders, and no
  # argument of this computer.
  expect_identical(
    names(init_call)[-1],
    c("version", "home", "folders", "generated")
  )
  one <- eval(init_call$generated)
  expect_identical(one$project, "project")
  # The md5 of the manifest of the project: blocks.R of this project cannot
  # change the results, so it is not there.
  expect_identical(one$source_md5, state$manifest_md5)
  expect_false("v01_functions/blocks.R" %in% names(one$source_md5))
})

test_that("to_one_file stops before initialize_project(), on a path that is not ASCII, and on a changed file", {
  p <- local_project()
  local_clean_state()
  expect_error(
    to_one_file(),
    "Call pbn::initialize_project() before pbn::to_one_file().",
    fixed = TRUE
  )
  two <- c(main = p$data_default, lookup = file.path(p$root, "lookup_absent"))
  init(
    p,
    folders = two,
    one_file_home = "N:/caf\u00e9",
    one_file_folders = c(main = "N:/d", lookup = "N:/l")
  )
  expect_error(to_one_file(), "is not ASCII", fixed = TRUE)
  init(
    p,
    folders = two,
    one_file_home = "N:/x",
    one_file_folders = c(lookup = "N:/l", main = "N:/d")
  )
  # one_file_folders comes in the order of folders.
  expect_identical(state$one_file_folders, c(main = "N:/d", lookup = "N:/l"))
  write_lines(p$home, "v01_functions/new.R", "new_one <- function() return(1)")
  expect_error(
    to_one_file(),
    paste0(
      "These files changed after pbn::initialize_project() ran: ",
      "v01_functions/new.R. Run pbn::initialize_project() again, then ",
      "pbn::to_one_file()."
    ),
    fixed = TRUE
  )
  expect_false(file.exists(file.path(p$home, "v01_one_file.R")))
})

test_that("to_one_file stops on a file that is not UTF-8 with its file and line, and on a run script without exactly one pbn::scenarios(CFG) and one pbn::initialize_project()", {
  p <- local_project()
  rel <- "v01_functions/analyses.R"
  lines <- readLines(file.path(p$home, rel))
  # The byte of e with an acute accent in Latin-1, which is not UTF-8.
  con <- file(file.path(p$home, rel), "wb")
  writeBin(charToRaw(paste0(lines[1], "\n", lines[2], "\n# caf")), con)
  writeBin(as.raw(0xe9), con)
  writeBin(
    charToRaw(paste0("\n", paste(lines[-(1:2)], collapse = "\n"), "\n")),
    con
  )
  close(con)
  # org stops on such a file in initialize_project() in a UTF-8 locale, so
  # the check of to_one_file() reads it directly.
  expect_error(
    ascii_lines(p$home, rel),
    paste0(rel, ":3 is not UTF-8."),
    fixed = TRUE
  )
  writeLines(lines, file.path(p$home, rel))
  run <- readLines(file.path(p$home, "v01_run.R"))
  cases <- list(
    only = list(
      sub(
        "pbn::scenarios(CFG)",
        "pbn::scenarios(CFG, only = \"null\")",
        run,
        fixed = TRUE
      ),
      "v01_run.R has 1 calls of pbn::scenarios(): pbn::scenarios(CFG, only = \"null\"). pbn::to_one_file() needs exactly one call"
    ),
    none = list(
      sub("pbn::scenarios(CFG)", "scenarios(CFG)", run, fixed = TRUE),
      "v01_run.R has 0 calls of pbn::scenarios(). pbn::to_one_file() needs exactly one call"
    ),
    two_init = list(
      c(run[1], run),
      "v01_run.R has 2 top-level calls pbn::initialize_project(). pbn::to_one_file() needs exactly one."
    )
  )
  for (case in names(cases)) {
    writeLines(cases[[case]][[1]], file.path(p$home, "v01_run.R"))
    init_server(p)
    expect_error(
      to_one_file(),
      cases[[case]][[2]],
      fixed = TRUE,
      info = case
    )
  }
  expect_false(file.exists(file.path(p$home, "v01_one_file.R")))
  # A TAB before the call and a second statement on its line keep their
  # place.
  writeLines(
    sub(
      "scenarios <- pbn::scenarios(CFG)",
      "\tscenarios <- pbn::scenarios(CFG); n_sc <- 1",
      run,
      fixed = TRUE
    ),
    file.path(p$home, "v01_run.R")
  )
  init_server(p)
  utils::capture.output(file <- to_one_file())
  expect_true(any(
    readLines(file) ==
      "\tscenarios <- pbn::scenarios(CFG, only = \"real\"); n_sc <- 1"
  ))
})

test_that("to_one_file keeps the workers of the run script, and the header says how to change them", {
  p <- local_project()
  writeLines(
    run_script_lines("v01", p$data_default, workers = 2),
    file.path(p$home, "v01_run.R")
  )
  init_server(p)
  utils::capture.output(file <- to_one_file())
  lines <- readLines(file)
  expect_true(any(
    lines == "# Workers: pbn::run_plan() below runs with workers = 2."
  ))
  expect_true(any(
    lines == "# To change the number of workers, change that argument."
  ))
  expect_true(any(lines == "r <- pbn::run_plan(p, workers = 2)"))
})

test_that("check_one_file prints PASS for the file of the project, and the first difference when the data of the copy differs", {
  skip_if_pbn_not_installed()
  p <- local_project()
  add_one_file_files(p)
  res <- run_script(p$home)
  expect_identical(res$status, 0L, info = res$stdout)
  init_server(p)
  utils::capture.output(to_one_file())
  # The new R session gets its library paths from check_one_file(), not
  # from R_LIBS of this R session, as in R CMD build.
  withr::local_envvar(R_LIBS = NA)
  out <- utils::capture.output(ok <- check_one_file())
  expect_identical(
    out,
    paste0(
      "PASS: the file of pbn::to_one_file() gives the real results of the ",
      "project, apart from manifest$run_date, manifest$packages, ",
      "manifest$data_cache$from_cache and manifest$fake_real_data. Each ",
      "page of the 2 blocks prints the same for both."
    )
  )
  expect_true(ok)
  # The data changes after the normal run, so the copy differs.
  write_data(p$data_default)
  csv <- file.path(p$data_default, "d.csv")
  writeLines(c(readLines(csv), "\"b\",0,7,1"), csv)
  init_server(p)
  out <- utils::capture.output(ok <- check_one_file())
  expect_false(ok)
  expect_match(
    out,
    "^FAIL: results.qs2\\$manifest\\$data_files\\$size\\[1\\] is [0-9]+ in the project and [0-9]+ in the file of pbn::to_one_file\\(\\)\\. The file and its run are in "
  )
})

test_that("initialize_project with generated evaluates its code, needs no functions folder and no pbn.yml, and a use of the cache checks no data_*.R file", {
  p <- local_project()
  local_clean_state()
  server <- file.path(p$root, "server")
  dir.create(server)
  one <- list(
    project = "project",
    source_md5 = c(
      "v01_functions/data_x.R" = "00112233445566778899aabbccddeeff",
      v01_run.R = "ffeeddccbbaa99887766554433221100"
    ),
    code = quote({
      CFG <- list(scenarios = data.frame(scenario = "real", sims = 1))
      data_x <- function(scenario, sim) return(data.frame(x = 1:2))
    })
  )
  env <- new.env(parent = globalenv())
  suppressMessages(initialize_project(
    "v01",
    server,
    c(main = p$data_default),
    env = env,
    generated = one
  ))
  expect_identical(env$CFG$scenarios$scenario, "real")
  expect_setequal(state$sourced, c("CFG", "data_x"))
  expect_identical(state$source_md5, one$source_md5)
  expect_identical(state$project, "project")
  expect_identical(list.files(server), character(0))
  sc <- scenarios(env$CFG)
  plan <- plnr::Plan$new()
  add_data(
    plan,
    "d",
    fn = function(scenario, sim) return(env$data_x(scenario, sim)),
    cache = TRUE
  )
  add_analyses(plan, sc, "a", fn = function(data, argset) return(nrow(data$d)))
  out <- utils::capture.output(r <- run_plan(plan))
  expect_identical(
    out,
    "pbn: data d, real sim_0001: computed, because there is no cache file."
  )
  expect_identical(r[["real__sim_0001__a__1"]], 2L)
  bad <- one
  bad$code <- quote(CFG <- 1)
  expect_error(
    suppressMessages(initialize_project(
      "v01",
      server,
      c(main = p$data_default),
      generated = bad
    )),
    "`generated` is for the file of pbn::to_one_file() only.",
    fixed = TRUE
  )
})

test_that("to_one_file has no argument, and writes into the project of the last initialize_project()", {
  expect_identical(names(formals(to_one_file)), "...")
  expect_identical(names(formals(check_one_file)), "...")
  a <- local_project()
  b <- local_project()
  init_server(b)
  out <- utils::capture.output(to_one_file())
  expect_match(
    out,
    paste0(
      "in the project folder ",
      normalizePath(b$home, winslash = "/"),
      ":"
    ),
    fixed = TRUE
  )
  expect_true(file.exists(file.path(b$home, "v01_one_file.R")))
  expect_false(file.exists(file.path(a$home, "v01_one_file.R")))
})

test_that("to_one_file stops when kept code uses a dropped expect_ function, or when a dropped assignment shares its line, and writes no file", {
  p <- local_project()
  file <- file.path(p$home, "v01_one_file.R")
  write_lines(
    p$home,
    "v01_functions/uses.R",
    c(
      "# A helper that the real data also needs.",
      "check_null <- function(results) {",
      "  return(expect_null(results))",
      "}"
    )
  )
  init_server(p)
  expect_error(
    to_one_file(),
    paste0(
      "v01_functions/uses.R:3 uses expect_null, and pbn::to_one_file() ",
      "leaves that function of a fake scenario out."
    ),
    fixed = TRUE
  )
  expect_false(file.exists(file))
  unlink(file.path(p$home, "v01_functions", "uses.R"))
  # The run script uses it too.
  run <- readLines(file.path(p$home, "v01_run.R"))
  writeLines(c(run, "x <- expect_known"), file.path(p$home, "v01_run.R"))
  init_server(p)
  expect_error(
    to_one_file(),
    paste0("v01_run.R:", length(run) + 1, " uses expect_known"),
    fixed = TRUE
  )
  writeLines(run, file.path(p$home, "v01_run.R"))
  # A dropped assignment that shares its line with other code.
  rel <- file.path(p$home, "v01_functions", "expect_null.R")
  lines <- readLines(rel)
  writeLines(c(lines[-length(lines)], "}; helper <- 1"), rel)
  init_server(p)
  expect_error(
    to_one_file(),
    paste0(
      "v01_functions/expect_null.R:",
      length(lines),
      ": the assignment of ",
      "expect_null shares its line with other code."
    ),
    fixed = TRUE
  )
  writeLines(c("x <- 1; expect_null <- function(results) 1"), rel)
  init_server(p)
  expect_error(
    to_one_file(),
    "v01_functions/expect_null.R:1: the assignment of expect_null shares its line",
    fixed = TRUE
  )
  # A comment after it is no code, and the file is written.
  writeLines(c("expect_null <- function(results) 1 # the null check"), rel)
  init_server(p)
  utils::capture.output(to_one_file())
  expect_true(file.exists(file))
})

test_that("to_one_file parses the whole file as its last step, and writes no file that does not parse", {
  p <- local_project()
  init_server(p)
  local_mocked_bindings(one_file_footer = function(home, version) {
    return("x <- (")
  })
  expect_error(
    to_one_file(),
    "The file of pbn::to_one_file() does not parse, so pbn wrote no file.",
    fixed = TRUE
  )
  expect_false(file.exists(file.path(p$home, "v01_one_file.R")))
})

test_that("check_one_file prints the exit status and the last lines of the output when the run of the file stops", {
  skip_if_pbn_not_installed()
  p <- local_project()
  res <- run_script(p$home)
  expect_identical(res$status, 0L, info = res$stdout)
  init_server(p)
  made <- one_file_lines
  local_mocked_bindings(one_file_lines = function(...) {
    return(c(made(...), "stop(\"boom in the new session\")"))
  })
  utils::capture.output(to_one_file())
  out <- utils::capture.output(ok <- check_one_file())
  expect_false(ok)
  expect_match(
    out[1],
    paste0(
      "^FAIL: the file of pbn::to_one_file\\(\\) stopped in its new R session ",
      "with the exit status 1, so there are no results to compare\\. The last ",
      "[0-9]+ lines of its output:$"
    )
  )
  expect_true(any(grepl("boom in the new session", out, fixed = TRUE)))
  expect_match(
    out[length(out)],
    "^Its whole output is in .*run\\.log\\. The file and its run are in "
  )
})

test_that("a Windows path with \\ and a short name goes into the file as text that R reads back", {
  p <- local_project()
  home <- "C:\\Users\\RUNNER~1\\AppData\\Local\\Temp\\Rtmp a b\\home"
  main <- "D:\\data dir\\main"
  init(p, one_file_home = home, one_file_folders = c(main = main))
  utils::capture.output(file <- to_one_file())
  exprs <- parse(file, keep.source = FALSE)
  init_call <- exprs[[which(vapply(
    as.list(exprs),
    function(e) {
      return(is.call(e) && identical(e[[1]], quote(pbn::initialize_project)))
    },
    logical(1)
  ))]]
  expect_identical(init_call$home, home)
  expect_identical(eval(init_call$folders), c(main = main))
  expect_identical(exprs[[1]], call("setwd", home))
})

test_that("the manifest leaves blocks.R out of source_md5, so a change of the blocks after the run keeps check_one_file at PASS", {
  skip_if_pbn_not_installed()
  p <- local_project()
  add_one_file_files(p)
  res <- run_script(p$home)
  expect_identical(res$status, 0L, info = res$stdout)
  real <- qs2::qs_read(file.path(p$home, "v01_results", "real", "results.qs2"))
  expect_false("v01_functions/blocks.R" %in% names(real$manifest$source_md5))
  expect_true(
    "v01_functions/expect_real.R" %in% names(real$manifest$source_md5)
  )
  init_server(p)
  utils::capture.output(to_one_file())
  write_lines(
    p$home,
    "v01_functions/blocks.R",
    c(
      "blocks <- list(",
      "  \"Table 1\" = function(results) results$table1,",
      "  \"Models, new\" = function(results) results$models[, 1:2]",
      ")"
    )
  )
  init_server(p)
  # The file in the project still holds the old blocks, so it fails.
  local({
    local_mocked_bindings(run_rscript = function(...) stop("no run expected"))
    out <- utils::capture.output(ok <- check_one_file())
    expect_false(ok)
    expect_match(
      out,
      paste0(
        "^FAIL: the file v01_one_file\\.R in the project is older than the ",
        "code of the project: run pbn::to_one_file\\(\\) again, then check\\. ",
        "The first line that differs is line [0-9]+\\. The file has ",
        "\"#   v01_functions/blocks\\.R "
      )
    )
  })
  utils::capture.output(to_one_file())
  # The md5 of the header still names the file of the blocks.
  expect_true("v01_functions/blocks.R" %in% names(state$source_md5))
  out <- utils::capture.output(ok <- check_one_file())
  expect_match(
    out,
    "^PASS: .*Each page of the 2 blocks prints the same for both\\.$"
  )
  expect_true(ok)
})

test_that("check_one_file says that the results are older than the code, and runs nothing, when a file other than blocks.R changed after the run", {
  p <- local_project()
  save_run(p, only = c("real", "known"))
  write_lines(
    p$home,
    "v01_functions/double_it.R",
    "double_it <- function(x) return(x + x)"
  )
  init_server(p)
  utils::capture.output(to_one_file())
  local_mocked_bindings(run_rscript = function(...) stop("no run expected"))
  out <- utils::capture.output(ok <- check_one_file())
  expect_false(ok)
  expect_identical(
    out,
    paste0(
      "FAIL: the results of the project are older than its code. These ",
      "files changed after the run that made them: ",
      "v01_functions/double_it.R. Run v01_run.R again, then ",
      "pbn::check_one_file()."
    )
  )
})

test_that("check_one_file without real results compares the file of the project first, and then says to run the project", {
  p <- local_project()
  init_server(p)
  out <- utils::capture.output(ok <- check_one_file())
  expect_false(ok)
  expect_identical(
    out,
    paste0(
      "FAIL: there is no file v01_one_file.R in the project. Run ",
      "pbn::to_one_file() first, then pbn::check_one_file()."
    )
  )
  utils::capture.output(to_one_file())
  expect_error(
    check_one_file(),
    paste0(
      "There are no real results in v01_results/real/. The file of ",
      "pbn::to_one_file() passes the checks that need no results: it is the ",
      "file that pbn::to_one_file() writes now"
    ),
    fixed = TRUE
  )
  file <- file.path(p$home, "v01_one_file.R")
  md5 <- tools::md5sum(file)
  writeLines(
    c("x <- 1", "`caf\u00e9` <- 2"),
    file.path(p$home, "v01_functions", "odd.R"),
    useBytes = TRUE
  )
  init_server(p)
  expect_error(
    check_one_file(),
    "v01_functions/odd.R:2 has the name `caf\u00e9`, which is not ASCII.",
    fixed = TRUE
  )
  expect_identical(tools::md5sum(file), md5)
})

test_that("check_one_file fails when the file of the project is older than the code, names the first line that differs, and runs nothing", {
  p <- local_project()
  save_run(p, only = c("real", "known"))
  init_server(p)
  utils::capture.output(to_one_file())
  local_mocked_bindings(run_rscript = function(...) stop("no run expected"))
  write_lines(
    p$home,
    "v01_functions/double_it.R",
    "double_it <- function(x) return(x + x)"
  )
  init_server(p)
  lines <- readLines(file.path(p$home, "v01_one_file.R"))
  # The first line that differs is the md5 of the file in the header.
  i <- grep("^#   v01_functions/double_it.R ", lines)
  expect_length(i, 1)
  md5 <- c(
    unname(tools::md5sum(file.path(p$home, "v01_functions/double_it.R")))
  )
  out <- utils::capture.output(ok <- check_one_file())
  expect_false(ok)
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
      sub("[0-9a-f]{32}$", md5, lines[i]),
      "\" now."
    )
  )
  # Without the lines of the md5, the first line that differs is the code.
  now <- one_file_lines("v01", "N:/x", c(main = "N:/d"))
  keep <- !grepl("[0-9a-f]{32}", lines)
  expect_match(
    first_line_difference(lines[keep], now[keep]),
    "The file has \"double_it <- function(x) return(2 * x)\", and pbn::to_one_file() writes \"double_it <- function(x) return(x + x)\" now.",
    fixed = TRUE
  )
  # A file that is longer than the file of to_one_file() now.
  utils::capture.output(to_one_file())
  file <- file.path(p$home, "v01_one_file.R")
  writeLines(c(readLines(file), "# a line by hand"), file)
  out <- utils::capture.output(ok <- check_one_file())
  expect_false(ok)
  expect_match(
    out,
    "then check\\. The file has [0-9]+ lines, and pbn::to_one_file\\(\\) writes [0-9]+ lines now\\.$"
  )
  # A file without a call of pbn::initialize_project().
  writeLines("x <- 1", file)
  out <- utils::capture.output(ok <- check_one_file())
  expect_false(ok)
  expect_match(
    out,
    "then check. The first line that differs is line 1. The file has \"x <- 1\"",
    fixed = TRUE
  )
})

test_that("the FAIL of a file that is older than the code cuts each quoted line to 80 characters and keeps the line number", {
  long_disk <- paste0("x <- c(", strrep("1, ", 1200), "1)")
  long_now <- paste0("x <- c(", strrep("2, ", 1200), "2)")
  said <- first_line_difference(
    c("a <- 1", long_disk, "b <- 2"),
    c("a <- 1", long_now, "b <- 2")
  )
  expect_identical(
    said,
    paste0(
      "The first line that differs is line 2. The file has \"",
      substr(long_disk, 1, 80),
      "\" [...], and pbn::to_one_file() writes \"",
      substr(long_now, 1, 80),
      "\" [...] now."
    )
  )
  expect_lt(nchar(said), 300)
  # A line of 80 characters stays whole.
  line80 <- strrep("y", 80)
  expect_identical(
    first_line_difference(line80, "z"),
    paste0(
      "The first line that differs is line 1. The file has \"",
      line80,
      "\", and pbn::to_one_file() writes \"z\" now."
    )
  )
})

test_that("check_one_file runs the file of the project with the temporary paths, and the line of the pbn version and the date does not count", {
  p <- local_project()
  save_run(p, only = c("real", "known"))
  init_server(p)
  utils::capture.output(to_one_file())
  file <- file.path(p$home, "v01_one_file.R")
  lines <- readLines(file)
  at <- grep("^# pbn .* generated it on ", lines)
  expect_length(at, 1)
  lines[at] <- sub(
    "pbn [^ ]+ generated it on [0-9-]+",
    "pbn 1.0.0 generated it on 2000-01-01",
    lines[at]
  )
  writeLines(lines, file)
  ran <- NULL
  local_mocked_bindings(run_rscript = function(file, log) {
    ran <<- readLines(file)
    writeLines("stopped by the test", log)
    return(3L)
  })
  out <- utils::capture.output(ok <- check_one_file())
  expect_false(ok)
  expect_match(out[1], "with the exit status 3", fixed = TRUE)
  # The run got the file of the project, with its own line of the date. Only
  # the lines with a path differ, and each holds the temporary path.
  expect_length(ran, length(lines))
  expect_identical(ran[at], lines[at])
  swapped <- which(ran != lines)
  expect_true(all(grepl("N:/", lines[swapped], fixed = TRUE)))
  expect_false(any(grepl("N:/", ran, fixed = TRUE)))
  expect_true(all(grepl("pbn_one_file_", ran[swapped], fixed = TRUE)))
  expect_true(any(ran == "  version = \"v01\","))
  # setwd(), home and main of the call, and two comments with home.
  expect_length(swapped, 5)
})

test_that("check_one_file fails a block that prints differently for the two results, and a block that calls a function of a package that the run script attaches, and names the block", {
  skip_if_pbn_not_installed()
  p <- local_project()
  add_one_file_files(p)
  run <- readLines(file.path(p$home, "v01_run.R"))
  i <- init_line(run)
  flagged <- run
  flagged[i] <- sub("))$", "), fake_real_data = TRUE)", run[i])
  writeLines(flagged, file.path(p$home, "v01_run.R"))
  # The comparison of the results leaves fake_real_data out, and a block
  # that prints it differs.
  write_lines(
    p$home,
    "v01_functions/blocks.R",
    c(
      "blocks <- list(",
      "  \"Table 1\" = function(results) results$table1,",
      "  \"Flag\" = function(results) as.character(results$manifest$fake_real_data)",
      ")"
    )
  )
  res <- run_script(p$home)
  expect_identical(res$status, 0L, info = res$stdout)
  init_server(p, fake_real_data = TRUE)
  utils::capture.output(to_one_file())
  out <- utils::capture.output(ok <- check_one_file())
  expect_false(ok)
  expect_match(
    out,
    paste0(
      "^FAIL: the block 2, \"Flag\", prints differently\\. Line 4 of its ",
      "pages is \"TRUE\" for the project and \"FALSE\" for the file of ",
      "pbn::to_one_file\\(\\)\\. The file and its run are in "
    )
  )
  # mle() is in stats4, which the run script attaches. The blocks run in a
  # new R session that does not attach it.
  writeLines(c("library(stats4)", run), file.path(p$home, "v01_run.R"))
  write_lines(
    p$home,
    "v01_functions/blocks.R",
    c(
      "blocks <- list(",
      "  \"Table 1\" = function(results) results$table1,",
      "  \"A call without its package\" = function(results) mle(function(m) 0)",
      ")"
    )
  )
  res <- run_script(p$home)
  expect_identical(res$status, 0L, info = res$stdout)
  init_server(p)
  utils::capture.output(to_one_file())
  out <- utils::capture.output(ok <- check_one_file())
  expect_false(ok)
  expect_match(
    out,
    paste0(
      "^FAIL: the block 2, \"A call without its package\", failed on the ",
      "real results of the project with: could not find function \"mle\"\\. ",
      "A block function MUST name the package of each function that it calls"
    )
  )
})

test_that("the comparison of check_one_file leaves blocks.R out of source_md5, also for results of an older pbn that hold it, and leaves fake_real_data out", {
  md5 <- c(
    v01_run.R = "00112233445566778899aabbccddeeff",
    "v01_functions/blocks.R" = "ffeeddccbbaa99887766554433221100",
    "v01_functions/_cfg.R" = "0123456789abcdef0123456789abcdef"
  )
  old <- list(
    manifest = list(version = "v01", source_md5 = md5, fake_real_data = TRUE)
  )
  new <- list(
    manifest = list(
      version = "v01",
      source_md5 = md5[-2],
      fake_real_data = FALSE
    )
  )
  expect_identical(comparable_results(old), comparable_results(new))
  expect_identical(
    names(comparable_results(old)$manifest$source_md5),
    c("v01_run.R", "v01_functions/_cfg.R")
  )
  expect_identical(manifest_source_md5(md5, "v02"), md5)
})
