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
      folders = c(main = p$data_default)
    )
  )
  expect_true(is.function(sourced$get_data))
  expect_identical(sourced$CFG$outcomes, c("y", "z"))
  # No results folder, no folder with today's date, no other folder.
  after <- sort(list.files(p$root, recursive = TRUE, include.dirs = TRUE))
  expect_identical(after, before)
  expect_false(dir.exists(file.path(p$home, "v01_results")))
  # A data function finds the folder with pbn::folder(), and org::project
  # gets no folder of the project.
  expect_identical(folder("main"), p$data_default)
  expect_identical(folder(), p$data_default)
  expect_null(org::project$data_raw)
  expect_null(org::project$main)
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
    c(
      "version",
      "home",
      "folders",
      "env",
      "...",
      "fake_real_data",
      "one_file_home",
      "one_file_folders",
      "generated"
    )
  )
  expect_false(any(grepl("commandArgs", deparse(initialize_project))))
})

test_that("initialize_project stops on a bad version, a bad home, bad folders or a missing functions folder, and leaves no folder", {
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
    "`folders` has 2 folders without names. Give each a name",
    fixed = TRUE
  )
  expect_error(
    quiet("v01", p$home, character(0), env = new.env()),
    "`folders` must be one or more paths of folders",
    fixed = TRUE
  )
  expect_error(
    quiet("v01", p$home, c(main = "a", "b"), env = new.env()),
    "These are not: \"\".",
    fixed = TRUE
  )
  expect_error(
    quiet("v01", p$home, c(main = "a", `2nd` = "b"), env = new.env()),
    "These are not: \"2nd\".",
    fixed = TRUE
  )
  expect_error(
    quiet("v01", p$home, c(main = "a", main = "b"), env = new.env()),
    "These come more than once: main.",
    fixed = TRUE
  )
  expect_error(
    quiet("v02", p$home, p$data_default, env = new.env()),
    "There is no functions folder .*v02_functions"
  )
  expect_null(state$results)
  after <- sort(list.files(p$root, recursive = TRUE, include.dirs = TRUE))
  expect_identical(after, before)
})

test_that("data_files lists every file under each folder, at any depth, with its size and time, sorted by name, and an absent folder gives none", {
  p <- local_project()
  init(p)
  csv <- file.path(p$data_default, "d.csv")
  expect_identical(
    state$data_files,
    data.frame(
      file = "main/d.csv",
      size = as.numeric(file.size(csv)),
      mtime = floor(as.numeric(file.mtime(csv)))
    )
  )
  # A file written after the call is not in the list.
  writeLines("x", file.path(p$data_default, "made_by_run.txt"))
  expect_identical(state$data_files$file, "main/d.csv")

  absent <- file.path(p$root, "no_data_here")
  lookup <- file.path(p$root, "lookup")
  dir.create(file.path(lookup, "Sub", "deeper"), recursive = TRUE)
  writeLines("a,b", file.path(lookup, "codes.csv"))
  writeLines("1", file.path(lookup, "Sub", "deeper", "x.csv"))
  writeLines("2", file.path(lookup, ".hidden"))
  init(p, folders = c(main = p$data_default, lookup = lookup, gone = absent))
  expect_identical(
    state$data_files$file,
    c(
      "lookup/.hidden",
      "lookup/Sub/deeper/x.csv",
      "lookup/codes.csv",
      "main/d.csv",
      "main/made_by_run.txt"
    )
  )
  expect_identical(state$folders_absent, "gone")
  expect_identical(folder("gone"), absent)
  expect_identical(folder("lookup"), normalizePath(lookup, winslash = "/"))
  expect_false(dir.exists(absent))
})

test_that("initialize_project stops on a folder inside another and on a folder that holds or is inside the results or the cache folder, and allows a folder elsewhere in home", {
  p <- local_project()
  local_clean_state()
  quiet <- function(folders) {
    return(suppressMessages(initialize_project(
      "v01",
      p$home,
      folders,
      env = new.env()
    )))
  }
  sub <- file.path(p$data_default, "sub")
  dir.create(sub)
  expect_error(
    quiet(c(main = p$data_default, part = sub)),
    "The folder part of `folders` is inside the folder main, or is the same folder.",
    fixed = TRUE
  )
  expect_error(
    quiet(c(main = p$data_default, again = p$data_default)),
    "The folder main of `folders` is inside the folder again, or is the same folder.",
    fixed = TRUE
  )
  inside <- file.path(p$home, "v01_results", "real")
  dir.create(inside, recursive = TRUE)
  expect_error(
    quiet(c(main = inside)),
    "The folder main of `folders` holds or is inside v01_results/, which pbn writes in each run.",
    fixed = TRUE
  )
  expect_error(
    quiet(c(main = p$root)),
    "The folder main of `folders` holds or is inside v01_results/",
    fixed = TRUE
  )
  expect_null(state$results)
  # A folder elsewhere inside home is allowed.
  dir.create(file.path(p$home, "data_describe", "codes"))
  quiet(c(main = file.path(p$home, "data_describe", "codes")))
  expect_identical(state$folders_absent, character(0))
})

test_that("a link to a file counts with its target, and a broken link stops initialize_project()", {
  skip_on_os("windows")
  p <- local_project()
  csv <- file.path(p$data_default, "d.csv")
  file.symlink(csv, file.path(p$data_default, "link.csv"))
  init(p)
  expect_identical(state$data_files$file, c("main/d.csv", "main/link.csv"))
  expect_identical(state$data_files$size[1], state$data_files$size[2])
  file.symlink(
    file.path(p$root, "nothing"),
    file.path(p$data_default, "broken")
  )
  expect_error(
    init(p),
    "pbn cannot read the size or the time of the file main/broken",
    fixed = TRUE
  )
})

test_that("folder() joins parts inside the folder, and stops on a part that leaves it", {
  p <- local_project()
  init(p)
  expect_identical(
    folder("main", "sub", "d.csv"),
    paste0(p$data_default, "/sub/d.csv")
  )
  expect_identical(
    folder("main", "sub/d.csv"),
    paste0(p$data_default, "/sub/d.csv")
  )
  expect_identical(folder("main"), p$data_default)
  for (part in c(
    "/etc",
    "\\\\server",
    "C:/x",
    "c:x",
    "..",
    "../x",
    "a/../../b",
    "a\\..\\b",
    ""
  )) {
    expect_error(
      folder("main", "ok", part),
      "of pbn::folder() leaves the folder main or is no path.",
      fixed = TRUE,
      info = part
    )
  }
  expect_error(
    folder("main", 1),
    "The part that is not one string of pbn::folder()",
    fixed = TRUE
  )
  expect_identical(
    folder("main", "..x", "x..csv"),
    paste0(p$data_default, "/..x/x..csv")
  )
})

test_that("folder() stops before initialize_project() and on a name the project does not declare, with the declared names", {
  p <- local_project()
  local_clean_state()
  expect_error(
    folder("main"),
    "Call pbn::initialize_project() before pbn::folder().",
    fixed = TRUE
  )
  init(
    p,
    folders = c(
      main = p$data_default,
      lookup = file.path(p$root, "lookup_absent")
    )
  )
  expect_error(
    folder("codes"),
    paste0(
      "The project has no folder \"codes\". The folders of ",
      "pbn::initialize_project() are: main, lookup."
    ),
    fixed = TRUE
  )
  expect_error(
    folder(1),
    "The project has no folder of that name.",
    fixed = TRUE
  )
})

test_that("a call with the removed argument data_raw stops with the way to folders", {
  p <- local_project()
  local_clean_state()
  expect_error(
    suppressMessages(initialize_project(
      "v01",
      p$home,
      data_raw = p$data_default,
      env = new.env()
    )),
    paste0(
      "pbn::initialize_project() has no argument data_raw. Give the folder ",
      "as folders = c(main = \"<the folder>\"), and read it in the data code ",
      "with pbn::folder(\"main\") in the place of org::project$data_raw."
    ),
    fixed = TRUE
  )
  expect_error(
    suppressMessages(initialize_project(
      "v01",
      p$home,
      p$data_default,
      datas = 1
    )),
    "pbn::initialize_project() has no argument datas.",
    fixed = TRUE
  )
  expect_null(state$results)
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
  dir.create(file.path(folder, "sims_gitexclude"), recursive = TRUE)
  qs2::qs_save(list(a = 1), file.path(folder, "results.qs2"))
  qs2::qs_save(
    list(a = 2),
    file.path(folder, "sims_gitexclude", "sim_0002.qs2")
  )
  expect_identical(read_results("v01", "null", home = p$home), list(a = 1))
  expect_identical(
    read_results("v01", "null", sim = 2, home = p$home),
    list(a = 2)
  )
  # A clone of the repository has no file of a simulation.
  expect_error(
    read_results("v01", "null", sim = 3, home = p$home),
    paste0(
      "There is no results file of the simulation 3 at ",
      file.path(folder, "sims_gitexclude", "sim_0003.qs2"),
      ". git does not track the results of each simulation, so a clone of ",
      "the repository has none. To write them, run the scenario \"null\" ",
      "with pbn::run_plan() and pbn::save_results()."
    ),
    fixed = TRUE
  )
  expect_error(read_results("v01", "Null", home = p$home), "`scenario` must")
  expect_error(read_results("v01", "null", sim = 0, home = p$home), "`sim`")
})

# The start of the project in `p`, as the first line of the run script does,
# into a new environment. Returns that environment.
start_project <- function(p, env = parent.frame()) {
  local_clean_state(env)
  env <- new.env(parent = globalenv())
  suppressMessages(initialize_project(
    "v01",
    p$home,
    p$data_default,
    env = env
  ))
  return(env)
}

# A project with a functions file that writes the file marker.txt beside the
# project when org sources it (a_marker.R, sourced before long.R), and long.R with
# `lines`. Returns the project with the path of the marker.
long_file_project <- function(lines, env = parent.frame()) {
  p <- local_project(env = env)
  p$marker <- file.path(p$root, "marker.txt")
  write_lines(
    p$home,
    "v01_functions/a_marker.R",
    paste0("writeLines(\"sourced\", \"", p$marker, "\")")
  )
  write_lines(p$home, "v01_functions/long.R", lines)
  return(p)
}

# The status of each finding of loc_per_file in check_project().
loc_status <- function(p) {
  found <- project_findings(p$home)
  found <- found[found$rule == "loc_per_file", ]
  return(stats::setNames(found$status, found$path))
}

lines_1001 <- paste0("x", seq_len(1001), " <- 1")

test_that("a functions file over the limit stops the start before org sources a file, as check_project() fails it", {
  p <- long_file_project(lines_1001)
  expect_identical(loc_status(p), c("v01_functions/long.R" = "FAIL"))
  err <- tryCatch(start_project(p), error = function(e) return(e))
  expect_s3_class(err, "error")
  expect_identical(
    conditionMessage(err),
    paste0(
      "These files of v01_functions/ fail the rule loc_per_file of pbn.yml, ",
      "so the project does not start:\n",
      "  v01_functions/long.R: 1001 code lines, more than 1000\n",
      "Split each file. A file that MUST stay long needs its path under ",
      "allow: loc_per_file: in pbn.yml, for example allow: {loc_per_file: ",
      "[v01_functions/long.R]}."
    )
  )
  expect_false(file.exists(p$marker))
  expect_null(state$results)
})

# The start of a project with the long file and the allow entry `entry`:
# the start sources the functions folder, and check_project() does not fail
# the file.
expect_allowed_start <- function(entry) {
  p <- long_file_project(lines_1001)
  write_pbn_yml(p$home, "allow:", paste0("  loc_per_file: [", entry, "]"))
  expect_identical(
    loc_status(p),
    c("v01_functions/long.R" = "WARN"),
    info = entry
  )
  env <- start_project(p)
  expect_true(file.exists(p$marker), info = entry)
  expect_identical(env$x1001, 1, info = entry)
  expect_identical(state$version, "v01", info = entry)
  return(invisible(NULL))
}

test_that("an allow entry on the long file lets the project start, and check_project() does not fail it", {
  expect_allowed_start("v01_functions/long.R")
})

test_that("an allow entry on the folder of the long file lets the project start, with or without a final /", {
  expect_allowed_start("v01_functions")
  expect_allowed_start("v01_functions/")
})

test_that("a second long file that is not allowed stops the start, and the error names only that file", {
  p <- long_file_project(lines_1001)
  write_lines(p$home, "v01_functions/long2.R", paste0("y", seq_len(1002), " <- 1"))
  write_pbn_yml(p$home, "allow:", "  loc_per_file: [v01_functions/long.R]")
  expect_identical(
    loc_status(p),
    c("v01_functions/long.R" = "WARN", "v01_functions/long2.R" = "FAIL")
  )
  err <- tryCatch(start_project(p), error = function(e) return(e))
  expect_s3_class(err, "error")
  expect_match(
    conditionMessage(err),
    "\n  v01_functions/long2.R: 1002 code lines, more than 1000\nSplit",
    fixed = TRUE
  )
  expect_no_match(conditionMessage(err), "long.R:", fixed = TRUE)
  expect_false(file.exists(p$marker))
})

test_that("the level warn or off lets the project start, and the start uses the limit of pbn.yml", {
  p <- long_file_project(paste0("x", seq_len(60), " <- 1"))
  expect_no_error(start_project(p))
  write_pbn_yml(p$home, "rules:", "  loc_per_file: {limit: 60}")
  expect_no_error(start_project(p))
  write_pbn_yml(p$home, "rules:", "  loc_per_file: {limit: 59}")
  expect_identical(loc_status(p), c("v01_functions/long.R" = "FAIL"))
  expect_error(
    start_project(p),
    "v01_functions/long.R: 60 code lines, more than 59\n",
    fixed = TRUE
  )
  for (level in c("warn", "off")) {
    write_pbn_yml(
      p$home,
      "rules:",
      paste0("  loc_per_file: {limit: 59, level: ", level, "}")
    )
    expect_false(
      "FAIL" %in% loc_status(p),
      info = level
    )
    expect_no_error(start_project(p))
  }
})

# A project whose file v01_functions/long.R is one string of 1202 lines.
string_file_project <- function(env = parent.frame()) {
  return(long_file_project(
    c(
      "lookup <- \"",
      paste0("code", seq_len(1200), ",group", seq_len(1200) %% 7),
      "\""
    ),
    env = env
  ))
}

test_that("a long file that is one string of 1202 lines stops the start before org sources a file", {
  p <- string_file_project()
  expect_identical(loc_status(p), c("v01_functions/long.R" = "FAIL"))
  expect_error(
    start_project(p),
    "v01_functions/long.R: 1202 code lines, more than 1000\n",
    fixed = TRUE
  )
  expect_false(file.exists(p$marker))
})

test_that("a long file that is one string of 1202 lines starts when an allow entry covers it", {
  p <- string_file_project()
  write_pbn_yml(p$home, "allow:", "  loc_per_file: [v01_functions/long.R]")
  expect_identical(loc_status(p), c("v01_functions/long.R" = "WARN"))
  env <- start_project(p)
  expect_true(file.exists(p$marker))
  expect_length(strsplit(env$lookup, "\n", fixed = TRUE)[[1]], 1201L)
})
