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
    c("version", "home", "folders", "env", "one_file", "...")
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
    return(suppressMessages(initialize_project("v01", p$home, folders, env = new.env())))
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
  file.symlink(file.path(p$root, "nothing"), file.path(p$data_default, "broken"))
  expect_error(
    init(p),
    "pbn cannot read the size or the time of the file main/broken",
    fixed = TRUE
  )
})

test_that("folder() joins parts inside the folder, and stops on a part that leaves it", {
  p <- local_project()
  init(p)
  expect_identical(folder("main", "sub", "d.csv"), paste0(p$data_default, "/sub/d.csv"))
  expect_identical(folder("main", "sub/d.csv"), paste0(p$data_default, "/sub/d.csv"))
  expect_identical(folder("main"), p$data_default)
  for (part in c("/etc", "\\\\server", "C:/x", "c:x", "..", "../x", "a/../../b", "a\\..\\b", "")) {
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
  expect_identical(folder("main", "..x", "x..csv"), paste0(p$data_default, "/..x/x..csv"))
})

test_that("folder() stops before initialize_project() and on a name the project does not declare, with the declared names", {
  p <- local_project()
  local_clean_state()
  expect_error(
    folder("main"),
    "Call pbn::initialize_project() before pbn::folder().",
    fixed = TRUE
  )
  init(p, folders = c(main = p$data_default, lookup = file.path(p$root, "lookup_absent")))
  expect_error(
    folder("codes"),
    paste0(
      "The project has no folder \"codes\". The folders of ",
      "pbn::initialize_project() are: main, lookup."
    ),
    fixed = TRUE
  )
  expect_error(folder(1), "The project has no folder of that name.", fixed = TRUE)
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
  qs2::qs_save(list(a = 2), file.path(folder, "sims_gitexclude", "sim_0002.qs2"))
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
