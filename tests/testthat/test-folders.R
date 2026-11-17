# The folders of initialize_project(): the md5 of their files, the cache
# key, the run log, and the workers.

# A folder lookup beside the data of the project `p`, with one file.
local_lookup <- function(p) {
  lookup <- file.path(p$root, "lookup")
  dir.create(lookup, showWarnings = FALSE)
  writeLines(c("code,label", "1,one"), file.path(lookup, "codes.csv"))
  return(normalizePath(lookup, winslash = "/"))
}

# A plan of the scenario real with one cached data function and one
# analysis that counts its rows.
real_plan <- function(sourced, cache = TRUE) {
  sc <- scenarios(sourced$CFG, only = "real")
  plan <- plnr::Plan$new()
  add_data(
    plan,
    "d",
    fn = function(scenario, sim) return(data.frame(x = 1:3)),
    cache = cache
  )
  add_analyses(plan, sc, "a", fn = function(data, argset) {
    return(data.frame(n = nrow(data$d)))
  })
  return(list(plan = plan, scenarios = sc))
}

test_that("two folders: data_files names each file by its folder, a changed file names its folder in the cache line, and run.md names the folders without a path", {
  p <- local_project()
  lookup <- local_lookup(p)
  sourced <- init(p, folders = c(main = p$data_default, lookup = lookup))
  run <- real_plan(sourced)
  first <- utils::capture.output(r <- run_plan(run$plan))
  expect_identical(
    first,
    "pbn: data d, real sim_0001: computed, because there is no cache file."
  )
  csv <- file.path(lookup, "codes.csv")
  writeLines(c("code,label", "1,uno"), csv)
  Sys.setFileTime(csv, Sys.time() + 10)
  second <- utils::capture.output(r <- run_plan(run$plan))
  expect_identical(
    second,
    paste0(
      "pbn: data d, real sim_0001: computed, because the raw data in the ",
      "folder lookup changed."
    )
  )
  utils::capture.output(save_results(r, run$plan, run$scenarios))
  m <- read_results("v01", home = p$home)$manifest
  expect_identical(m$data_files$file, c("lookup/codes.csv", "main/d.csv"))
  expect_identical(
    m$data_files$mtime,
    floor(as.numeric(file.mtime(c(csv, file.path(p$data_default, "d.csv")))))
  )
  log <- readLines(file.path(p$home, "v01_results", "run.md"))
  expect_true(any(
    log ==
      "The scenario real reads the folders `main` (1 file) and `lookup` (1 file)."
  ))
  expect_false(any(grepl(p$root, log, fixed = TRUE)))
})

test_that("the same project with its folders and its home in another place gives the same cache key and the same bytes of results.qs2", {
  p <- local_project()
  lookup <- local_lookup(p)
  # The run without a cache writes the same bytes in both places.
  sourced <- init(p, folders = c(main = p$data_default, lookup = lookup))
  run <- real_plan(sourced, cache = FALSE)
  utils::capture.output(save_results(
    run_plan(run$plan),
    run$plan,
    run$scenarios
  ))
  file <- file.path("v01_results", "real", "results.qs2")
  bytes_here <- md5_of(file.path(p$home, file))
  # The run with a cache, and its key.
  run <- real_plan(sourced)
  utils::capture.output(save_results(
    run_plan(run$plan),
    run$plan,
    run$scenarios
  ))
  key_here <- read_results("v01", home = p$home)$manifest$data_cache$cache_key

  # Move: a copy of the project with its cache, and the two folders in other
  # places with other names on the disk.
  moved <- file.path(p$root, "moved")
  dir.create(file.path(moved, "elsewhere", "deeper"), recursive = TRUE)
  # The copies keep the modification times, as check_one_file() does.
  file.copy(p$home, moved, recursive = TRUE, copy.date = TRUE)
  file.copy(
    p$data_default,
    file.path(moved, "elsewhere"),
    recursive = TRUE,
    copy.date = TRUE
  )
  file.copy(
    lookup,
    file.path(moved, "elsewhere", "deeper"),
    recursive = TRUE,
    copy.date = TRUE
  )
  home2 <- file.path(moved, "project")
  p2 <- list(home = home2, root = moved, data_default = NULL)
  folders2 <- c(
    main = file.path(moved, "elsewhere", basename(p$data_default)),
    lookup = file.path(moved, "elsewhere", "deeper", "lookup")
  )
  sourced2 <- init(p2, folders = folders2)
  run2 <- real_plan(sourced2)
  lines <- utils::capture.output(r2 <- run_plan(run2$plan))
  expect_match(lines, "^pbn: data d, real sim_0001: reused the cache of ")
  utils::capture.output(save_results(r2, run2$plan, run2$scenarios))
  m2 <- read_results("v01", home = home2)$manifest
  expect_identical(m2$data_cache$cache_key, key_here)
  run2 <- real_plan(sourced2, cache = FALSE)
  utils::capture.output(save_results(
    run_plan(run2$plan),
    run2$plan,
    run2$scenarios
  ))
  expect_identical(md5_of(file.path(home2, file)), bytes_here)
})

test_that("pbn::folder() works in a data function on a worker", {
  skip_if_pbn_not_installed()
  skip_if_not_installed("future")
  skip_if_not_installed("doFuture")
  p <- local_project()
  lookup <- local_lookup(p)
  sourced <- init(p, folders = c(main = p$data_default, lookup = lookup))
  sc <- scenarios(sourced$CFG, only = "real")
  plan <- plnr::Plan$new()
  add_data(plan, "d", fn = function(scenario, sim) {
    return(data.frame(path = pbn::folder("lookup")))
  })
  add_analyses(plan, sc, "a", fn = function(data, argset) {
    return(data.frame(path = data$d$path, pid = Sys.getpid()))
  })
  r <- run_plan(plan, workers = 2)
  value <- r[["real__sim_0001__a__1"]]
  expect_identical(value$path, lookup)
  expect_false(identical(value$pid, Sys.getpid()))
})

test_that("run.md holds no absolute path of the computer, also after save_results stops on an absent folder", {
  p <- local_project()
  absent <- file.path(p$root, "absent_folder")
  sourced <- init(p, folders = c(main = p$data_default, codes = absent))
  run <- real_plan(sourced, cache = FALSE)
  r <- run_plan(run$plan)
  out <- utils::capture.output(
    err <- tryCatch(
      save_results(r, run$plan, run$scenarios),
      error = function(e) {
        return(e)
      }
    )
  )
  expect_match(conditionMessage(err), absent, fixed = TRUE)
  log <- readLines(file.path(p$home, "v01_results", "run.md"))
  expect_identical(log[1], "# Run log of `project`, version v01")
  expect_false(any(grepl(p$root, log, fixed = TRUE)))
  expect_false(any(grepl(
    normalizePath(tempdir(), winslash = "/"),
    log,
    fixed = TRUE
  )))
  # The full run of the run script, from a new R session.
  skip_if_pbn_not_installed()
  res <- run_script(p$home)
  expect_identical(res$status, 0L, info = res$stdout)
  log <- readLines(file.path(p$home, "v01_results", "run.md"))
  expect_true(any(log == "The scenario real reads the folder `main` (1 file)."))
  expect_false(any(grepl(p$root, log, fixed = TRUE)))
})

test_that("the cache key follows the fingerprint: a changed size, a changed time, a new file and a removed file each compute the data again and name the folder, and the same listing reuses it", {
  p <- local_project()
  lookup <- local_lookup(p)
  dir.create(file.path(lookup, "sub"))
  deep <- file.path(lookup, "sub", "deep.csv")
  writeLines("1,2", deep)
  sourced <- init(p, folders = c(main = p$data_default, lookup = lookup))
  run <- real_plan(sourced)
  line <- function() {
    return(utils::capture.output(run_plan(run$plan)))
  }
  computed <- paste0(
    "pbn: data d, real sim_0001: computed, because the raw data in the ",
    "folder lookup changed."
  )
  expect_match(line(), "computed, because there is no cache file.", fixed = TRUE)
  expect_match(line(), "reused the cache of", fixed = TRUE)
  # A changed size, with the old time.
  old <- file.mtime(deep)
  writeLines("1,2,3", deep)
  Sys.setFileTime(deep, old)
  expect_identical(line(), computed)
  expect_match(line(), "reused the cache of", fixed = TRUE)
  # A changed time, with the same bytes.
  Sys.setFileTime(deep, old + 100)
  expect_identical(line(), computed)
  # A new file, deep in a folder.
  writeLines("x", file.path(lookup, "sub", "new.csv"))
  expect_identical(line(), computed)
  # A removed file.
  unlink(file.path(lookup, "sub", "new.csv"))
  expect_identical(line(), computed)
  expect_match(line(), "reused the cache of", fixed = TRUE)
  # The one limit: other bytes with the same size and the same time are not
  # seen, because pbn reads no content of a data file.
  now <- file.mtime(deep)
  writeLines("9,9,9", deep)
  Sys.setFileTime(deep, now)
  expect_match(line(), "reused the cache of", fixed = TRUE)
})

test_that("a copy of the data with new modification times looks like changed data", {
  p <- local_project()
  sourced <- init(p, folders = c(main = p$data_default))
  run <- real_plan(sourced)
  utils::capture.output(run_plan(run$plan))
  copy <- file.path(p$root, "copy")
  dir.create(copy)
  file.copy(file.path(p$data_default, "d.csv"), copy, copy.date = FALSE)
  Sys.setFileTime(file.path(copy, "d.csv"), Sys.time() + 50)
  sourced <- init(p, folders = c(main = copy))
  run <- real_plan(sourced)
  expect_identical(
    utils::capture.output(run_plan(run$plan)),
    paste0(
      "pbn: data d, real sim_0001: computed, because the raw data in the ",
      "folder main changed."
    )
  )
})

test_that("a copy that keeps the modification time with less than its fraction of a second gives the same fingerprint and the same cache key", {
  p <- local_project()
  csv <- file.path(p$data_default, "d.csv")
  # A time with a fraction, and the copy that Windows makes of it: the same
  # second, without the whole fraction.
  Sys.setFileTime(csv, as.POSIXct(1.8e9 + 0.75, origin = "1970-01-01", tz = "UTC"))
  sourced <- init(p, folders = c(main = p$data_default))
  first <- state$data_files
  run <- real_plan(sourced)
  utils::capture.output(run_plan(run$plan))
  copy <- file.path(p$root, "copy")
  dir.create(copy)
  file.copy(csv, copy)
  Sys.setFileTime(
    file.path(copy, "d.csv"),
    as.POSIXct(1.8e9 + 0.5, origin = "1970-01-01", tz = "UTC")
  )
  sourced <- init(p, folders = c(main = copy))
  expect_identical(state$data_files, first)
  expect_identical(first$mtime, 1.8e9)
  run <- real_plan(sourced)
  expect_match(
    utils::capture.output(run_plan(run$plan)),
    "reused the cache of",
    fixed = TRUE
  )
})
