no_file <- "there is no cache file"

calls <- function(p) {
  if (!file.exists(p$counter)) {
    return(character(0))
  }
  return(readLines(p$counter))
}

test_that("add_data checks cache, and run_plan with a cache needs initialize_project", {
  p <- plnr::Plan$new()
  fn <- function(scenario, sim) return(data.frame(x = 1))
  expect_error(
    add_data(p, "d", fn = fn, cache = NA),
    "`cache` must be TRUE or FALSE"
  )
  expect_error(
    add_data(p, "d", fn = function(scenario) return(1), cache = TRUE),
    "With cache = TRUE, the data function needs the arguments scenario and sim",
    fixed = TRUE
  )
  expect_length(plan_info(p)$data, 0)
  local_clean_state()
  add_data(p, "d", fn = fn, cache = TRUE)
  add_analyses(
    p,
    scenarios(list(scenarios = data.frame(scenario = "real", sims = 1))),
    "a",
    fn = function(data, argset) 1
  )
  expect_error(
    run_plan(p),
    "Call pbn::initialize_project() before pbn::run_plan().",
    fixed = TRUE
  )
})

test_that("a second run reads the cache, does not call the data function, and gives results identical to a new computation", {
  skip_if_not_installed("cohort")
  p <- local_cohort_project()
  first <- cohort_run(p)
  expect_identical(first$out, cache_lines(p, rep(no_file, 3)))
  expect_identical(calls(p), c("real 1", "null 1", "null 2"))
  expect_true(all(file.exists(c(
    cache_file(p, "null", 2),
    cache_file(p, "null", 2, key = TRUE)
  ))))
  # No temporary file stays after the rename.
  expect_identical(
    list.files(
      file.path(p$home, "v01_cache_gitexclude"),
      "tmp$",
      recursive = TRUE
    ),
    character(0)
  )

  second <- cohort_run(p)
  expect_identical(second$out, cache_lines(p, rep(NA, 3)))
  expect_identical(calls(p), c("real 1", "null 1", "null 2"))
  expect_identical(without_counts(second$r), without_counts(first$r))
  for (nm in grep("__pbn_n_rows$", names(first$r), value = TRUE)) {
    expect_identical(
      attr(second$r[[nm]], "exclusions"),
      attr(first$r[[nm]], "exclusions")
    )
    expect_identical(
      attr(second$r[[nm]], "cache")$d$key,
      attr(first$r[[nm]], "cache")$d$key
    )
  }
  # The same as without a cache, also the draws of the analyses.
  fresh <- cohort_run(p, cache = FALSE)
  expect_identical(fresh$out, character(0))
  expect_identical(without_counts(second$r), without_counts(fresh$r))
  expect_identical(
    attr(second$r[["real__sim_0001__pbn_n_rows"]], "exclusions"),
    attr(fresh$r[["real__sim_0001__pbn_n_rows"]], "exclusions")
  )

  # The manifest says where the data came from.
  utils::capture.output(save_results(second$r, second$plan, second$scenarios))
  m <- read_results("v01", "null", sim = 2, home = p$home)$manifest
  expect_identical(m$data_cache$from_cache, TRUE)
  expect_identical(m$data_cache$cache, TRUE)
  expect_identical(
    m$data_cache$cache_key,
    attr(first$r[["null__sim_0002__pbn_n_rows"]], "cache")$d$key
  )
  expect_match(m$data_cache$cache_key, "^[0-9a-f]{32}$")
  stacked <- read_results("v01", "null", home = p$home)$manifest$data_cache
  expect_identical(stacked$from_cache, NA)
  expect_identical(stacked$cache_key, NA_character_)
})

test_that("each part of the cache key computes the data again with its reason, and a change of the analysis does not", {
  skip_if_not_installed("cohort")
  p <- local_cohort_project()
  edit <- function(rel, lines) {
    write_lines(p$home, file.path("v01_functions", rel), lines)
  }
  step <- function(why, ...) {
    before <- length(calls(p))
    run <- cohort_run(p, ...)
    expect_identical(run$out, cache_lines(p, why))
    return(list(run = run, new_calls = calls(p)[-seq_len(before)]))
  }
  step(rep(no_file, 3))
  files <- cohort_function_files()

  # The analysis and CFG outside CFG$data: the data comes from the cache.
  edit("analyses.R", c("# A new comment.", files[["analyses.R"]]))
  edit(
    "_cfg.R",
    sub("outcome = ", "extra = 1, outcome = ", cohort_cfg_lines(p$counter))
  )
  expect_identical(step(rep(NA, 3))$new_calls, character(0))

  edit("data_clean.R", c("# A new comment.", files[["data_clean.R"]]))
  s <- step(rep("v01_functions/data_clean.R changed", 3))
  expect_identical(s$new_calls, c("real 1", "null 1", "null 2"))

  edit("_cfg.R", cohort_cfg_lines(p$counter, age_min = 17))
  s <- step(rep("CFG$data changed", 3))
  expect_identical(s$new_calls, c("real 1", "null 1", "null 2"))
  expect_identical(
    attr(s$run$r[["real__sim_0001__pbn_n_rows"]], "exclusions")$d$n_excluded,
    1L
  )

  write_cohort_data(p$data_default, age = c(16, 16, 16, 30, 40, 50, 60, 25))
  s <- step(c("the raw data changed", NA, NA))
  expect_identical(s$new_calls, "real 1")

  edit("_cfg.R", cohort_cfg_lines(p$counter, age_min = 17, n_boot_null = 6))
  s <- step(c(NA, rep("the settings of the scenario changed", 2)))
  expect_identical(s$new_calls, c("null 1", "null 2"))

  other_fn <- function(scenario, sim) {
    d <- data_raw(scenario, sim)
    return(data_cohort(data_clean(d)))
  }
  s <- step(rep("the data function of add_data() changed", 3), fn = other_fn)
  expect_identical(s$new_calls, c("real 1", "null 1", "null 2"))

  versions <- cache_versions()
  expect_identical(names(versions), c("R", "pbn"))
  local_mocked_bindings(cache_versions = function() {
    return(replace(versions, "pbn", "0.0.1"))
  })
  s <- step(rep("the version of pbn changed", 3), fn = other_fn)
  expect_identical(s$new_calls, c("real 1", "null 1", "null 2"))

  # Two parts at once are both named.
  edit("data_raw.R", c("# A new comment.", files[["data_raw.R"]]))
  edit("_cfg.R", cohort_cfg_lines(p$counter, age_min = 18, n_boot_null = 6))
  step(
    rep("v01_functions/data_raw.R, CFG$data changed", 3),
    fn = other_fn
  )
})

test_that("an unreadable cache file, a missing or partial key file, and a cache file of another key are computed again with a line that says so", {
  skip_if_not_installed("cohort")
  p <- local_cohort_project()
  first <- cohort_run(p)
  writeBin(as.raw(1:50), cache_file(p, "real", 1))
  unlink(cache_file(p, "null", 1, key = TRUE))
  file.copy(
    cache_file(p, "null", 1),
    cache_file(p, "null", 2),
    overwrite = TRUE
  )
  run <- cohort_run(p)
  expect_identical(
    run$out,
    cache_lines(
      p,
      c(
        "the cache file cannot be read",
        "the key file of the cache is missing or misses parts of the key",
        "the cache file holds another key than its key file"
      )
    )
  )
  expect_identical(without_counts(run$r), without_counts(first$r))

  key <- qs2::qs_read(cache_file(p, "null", 2, key = TRUE))
  key$parts$versions <- NULL
  qs2::qs_save(key, cache_file(p, "null", 2, key = TRUE))
  run <- cohort_run(p)
  expect_identical(
    run$out[3],
    cache_lines(
      p,
      c(
        NA,
        NA,
        "the key file of the cache is missing or misses parts of the key"
      )
    )[3]
  )
  expect_identical(run$out[1:2], cache_lines(p, c(NA, NA, NA))[1:2])
})

test_that("with 2 workers, the cache lines reach this session and the results are identical to 1 worker, in the first and the second run", {
  skip_if_not_installed("cohort")
  skip_if_pbn_not_installed()
  p <- local_cohort_project()
  cache <- file.path(p$home, "v01_cache_gitexclude")
  one_first <- cohort_run(p)
  unlink(cache, recursive = TRUE)
  two_first <- cohort_run(p, workers = 2)
  expect_identical(two_first$out, cache_lines(p, rep(no_file, 3)))
  expect_identical(two_first$out, one_first$out)
  expect_identical(unstamped(two_first$r), unstamped(one_first$r))

  one_second <- cohort_run(p)
  two_second <- cohort_run(p, workers = 2)
  expect_identical(two_second$out, cache_lines(p, rep(NA, 3)))
  expect_identical(two_second$out, one_second$out)
  expect_identical(unstamped(two_second$r), unstamped(one_second$r))
  expect_identical(without_counts(two_second$r), without_counts(two_first$r))
  # The data came from the cache on the workers too.
  expect_identical(
    attr(two_second$r[["null__sim_0002__pbn_n_rows"]], "cache")$d$from_cache,
    TRUE
  )
  expect_identical(
    sort(calls(p)),
    sort(rep(c("real 1", "null 1", "null 2"), 2))
  )
})

test_that("in a git repository, run_plan stops before it runs anything until git ignores the cache folder", {
  skip_if_not_installed("cohort")
  p <- local_cohort_project()
  git_in(p$home, "init", "-q")
  write_lines(p$home, ".gitignore", "v01_results/")
  expect_error(
    cohort_run(p),
    paste0(
      "is a git repository, and git does not ignore ",
      "v01_cache_gitexclude/d/real/sim_0001.qs2. That file holds data for ",
      "each person. Add the line *gitexclude* to"
    ),
    fixed = TRUE
  )
  expect_identical(calls(p), character(0))
  expect_false(dir.exists(file.path(p$home, "v01_cache_gitexclude")))

  write_lines(p$home, ".gitignore", c("v01_results/", "*gitexclude*"))
  run <- cohort_run(p)
  expect_identical(run$out, cache_lines(p, rep(no_file, 3)))
  # Without a cache, the run needs no .gitignore entry.
  write_lines(p$home, ".gitignore", "v01_results/")
  expect_identical(cohort_run(p, cache = FALSE)$out, character(0))
})

test_that("after run_plan, p$get_data() and p$run_one() by hand read the cache and print the same line", {
  skip_if_not_installed("cohort")
  p <- local_cohort_project()
  run <- cohort_run(p)
  n_calls <- length(calls(p))
  out <- utils::capture.output(
    d <- run$plan$get_data("null__sim_0002__means__1")$d
  )
  expect_identical(out, cache_lines(p, rep(NA, 3))[3])
  expect_identical(length(calls(p)), n_calls)
  expect_s3_class(d, "data.table")
  expect_identical(attr(d, "pbn_cache")$from_cache, TRUE)
  expect_identical(nrow(d), run$r[["null__sim_0002__pbn_n_rows"]][["d"]])
  # pbn::get_data() takes the same path, with the same one line.
  out <- utils::capture.output(short <- get_data(run$plan, "null", 2)$d)
  expect_identical(out, cache_lines(p, rep(NA, 3))[3])
  expect_identical(short, d)
  expect_identical(length(calls(p)), n_calls)

  out <- utils::capture.output(
    one <- run$plan$run_one("real__sim_0001__means__1")
  )
  expect_identical(out, cache_lines(p, rep(NA, 3))[1])
  expect_identical(length(calls(p)), n_calls)
  expect_identical(one, run$r[["real__sim_0001__means__1"]])

  # Without the state of initialize_project(), the call stops.
  local_clean_state()
  expect_error(
    run$plan$get_data("real__sim_0001__means__1"),
    "The data \"d\" has cache = TRUE. Call pbn::initialize_project() before",
    fixed = TRUE
  )
  expect_identical(length(calls(p)), n_calls)
})

test_that("after a change of a data_*.R file, p$get_data() computes again with the reason and writes the cache, and run_plan then reads it", {
  skip_if_not_installed("cohort")
  p <- local_cohort_project()
  cohort_run(p)
  write_lines(
    p$home,
    "v01_functions/data_clean.R",
    c("# A new comment.", cohort_function_files()[["data_clean.R"]])
  )
  why <- "v01_functions/data_clean.R changed"
  built <- cohort_plan(p)
  before <- calls(p)
  out <- utils::capture.output(
    d <- built$plan$get_data("null__sim_0002__means__1")$d
  )
  expect_identical(out, cache_lines(p, c(NA, NA, why))[3])
  expect_identical(calls(p)[-seq_along(before)], "null 2")
  expect_identical(attr(d, "pbn_cache")$from_cache, FALSE)

  before <- calls(p)
  out <- utils::capture.output(r <- run_plan(built$plan))
  expect_identical(out, cache_lines(p, c(why, why, NA)))
  expect_identical(calls(p)[-seq_along(before)], c("real 1", "null 1"))
  expect_identical(
    r[["null__sim_0002__means__1"]],
    cohort_run(p, cache = FALSE)$r[["null__sim_0002__means__1"]]
  )
})

test_that("in a new R session, the run script up to the add_ calls and p$get_data() read the cache of an earlier run", {
  skip_if_not_installed("cohort")
  skip_if_pbn_not_installed()
  p <- local_cohort_project()
  write_lines(p$home, "v01_run.R", cohort_script_lines(p, "r <- pbn::run_plan(p)"))
  first <- run_script(p$home)
  expect_identical(first$status, 0L, info = first$stdout)
  expect_match(first$stdout, "null sim_0002: computed, because there is no cache file.", fixed = TRUE)
  expect_length(calls(p), 3)

  write_lines(
    p$home,
    "v01_run.R",
    cohort_script_lines(p, c(
      "d <- p$get_data(\"null__sim_0002__means__1\")$d",
      "cat(\"rows\", nrow(d), \"\\n\")"
    ))
  )
  second <- run_script(p$home)
  expect_identical(second$status, 0L, info = second$stdout)
  lines <- strsplit(second$stdout, "\n")[[1]]
  expect_identical(
    grep("^pbn: data", lines, value = TRUE),
    cache_lines(p, rep(NA, 3))[3]
  )
  expect_true(any(grepl("^rows [0-9]+ $", lines)))
  expect_length(calls(p), 3)
})

test_that("a raw data file changed after initialize_project() changes the key and the manifest, and an unchanged file is not hashed again", {
  skip_if_not_installed("cohort")
  p <- local_cohort_project()
  run <- cohort_run(p)
  hashed <- new.env()
  hashed$files <- character(0)
  local_mocked_bindings(hash_files = function(files) {
    hashed$files <- c(hashed$files, basename(files))
    return(unname(tools::md5sum(files)))
  })
  # No change: the raw data is not hashed again.
  out <- utils::capture.output(r <- run_plan(run$plan))
  expect_identical(out, cache_lines(p, rep(NA, 3)))
  expect_identical(hashed$files, character(0))

  csv <- file.path(p$data_default, "d.csv")
  write_cohort_data(p$data_default, age = c(16, 16, 16, 30, 40, 50, 60, 25))
  Sys.setFileTime(csv, Sys.time() + 10)
  out <- utils::capture.output(r <- run_plan(run$plan))
  expect_identical(out, cache_lines(p, c("the raw data changed", NA, NA)))
  expect_identical(hashed$files, "d.csv")
  expect_identical(
    attr(r[["real__sim_0001__pbn_n_rows"]], "exclusions")$d$n_excluded,
    3L
  )
  read_md5 <- md5_of(csv)
  # The file changes again after the run, and a step by hand reads it. The
  # manifest of the run still names the data that the run read.
  write_cohort_data(p$data_default, age = c(16, 16, 16, 16, 40, 50, 60, 25))
  Sys.setFileTime(csv, Sys.time() + 15)
  utils::capture.output(run$plan$get_data("real__sim_0001__means__1"))
  expect_false(identical(md5_of(csv), read_md5))
  utils::capture.output(save_results(r, run$plan, run$scenarios))
  m <- read_results("v01", home = p$home)$manifest
  expect_identical(m$data_md5, c(d.csv = read_md5))

  # A new mtime with the same bytes: hashed once, and the cache is used.
  hashed$files <- character(0)
  Sys.setFileTime(csv, Sys.time() + 25)
  out <- utils::capture.output(run_plan(run$plan))
  expect_identical(out, cache_lines(p, rep(NA, 3)))
  expect_identical(hashed$files, "d.csv")
})

test_that("a data_*.R file changed after initialize_project() stops run_plan and p$get_data() with its name", {
  skip_if_not_installed("cohort")
  p <- local_cohort_project()
  run <- cohort_run(p)
  n_calls <- length(calls(p))
  file <- file.path(p$home, "v01_functions", "data_clean.R")
  # A new mtime with the same bytes does not stop the run.
  Sys.setFileTime(file, Sys.time() + 10)
  expect_identical(
    utils::capture.output(run_plan(run$plan)),
    cache_lines(p, rep(NA, 3))
  )
  write_lines(
    p$home,
    "v01_functions/data_clean.R",
    c("# A new comment.", cohort_function_files()[["data_clean.R"]])
  )
  msg <- paste0(
    "These data files changed after pbn::initialize_project() ran: ",
    "v01_functions/data_clean.R. This R session may hold the old code. Run ",
    "pbn::initialize_project() again."
  )
  expect_error(run_plan(run$plan), paste(msg, "pbn ran nothing."), fixed = TRUE)
  expect_error(
    run$plan$get_data("real__sim_0001__means__1"),
    paste(msg, "pbn computed nothing."),
    fixed = TRUE
  )
  expect_identical(length(calls(p)), n_calls)
  write_lines(p$home, "v01_functions/data_extra.R", "# More data code.")
  expect_error(
    run_plan(run$plan),
    "ran: v01_functions/data_clean.R, v01_functions/data_extra.R.",
    fixed = TRUE
  )
  # initialize_project() again: the run computes the data with the reason.
  again <- cohort_run(p)
  expect_identical(
    again$out,
    cache_lines(p, rep("v01_functions/data_clean.R, v01_functions/data_extra.R changed", 3))
  )
})

test_that("the kind of random number generator is a part of the key", {
  skip_if_not_installed("cohort")
  p <- local_cohort_project()
  cohort_run(p)
  old <- RNGkind()
  withr::defer(RNGkind(old[1], old[2], old[3]))
  RNGkind("L'Ecuyer-CMRG")
  run <- cohort_run(p)
  expect_identical(
    run$out,
    cache_lines(p, rep("the kind of random number generator changed", 3))
  )
})

test_that("the key file records the loaded packages after the data function, and a package with another installed version computes the data again", {
  skip_if_not_installed("cohort")
  p <- local_cohort_project()
  cohort_run(p)
  key_file <- cache_file(p, "null", 1, key = TRUE)
  record <- qs2::qs_read(key_file)$packages
  expect_true(all(c("cohort", "data.table", "R6") %in% names(record)))
  expect_false(any(c("pbn", "plnr", "testthat", "qs2", "stats", "utils") %in% names(record)))
  expect_identical(record[["cohort"]], as.character(utils::packageVersion("cohort")))
  # Each version is in the form of utils::packageVersion(), so an unchanged
  # package compares equal.
  expect_identical(
    unname(record),
    vapply(names(record), function(n) as.character(utils::packageVersion(n)), "", USE.NAMES = FALSE)
  )

  for (s in list(c("real", 1), c("null", 1), c("null", 2))) {
    f <- cache_file(p, s[1], as.integer(s[2]), key = TRUE)
    k <- qs2::qs_read(f)
    k$packages[["cohort"]] <- "1.0.0"
    if (s[1] == "null" && s[2] == "2") {
      k$packages[["pbnnotapackage"]] <- "1.0"
    }
    qs2::qs_save(k, f)
  }
  run <- cohort_run(p)
  expect_identical(
    run$out,
    cache_lines(p, c(
      "the version of cohort changed",
      "the version of cohort changed",
      "the version of cohort, the version of pbnnotapackage changed"
    ))
  )
  expect_identical(
    qs2::qs_read(key_file)$packages[["cohort"]],
    as.character(utils::packageVersion("cohort"))
  )
})

test_that("on a worker, the record holds the packages that the data function loaded there", {
  skip_if_not_installed("cohort")
  skip_if_pbn_not_installed()
  p <- local_cohort_project()
  run <- cohort_run(p, workers = 2)
  for (f in c(cache_file(p, "real", 1, key = TRUE), cache_file(p, "null", 1:2, key = TRUE))) {
    expect_true("cohort" %in% names(qs2::qs_read(f)$packages), info = f)
  }
  expect_identical(cohort_run(p)$out, cache_lines(p, rep(NA, 3)))
})
