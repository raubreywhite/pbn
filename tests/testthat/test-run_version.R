test_that("choose_data_raw reads a trailing data_raw= argument", {
  expect_identical(
    choose_data_raw("/a", c("--x", "data_raw=/b")),
    list(path = "/b", source = "argument")
  )
  expect_identical(
    choose_data_raw("/a", character(0)),
    list(path = "/a", source = "default")
  )
  expect_error(
    choose_data_raw("/a", c("data_raw=/b", "data_raw=/c")),
    "more than once"
  )
})

test_that("initialize_project sources the functions and leaves no dated folder", {
  p <- local_project()
  env <- new.env()
  out <- suppressMessages(
    initialize_project("v01", p$home, p$data_default, env = env)
  )
  expect_identical(
    names(out),
    c("version", "home", "results", "data_raw", "data_raw_source")
  )
  expect_identical(out$data_raw_source, "default")
  expect_identical(out$results, file.path(out$home, "v01_results"))
  expect_true(exists("CFG", envir = env, inherits = FALSE))
  expect_true(exists("double_it", envir = env, inherits = FALSE))
  expect_true(dir.exists(out$results))
  expect_identical(
    list.files(out$results, all.files = TRUE, no.. = TRUE),
    character(0)
  )
})

test_that("initialize_project keeps a dated folder that existed before", {
  p <- local_project()
  old <- file.path(p$home, "v01_results", format(Sys.Date(), "%Y-%m-%d"))
  dir.create(old, recursive = TRUE)
  writeLines("x", file.path(old, "keep.txt"))
  suppressMessages(
    initialize_project("v01", p$home, p$data_default, env = new.env())
  )
  expect_true(file.exists(file.path(old, "keep.txt")))
})

test_that("initialize_project stops on a bad version or a missing data folder", {
  p <- local_project()
  expect_error(initialize_project("V01", p$home, p$data_default), "two digits")
  expect_error(
    initialize_project("v01", p$home, file.path(p$root, "absent")),
    "data_raw folder does not exist"
  )
})

test_that("save_results stops on an object that fails check_results", {
  p <- local_project()
  suppressMessages(
    initialize_project("v01", p$home, p$data_default, env = new.env())
  )
  expect_error(
    save_results(
      exclusions = data.frame(
        step = "a",
        n_before = 10,
        n_excluded = 1,
        n_after = 10
      ),
      estimates = data.frame(term = "x", estimate = 1),
      tables = list()
    ),
    "exclusions_table"
  )
  expect_false(file.exists(file.path(p$home, "v01_results", "results.qs2")))
})

test_that("run_version runs v01 with the default and with a data_raw argument", {
  skip_if_pbn_not_installed()
  p <- local_project()
  other <- write_data(
    file.path(p$root, "data_other"),
    x = c(1, 1, 1, 1, 1, 0, 0, 0, 0, 0),
    y = 11:20
  )

  file <- run_version("v01", home = p$home)
  expect_identical(
    file,
    file.path(normalizePath(p$home), "v01_results", "results.qs2")
  )
  a <- read_results("v01", home = p$home)

  file_b <- run_version("v01", data_raw = other, home = p$home)
  expect_identical(
    file_b,
    file.path(
      normalizePath(p$home),
      "v01_results",
      "scenario",
      "data_other",
      "results.qs2"
    )
  )
  b <- qs2::qs_read(file_b)

  expect_identical(
    names(a),
    c("manifest", "exclusions", "estimates", "tables", "extra")
  )
  expect_identical(check_results(a)$status, rep("PASS", 4))
  expect_identical(check_results(b)$status, rep("PASS", 4))

  expect_identical(a$manifest$data_raw_source, "default")
  expect_identical(b$manifest$data_raw_source, "argument")
  expect_identical(a$manifest$data_raw, p$data_default)
  expect_identical(b$manifest$data_raw, normalizePath(other, winslash = "/"))
  expect_identical(a$manifest$version, "v01")
  expect_identical(a$manifest$n_rows_data, 10L)
  expect_identical(names(a$manifest$data_md5), "d.csv")
  expect_false(identical(a$manifest$data_md5, b$manifest$data_md5))
  expect_true(all(
    c("data.table", "org", "pbn") %in% names(a$manifest$packages)
  ))
  expect_identical(
    a$manifest$packages[["pbn"]],
    as.character(utils::packageVersion("pbn"))
  )
  expect_false("stats" %in% names(a$manifest$packages))

  # Planted: group a holds 3 ones in the default data and 5 in the other.
  ta <- a$tables$table1
  tb <- b$tables$table1
  expect_identical(ta$events[ta$var == "x" & ta$grp == "a"], 3L)
  expect_identical(tb$events[tb$var == "x" & tb$grp == "a"], 5L)
  expect_equal(ta$mean[ta$var == "y" & ta$grp == "All"], 5.5)
  expect_equal(tb$mean[tb$var == "y" & tb$grp == "All"], 15.5)
  expect_equal(a$estimates$estimate, 0.4)
  expect_equal(b$estimates$estimate, 0.5)

  results <- file.path(p$home, "v01_results")
  expect_identical(
    list.files(results, all.files = TRUE, no.. = TRUE),
    c("results.qs2", "scenario")
  )
  expect_identical(
    list.files(dirname(file_b), all.files = TRUE, no.. = TRUE),
    "results.qs2"
  )
  expect_identical(failed_rules(project_findings(p$home)), character(0))
})

test_that("a run with a data_raw argument leaves the real results.qs2 byte-identical", {
  skip_if_pbn_not_installed()
  p <- local_project()
  other <- write_data(
    file.path(p$root, "data_other"),
    x = c(1, 1, 1, 1, 1, 0, 0, 0, 0, 0),
    y = 11:20
  )
  real <- run_version("v01", home = p$home)
  md5_before <- unname(tools::md5sum(real))

  scenario <- run_version("v01", data_raw = other, home = p$home)

  expect_identical(unname(tools::md5sum(real)), md5_before)
  expect_false(identical(scenario, real))
  expect_identical(
    qs2::qs_read(scenario)$manifest$data_raw_source,
    "argument"
  )
  expect_identical(qs2::qs_read(real)$manifest$data_raw_source, "default")
})

test_that("run_version stops with the output of a failing script", {
  skip_if_pbn_not_installed()
  p <- local_project()
  writeLines(
    c("cat(\"before the stop\\n\")", "stop(\"planted failure\")"),
    file.path(p$home, "v01_run.R")
  )
  expect_error(run_version("v01", home = p$home), "planted failure")
  expect_error(run_version("v01", home = p$home), "before the stop")
})
