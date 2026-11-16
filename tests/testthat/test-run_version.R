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

test_that("a data_raw argument in any form leaves the real results.qs2 byte-identical", {
  skip_if_pbn_not_installed()
  p <- local_project()
  other <- write_data(
    file.path(p$root, "data_other"),
    x = c(1, 1, 1, 1, 1, 0, 0, 0, 0, 0),
    y = 11:20
  )
  dir.create(file.path(other, "sub"))
  # The parent folder of home holds a d.csv too, so a run on `..` reaches
  # save_results() unless initialize_project() stops it.
  write_data(p$root, x = c(0, 0, 0, 0, 0, 1, 1, 1, 1, 1), y = 21:30)
  real <- run_version("v01", home = p$home)
  md5_real <- md5_of(real)
  scenario <- file.path(
    p$home,
    "v01_results",
    "scenario",
    "data_other",
    "results.qs2"
  )

  for (arg in c("../data_other/sub/..", "../data_other/", "../data_other")) {
    unlink(scenario)
    res <- run_script_raw(p$home, arg)
    expect_identical(res$status, 0L, info = arg)
    expect_true(file.exists(scenario), info = arg)
    owner <- if (file.exists(scenario)) {
      qs2::qs_read(scenario)$manifest$data_raw
    } else {
      NA_character_
    }
    expect_identical(owner, normalizePath(other, winslash = "/"), info = arg)
    expect_identical(md5_of(real), md5_real, info = arg)
  }

  forbidden <- c(
    ".." = "or holds it",
    "." = "or holds it",
    "v01_results" = "or holds it",
    "v01_results/" = "or holds it",
    "../data_default" = "is the default data folder",
    "../data_default/" = "is the default data folder"
  )
  for (arg in names(forbidden)) {
    res <- run_script_raw(p$home, arg)
    expect_identical(res$status, 1L, info = arg)
    expect_match(res$stdout, forbidden[[arg]], fixed = TRUE, info = arg)
    expect_identical(md5_of(real), md5_real, info = arg)
  }
  expect_identical(
    sort(list.files(file.path(p$home, "v01_results", "scenario"))),
    "data_other"
  )
})

test_that("a scenario folder that links to the real results folder stops the run", {
  skip_if_pbn_not_installed()
  skip_on_os("windows")
  p <- local_project()
  real <- run_version("v01", home = p$home)
  md5_real <- md5_of(real)
  linked <- write_data(
    file.path(p$root, "data_link"),
    x = c(1, 1, 1, 1, 1, 0, 0, 0, 0, 0),
    y = 11:20
  )
  dir.create(file.path(p$home, "v01_results", "scenario"))
  file.symlink(
    file.path(p$home, "v01_results"),
    file.path(p$home, "v01_results", "scenario", "data_link")
  )

  res <- run_script_raw(p$home, linked)
  expect_identical(res$status, 1L)
  expect_match(
    res$stdout,
    "The scenario results folder",
    fixed = TRUE
  )
  expect_match(res$stdout, "is the real results folder", fixed = TRUE)
  expect_identical(md5_of(real), md5_real)
})

test_that("data_md5 lists only the files that existed at initialize_project()", {
  skip_if_pbn_not_installed()
  p <- local_project()
  # The run writes two outputs before save_results(): one into the results
  # folder and one into the data folder.
  lines <- append(
    run_script_lines("v01", p$data_default),
    c(
      "writeLines(\"x\", file.path(p$results, \"fig.txt\"))",
      "writeLines(\"x\", file.path(p$data_raw, \"made_by_run.txt\"))"
    ),
    after = 1
  )
  writeLines(lines, file.path(p$home, "v01_run.R"))

  # The data folder is the scenario results folder itself.
  same <- file.path(p$home, "v01_results", "scenario", "same")
  write_data(same, x = c(1, 1, 1, 1, 1, 0, 0, 0, 0, 0), y = 11:20)
  file <- run_version("v01", data_raw = same, home = p$home)
  expect_identical(
    normalizePath(dirname(file)),
    normalizePath(same)
  )
  r <- qs2::qs_read(file)
  expect_identical(r$manifest$data_raw, normalizePath(same, winslash = "/"))
  expect_identical(names(r$manifest$data_md5), "d.csv")
  expect_identical(
    unname(r$manifest$data_md5),
    md5_of(file.path(same, "d.csv"))
  )
  expect_true(all(file.exists(file.path(
    same,
    c("fig.txt", "made_by_run.txt")
  ))))

  # A real run hashes at the same time.
  real <- qs2::qs_read(run_version("v01", home = p$home))
  expect_true(file.exists(file.path(p$data_default, "made_by_run.txt")))
  expect_identical(names(real$manifest$data_md5), "d.csv")
})

test_that("two data folders with one basename cannot share a scenario folder", {
  skip_if_pbn_not_installed()
  p <- local_project()
  a <- normalizePath(
    write_data(
      file.path(p$root, "a", "same"),
      x = c(1, 1, 1, 1, 1, 0, 0, 0, 0, 0),
      y = 11:20
    ),
    winslash = "/"
  )
  b <- normalizePath(
    write_data(
      file.path(p$root, "b", "same"),
      x = c(0, 0, 0, 0, 0, 1, 1, 1, 1, 1),
      y = 21:30
    ),
    winslash = "/"
  )
  file <- run_version("v01", data_raw = a, home = p$home)
  md5_a <- md5_of(file)

  err <- tryCatch(
    run_version("v01", data_raw = b, home = p$home),
    error = function(e) return(e)
  )
  expect_s3_class(err, "error")
  msg <- if (inherits(err, "error")) conditionMessage(err) else ""
  expect_match(msg, "holds the results of the data folder", fixed = TRUE)
  expect_match(msg, a, fixed = TRUE)
  expect_match(msg, b, fixed = TRUE)
  expect_identical(md5_of(file), md5_a)

  # The same data folder again is the same scenario, so it runs.
  expect_identical(run_version("v01", data_raw = a, home = p$home), file)
})

test_that("the manifest packages name qs2, the package that writes the file", {
  skip_if_pbn_not_installed()
  p <- local_project()
  r <- qs2::qs_read(run_version("v01", home = p$home))
  expect_true("qs2" %in% names(r$manifest$packages))
  expect_identical(
    unname(r$manifest$packages["qs2"]),
    as.character(utils::packageVersion("qs2"))
  )
})

test_that("run_date is the date only, so a second run writes the same bytes", {
  skip_if_pbn_not_installed()
  p <- local_project()
  file <- run_version("v01", home = p$home)
  first <- md5_of(file)
  expect_match(
    qs2::qs_read(file)$manifest$run_date,
    "^[0-9]{4}-[0-9]{2}-[0-9]{2}$"
  )
  # A run takes less than one second. Wait, so that a date-time to the
  # second would differ between the two runs.
  Sys.sleep(1.1)
  run_version("v01", home = p$home)
  expect_identical(md5_of(file), first)
})
