# save_results() on the plan of the synthetic project, built and run in this
# R session from the functions folder sourced into a new environment.

run_project <- function(p, only = NULL, workers = 1) {
  sourced <- init(p, env = parent.frame())
  sc <- if (is.null(only)) {
    scenarios(sourced$CFG)
  } else {
    scenarios(sourced$CFG, only = only)
  }
  plan <- build_plan(sourced, sc)
  r <- run_plan(plan, workers = workers)
  return(list(sourced = sourced, scenarios = sc, plan = plan, r = r))
}

quiet_save <- function(run) {
  out <- NULL
  utils::capture.output(out <- save_results(run$r, run$plan, run$scenarios))
  return(invisible(out))
}

test_that("save_results writes one object for each scenario and simulation, with one element for each kind, and stacks null", {
  p <- local_project()
  run <- run_project(p)
  checks <- quiet_save(run)
  expect_identical(results_files(p), setdiff(expected_tree, "real/output.txt"))
  expect_identical(
    checks$scenario,
    c("known", "known", "null", "null")
  )
  expect_true(all(checks$pass))

  real <- read_results("v01", home = p$home)
  expect_identical(names(real), c("manifest", "table1", "models"))
  expect_identical(real$table1, run$r[["real__sim_0001__table1__1"]])
  expect_s3_class(real$table1, "data.table")
  # The extra dimension comes first, the rows in the order of the plan.
  models <- run$r[paste0("real__sim_0001__models__", 1:2)]
  expect_identical(
    real$models,
    rbind(
      data.frame(outcome = "y", models[[1]]),
      data.frame(outcome = "z", models[[2]])
    )
  )

  known <- read_results("v01", "known", home = p$home)
  expect_identical(
    names(known),
    c("manifest", "table1", "models", "models_crude")
  )
  expect_identical(names(known$models_crude)[1:2], c("outcome", "adjust"))
  expect_identical(unique(known$models_crude$adjust), "none")

  null2 <- read_results("v01", "null", sim = 2, home = p$home)
  stacked <- read_results("v01", "null", home = p$home)
  expect_identical(stacked$models$sim, rep(1:3, each = 6))
  sim2 <- stacked$models[stacked$models$sim == 2, -1, drop = FALSE]
  rownames(sim2) <- NULL
  expect_identical(sim2, null2$models)
})

test_that("the manifest names the run, the data, the rows of the data and the analysis kinds", {
  p <- local_project()
  run <- run_project(p)
  quiet_save(run)
  m <- read_results("v01", home = p$home)$manifest
  expect_identical(
    names(m),
    c(
      "pbn_version",
      "r_version",
      "packages",
      "run_date",
      "version",
      "scenario",
      "sim",
      "sims",
      "settings",
      "source_md5",
      "data_md5",
      "n_rows_data",
      "n_rows_before_exclusions",
      "n_rows_after_exclusions",
      "analyses",
      "data_cache"
    )
  )
  # The data of this project is no cohort.
  expect_identical(m$n_rows_before_exclusions, c(d = NA_integer_))
  expect_identical(m$n_rows_after_exclusions, c(d = NA_integer_))
  expect_identical(m$pbn_version, as.character(utils::packageVersion("pbn")))
  expect_identical(m$run_date, format(Sys.Date(), "%Y-%m-%d"))
  expect_true("qs2" %in% names(m$packages))
  expect_identical(
    m[c("version", "scenario", "sim", "sims")],
    list(
      version = "v01",
      scenario = "real",
      sim = 1L,
      sims = 1L
    )
  )
  expect_identical(m$settings, list(n_boot = 200))
  expect_identical(
    m$data_md5,
    c(d.csv = md5_of(file.path(p$data_default, "d.csv")))
  )
  expect_identical(m$n_rows_data, c(d = 10L))
  expect_identical(
    m$analyses,
    data.frame(
      analysis = c("table1", "models"),
      control = FALSE,
      file = "results.qs2"
    )
  )
  expect_identical(
    m$data_cache,
    data.frame(data = "d", cache = FALSE, from_cache = FALSE, cache_key = NA_character_)
  )

  f <- read_results("v01", "null", sim = 3, home = p$home)$manifest
  expect_false("data_md5" %in% names(f))
  expect_identical(f$fake_md5, function_md5(plan_info(run$plan)$data))
  expect_identical(f$n_rows_data, c(d = 40L))
  expect_identical(f$sim, 3L)
  expect_identical(f$sims, 3L)
  expect_identical(
    f$analyses,
    data.frame(
      analysis = c("table1", "models", "models_crude"),
      control = c(FALSE, FALSE, TRUE),
      file = "results.qs2"
    )
  )
})

test_that("a kind with values other than data.frames is a list keyed by <name>__<i>, or the one value", {
  p <- local_project()
  sourced <- init(p)
  sc <- scenarios(sourced$CFG, only = c("real", "known"))
  plan <- plnr::Plan$new()
  add_data(plan, "d", fn = function(scenario, sim) {
    sourced$get_data(scenario, sim)
  })
  add_analyses(
    plan,
    sc,
    "means",
    fn = function(data, argset) {
      return(mean(data$d[[argset$outcome]]))
    },
    outcome = c("y", "z")
  )
  add_analyses(plan, sc, "counts", fn = function(data, argset) {
    return(list(rows = nrow(data$d) - 1L, groups = 2L))
  })
  # A data.frame that has the column of the dimension already.
  add_analyses(
    plan,
    sc,
    "own",
    fn = function(data, argset) {
      return(data.frame(outcome = toupper(argset$outcome)))
    },
    outcome = c("y", "z")
  )
  r <- run_plan(plan)
  sourced$expect_known <- function(results) {
    return(data.frame(assertion = "a", expected = 1, observed = 1, pass = TRUE))
  }
  utils::capture.output(save_results(r, plan, sc))
  known <- read_results("v01", "known", home = p$home)
  expect_identical(known$means, list(means__1 = 2, means__2 = 5.5))
  expect_identical(known$counts, list(rows = 9L, groups = 2L))
  expect_identical(known$own, data.frame(outcome = c("Y", "Z")))
})

test_that("a missing expect_ function for a fake scenario stops before anything is saved", {
  p <- local_project()
  run <- run_project(p)
  rm("expect_null", envir = run$sourced)
  expect_error(
    quiet_save(run),
    paste0(
      "Each fake scenario needs a function expect_<scenario>(results) in ",
      "v01_functions/. These are missing: expect_null(). pbn saved nothing."
    ),
    fixed = TRUE
  )
  expect_false(dir.exists(file.path(p$home, "v01_results")))
  # expect_real() is optional.
  expect_null(expect_function("real"))
})

test_that("a failed assertion stops after every scenario, and check_project then fails scenario_checks", {
  p <- local_project()
  write_lines(p$home, "v01_functions/expect_known.R", expect_known_lines(5))
  run <- run_project(p)
  out <- utils::capture.output(
    err <- tryCatch(
      save_results(run$r, run$plan, run$scenarios),
      error = function(e) e
    )
  )
  expect_match(
    conditionMessage(err),
    "These assertions failed:\n  known: events of x in group a (expected 5, observed 4)",
    fixed = TRUE
  )
  # null came after known, and it was checked and written too.
  expect_identical(results_files(p), setdiff(expected_tree, "real/output.txt"))
  expect_true(any(grepl(
    "Assertions of the scenario \"null\"",
    out,
    fixed = TRUE
  )))
  known <- utils::read.csv(file.path(
    p$home,
    "v01_results",
    "known",
    "check.csv"
  ))
  expect_identical(known$pass, c(FALSE, TRUE))
  found <- project_findings(p$home)
  expect_identical(failed_rules(found), "scenario_checks")
})

test_that("an expect_ function that errors, returns zero rows or a wrong shape is a failed assertion", {
  bodies <- list(
    error = "stop('planted')",
    zero = "data.frame(assertion = character(0), expected = 1[0], observed = 1[0], pass = logical(0))",
    shape = "list(pass = TRUE)"
  )
  observed <- c(
    error = "planted",
    zero = "0 rows",
    shape = "an object of class list"
  )
  for (nm in names(bodies)) {
    p <- local_project()
    write_lines(
      p$home,
      "v01_functions/expect_known.R",
      paste0("expect_known <- function(results) ", bodies[[nm]])
    )
    run <- run_project(p, only = c("known", "null"))
    expect_error(quiet_save(run), "These assertions failed", info = nm)
    found <- utils::read.csv(
      file.path(p$home, "v01_results", "known", "check.csv")
    )
    expect_identical(found$pass, FALSE, info = nm)
    expect_identical(found$observed, observed[[nm]], info = nm)
  }
})

test_that("a result with one row for each row of the data fails no_row_per_person and saves nothing, also with 2 workers", {
  skip_if_pbn_not_installed()
  for (workers in 1:2) {
    p <- local_project()
    sourced <- init(p)
    sc <- scenarios(sourced$CFG)
    plan <- build_plan(sourced, sc)
    copy <- function(data, argset) {
      return(data.frame(row = seq_len(nrow(data$d))))
    }
    environment(copy) <- globalenv()
    add_analyses(plan, sc, "copy", fn = copy)
    r <- run_plan(plan, workers = workers)
    expect_identical(r[["null__sim_0002__pbn_n_rows"]], c(d = 40L))
    out <- utils::capture.output(
      err <- tryCatch(save_results(r, plan, sc), error = function(e) e)
    )
    expect_match(
      conditionMessage(err),
      "5 failures of check_results(), printed above. pbn saved nothing.",
      fixed = TRUE,
      info = paste("workers", workers)
    )
    expect_true(
      any(grepl(
        "null/sim_0002: no_row_per_person: x$copy (a data.frame with 40 rows)",
        out,
        fixed = TRUE
      )),
      info = paste("workers", workers)
    )
    expect_true(any(grepl("real: no_row_per_person", out, fixed = TRUE)))
    expect_false(dir.exists(file.path(p$home, "v01_results")))
  }
})

test_that("save_results stops on bad input and saves nothing", {
  p <- local_project()
  run <- run_project(p)
  expect_error(
    save_results(run$r, run$plan, run$scenarios, gitexclude = "x"),
    "These names in `gitexclude` are not analysis kinds of the plan: x.",
    fixed = TRUE
  )
  expect_error(
    save_results(run$r[-1], run$plan, run$scenarios),
    "`r` must be the value of pbn::run_plan(p)",
    fixed = TRUE
  )
  expect_error(
    save_results(run$r, run$plan, scenarios(run$sourced$CFG, only = "null")),
    "In the plan only: real 1, known 1. In `scenarios` only: [.]$"
  )
  bad <- run$r
  bad[["known__sim_0001__models__2"]] <- structure(
    list(message = "planted"),
    class = "pbn_failed_analysis"
  )
  expect_error(
    utils::capture.output(save_results(bad, run$plan, run$scenarios)),
    "1 of 28 analyses failed"
  )
  expect_error(
    save_results(run$r, plnr::Plan$new(), run$scenarios),
    "needs a plan with data from pbn::add_data()",
    fixed = TRUE
  )
  run$plan$add_analysis(name = "mine", fn = function(data, argset) 1)
  more <- c(run$r, list(mine = 1))
  attr(more, "pbn_run") <- attr(run$r, "pbn_run")
  expect_error(
    save_results(more, run$plan, run$scenarios),
    "did not add these analyses of the plan, so pbn saved nothing: mine"
  )
  expect_false(dir.exists(file.path(p$home, "v01_results")))

  local_clean_state()
  expect_error(
    save_results(run$r, run$plan, run$scenarios),
    "Call pbn::initialize_project() before pbn::save_results().",
    fixed = TRUE
  )
})

test_that("save_results stops for real when data_raw did not exist, and saves the fake scenarios", {
  p <- local_project()
  sourced <- init(p, data_raw = file.path(p$root, "absent"))
  sc <- scenarios(sourced$CFG)
  plan <- build_plan(sourced, scenarios(sourced$CFG, only = c("known", "null")))
  r <- run_plan(plan)
  expect_error(
    save_results(r, plan, sc),
    "In `scenarios` only: real 1"
  )
  fake <- scenarios(sourced$CFG, only = c("known", "null"))
  utils::capture.output(save_results(r, plan, fake))
  expect_identical(
    results_files(p),
    setdiff(expected_tree, c("real/output.txt", "real/results.qs2"))
  )
  # With real in the plan, the missing data folder stops before the run is
  # saved.
  real_only <- scenarios(sourced$CFG, only = "real")
  plan3 <- plnr::Plan$new()
  add_data(plan3, "d", fn = function(scenario, sim) data.frame(x = 1:3))
  add_analyses(plan3, real_only, "a", fn = function(data, argset) 1)
  expect_error(
    save_results(run_plan(plan3), plan3, real_only),
    "did not exist when initialize_project() ran, so pbn cannot save the scenario \"real\"",
    fixed = TRUE
  )
  expect_false(file.exists(file.path(p$home, "v01_results", "real")))
})

test_that("results of simulations above sims stop save_results before it saves anything", {
  p <- local_project()
  run <- run_project(p)
  old <- file.path(p$home, "v01_results", "null", "sim_0004")
  dir.create(old, recursive = TRUE)
  qs2::qs_save(list(a = 1), file.path(old, "results.qs2"))
  expect_error(
    quiet_save(run),
    "holds the results of other simulations: sim_0004/results.qs2"
  )
  expect_identical(results_files(p), "null/sim_0004/results.qs2")
})

test_that("a run of one scenario writes only under its folder and leaves the others byte-identical", {
  p <- local_project()
  run <- run_project(p)
  quiet_save(run)
  real <- file_states(file.path(p$home, "v01_results", "real"))
  known <- file_states(file.path(p$home, "v01_results", "known"))
  null_before <- file_states(file.path(p$home, "v01_results", "null"))
  Sys.sleep(1.1)
  run <- run_project(p, only = "null")
  quiet_save(run)
  expect_identical(file_states(file.path(p$home, "v01_results", "real")), real)
  expect_identical(
    file_states(file.path(p$home, "v01_results", "known")),
    known
  )
  null_after <- file_states(file.path(p$home, "v01_results", "null"))
  expect_identical(null_after$md5, null_before$md5)
  expect_true(all(null_after$mtime > null_before$mtime))
})

test_that("save_results stops on a value of run_plan from another plan, or without its stamp, and saves nothing", {
  p <- local_project()
  run <- run_project(p)
  # Another plan with the same analysis names.
  other <- build_plan(run$sourced, run$scenarios)
  r_other <- run_plan(other)
  expect_identical(names(r_other), names(run$r))
  expect_error(
    save_results(r_other, run$plan, run$scenarios),
    "`r` must be the value of pbn::run_plan(p) for the plan `p`. Its stamp names another plan.",
    fixed = TRUE
  )
  bare <- run$r
  attr(bare, "pbn_run") <- NULL
  expect_error(
    save_results(bare, run$plan, run$scenarios),
    "It has no stamp of run_plan().",
    fixed = TRUE
  )
  expect_false(dir.exists(file.path(p$home, "v01_results")))
  # The value of its own plan saves.
  quiet_save(run)
  expect_true(file.exists(file.path(p$home, "v01_results", "real", "results.qs2")))
})

test_that("save_results stops when the settings of `scenarios` are not those of the argsets", {
  p <- local_project()
  run <- run_project(p)
  changed <- run$scenarios
  changed$n_boot[changed$scenario == "null"] <- 50
  expect_error(
    save_results(run$r, run$plan, changed),
    "The settings of these scenarios in `scenarios` are not the settings that add_analyses() put in the argsets: null.",
    fixed = TRUE
  )
  extra <- run$scenarios
  extra$label <- "x"
  expect_error(
    save_results(run$r, run$plan, extra),
    "in the argsets: real, known, null.",
    fixed = TRUE
  )
  expect_false(dir.exists(file.path(p$home, "v01_results")))
})

test_that("read_results stops on results whose check.csv holds a failed assertion, and names how to read them", {
  p <- local_project()
  write_lines(p$home, "v01_functions/expect_null.R", sub(
    "abs(b) < 1",
    "abs(b) > 100",
    expect_null_lines(),
    fixed = TRUE
  ))
  run <- run_project(p)
  expect_error(quiet_save(run), "These assertions failed")
  file <- file.path(p$home, "v01_results", "null", "results.qs2")
  expect_true(file.exists(file))
  expect_error(
    read_results("v01", "null", home = p$home),
    paste0(
      file.path(p$home, "v01_results", "null", "check.csv"),
      " holds an assertion that did not pass, so read_results() does not read ",
      file,
      ". Fix the analysis and run it again. To look at the results for ",
      "debugging, read them with qs2::qs_read(\"",
      file,
      "\")."
    ),
    fixed = TRUE
  )
  expect_error(
    read_results("v01", "null", sim = 2, home = p$home),
    "sim_0002/results.qs2. Fix the analysis",
    fixed = TRUE
  )
  expect_identical(names(qs2::qs_read(file))[1], "manifest")
  # The other scenarios read as before: known has a check.csv that passes,
  # real has none.
  expect_identical(names(read_results("v01", "known", home = p$home))[1], "manifest")
  expect_identical(names(read_results("v01", home = p$home))[1], "manifest")
  # A check.csv without a column pass stops too, and so does one with a
  # header and no assertion, as an interrupted write can leave it.
  csv <- file.path(p$home, "v01_results", "known", "check.csv")
  writeLines("a,b", csv)
  expect_error(
    read_results("v01", "known", home = p$home),
    paste0(csv, " has no column pass that R can read, so read_results() does not read"),
    fixed = TRUE
  )
  writeLines("\"assertion\",\"expected\",\"observed\",\"pass\"", csv)
  expect_error(
    read_results("v01", "known", home = p$home),
    paste0(csv, " holds no assertion, so read_results() does not read"),
    fixed = TRUE
  )
})

test_that("save_results saves a plan whose CFG$scenarios has no setting column", {
  p <- local_project()
  sourced <- init(p)
  sc <- scenarios(list(scenarios = sourced$CFG$scenarios[, c("scenario", "sims")]))
  plan <- build_plan(sourced, sc)
  r <- run_plan(plan)
  utils::capture.output(save_results(r, plan, sc))
  expect_identical(read_results("v01", home = p$home)$manifest$settings, list())
})

test_that("an expect_real() that returns zero assertions is a failed assertion, as for a fake scenario", {
  p <- local_project()
  write_lines(
    p$home,
    "v01_functions/expect_real.R",
    "expect_real <- function(results) data.frame(assertion = character(0), expected = 1[0], observed = 1[0], pass = logical(0))"
  )
  run <- run_project(p, only = c("real", "known"))
  expect_error(
    quiet_save(run),
    "real: expect_real() returns at least one assertion (expected see the assertion, observed 0 rows)",
    fixed = TRUE
  )
  found <- utils::read.csv(file.path(p$home, "v01_results", "real", "check.csv"))
  expect_identical(found$pass, FALSE)
  expect_identical(found$observed, "0 rows")
  expect_error(read_results("v01", home = p$home), "holds an assertion that did not pass")
})

test_that("check.csv is written to a temporary file in its folder and renamed into place", {
  p <- local_project()
  run <- run_project(p, only = "known")
  quiet_save(run)
  csv <- file.path(p$home, "v01_results", "known", "check.csv")
  old <- readLines(csv)
  seen <- new.env()
  seen$from <- character(0)
  # The last of the two renames, the one of the checked file, is kept.
  local_mocked_bindings(rename_file = function(from, to) {
    seen$from <- from
    seen$to <- to
    seen$target_then <- readLines(to)
    seen$from_then <- readLines(from)
    return(file.rename(from, to))
  })
  write_lines(p$home, "v01_functions/expect_known.R", expect_known_lines(5))
  run <- run_project(p, only = "known")
  expect_error(quiet_save(run), "These assertions failed")
  expect_identical(seen$to, csv)
  expect_identical(dirname(seen$from), dirname(csv))
  expect_false(identical(seen$from, csv))
  # At the rename, the reader saw the whole file "not checked yet", and the
  # new file was complete.
  expect_match(seen$target_then[2], "not checked yet", fixed = TRUE)
  expect_identical(seen$from_then, readLines(csv))
  expect_false(identical(readLines(csv), old))
  expect_identical(
    list.files(dirname(csv)),
    c("check.csv", "results.qs2")
  )
})
