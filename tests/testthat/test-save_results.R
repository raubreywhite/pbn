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
      "data_files",
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
  csv <- file.path(p$data_default, "d.csv")
  expect_identical(
    m$data_files,
    data.frame(
      file = "main/d.csv",
      size = as.numeric(file.size(csv)),
      mtime = floor(as.numeric(file.mtime(csv)))
    )
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
  expect_false("data_files" %in% names(f))
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

test_that("a kind whose analyses return a named list of data.frames is a named list of row-bound data.frames, with the dimensions first", {
  p <- local_project()
  sourced <- init(p)
  sc <- scenarios(sourced$CFG)
  plan <- build_plan(sourced, sc)
  add_analyses(
    plan,
    sc,
    "sem",
    fn = function(data, argset) {
      return(list(
        estimates = data.frame(term = c("a", "b"), estimate = c(1, 2)),
        fit = data.frame(cfi = 0.9)
      ))
    },
    adjust = c("x", "none")
  )
  # Other names in two analyses: a list keyed by <name>__<i>, as before.
  add_analyses(
    plan,
    sc,
    "mixed",
    fn = function(data, argset) {
      out <- list(data.frame(v = 1))
      names(out) <- if (argset$adjust == "x") "a" else "b"
      return(out)
    },
    adjust = c("x", "none")
  )
  # The same names, and values that are not data.frames: as before.
  add_analyses(
    plan,
    sc,
    "counts",
    fn = function(data, argset) return(list(n = 1L, m = 2L)),
    adjust = c("x", "none")
  )
  r <- run_plan(plan)
  utils::capture.output(save_results(r, plan, sc))
  real <- read_results("v01", home = p$home)
  expect_identical(
    real$counts,
    list(counts__1 = list(n = 1L, m = 2L), counts__2 = list(n = 1L, m = 2L))
  )
  expect_identical(names(real$sem), c("estimates", "fit"))
  expect_identical(
    real$sem$estimates,
    data.frame(
      adjust = rep(c("x", "none"), each = 2),
      term = c("a", "b", "a", "b"),
      estimate = c(1, 2, 1, 2)
    )
  )
  expect_identical(
    real$sem$fit,
    data.frame(adjust = c("x", "none"), cfi = 0.9)
  )
  expect_identical(
    real$mixed,
    list(
      mixed__1 = list(a = data.frame(v = 1)),
      mixed__2 = list(b = data.frame(v = 1))
    )
  )
  # The stacker adds the column sim to each data.frame of the list.
  null <- read_results("v01", "null", home = p$home)
  expect_identical(
    null$sem$fit,
    data.frame(
      sim = rep(1:3, each = 2),
      adjust = rep(c("x", "none"), 3),
      cfi = 0.9
    )
  )
  expect_identical(nrow(null$sem$estimates), 12L)
  expect_identical(unique(check_results(null)$status), "PASS")
  found <- project_findings(p$home)
  expect_identical(found$status[found$rule == "results_contract"], "PASS")
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
  expect_identical(no_log(results_files(p)), character(0))
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
  expect_identical(
    conditionMessage(err),
    paste0(
      "1 of 4 assertions failed, in the scenarios known. pbn printed each ",
      "failed assertion above. Each scenario has its assertions in ",
      file.path(p$home, "v01_results", "<scenario>", "check.csv"),
      "."
    )
  )
  expect_identical(
    out[match("pbn: these assertions failed:", out) + 1],
    "  known: events of x in group a (expected 5, observed 4)"
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

test_that("save_results prints every failed assertion in full, and its error stays below 1000 bytes", {
  long <- paste("the estimate of a model with a long name", strrep("x", 60))
  checks <- data.frame(
    scenario = rep(c("null", "confounded"), each = 26),
    assertion = paste(long, 1:52),
    expected = "0",
    observed = "1",
    pass = FALSE
  )
  out <- utils::capture.output(
    err <- tryCatch(
      stop_on_failed_checks(checks, "/x/v01_results"),
      error = function(e) return(e)
    )
  )
  expect_identical(
    out,
    c(
      "pbn: these assertions failed:",
      paste0(
        "  ",
        checks$scenario,
        ": ",
        checks$assertion,
        " (expected 0, observed 1)"
      )
    )
  )
  expect_identical(
    conditionMessage(err),
    paste0(
      "52 of 52 assertions failed, in the scenarios null, confounded. pbn ",
      "printed each failed assertion above. Each scenario has its assertions ",
      "in /x/v01_results/<scenario>/check.csv."
    )
  )
  expect_lt(nchar(conditionMessage(err), "bytes"), 1000)
  expect_null(stop_on_failed_checks(checks[0, ], "/x/v01_results"))
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
    expect_error(
      quiet_save(run),
      "1 of 3 assertions failed, in the scenarios known.",
      fixed = TRUE,
      info = nm
    )
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
    # real, known, the 3 simulations of null and the stacked null.
    expect_match(
      conditionMessage(err),
      "6 failures of check_results(), printed above. pbn saved nothing.",
      fixed = TRUE,
      info = paste("workers", workers)
    )
    expect_true(
      any(grepl(
        "null/sims_gitexclude/sim_0002.qs2: no_row_per_person: x$copy (a data.frame with 40 rows)",
        out,
        fixed = TRUE
      )),
      info = paste("workers", workers)
    )
    expect_true(
      any(grepl(
        paste0(
          "null stacked: no_row_per_person: x$copy in simulation 1 (a ",
          "data.frame with 40 rows), x$copy in simulation 2 (a data.frame ",
          "with 40 rows), x$copy in simulation 3 (a data.frame with 40 rows)"
        ),
        out,
        fixed = TRUE
      )),
      info = paste("workers", workers)
    )
    expect_true(any(grepl("real: no_row_per_person", out, fixed = TRUE)))
    # No results file, and the run log with the failed rules.
    expect_identical(results_files(p), log_files)
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
  expect_identical(no_log(results_files(p)), character(0))

  local_clean_state()
  expect_error(
    save_results(run$r, run$plan, run$scenarios),
    "Call pbn::initialize_project() before pbn::save_results().",
    fixed = TRUE
  )
})

test_that("save_results stops for real when a folder did not exist, names it, and saves the fake scenarios", {
  p <- local_project()
  absent <- file.path(p$root, "absent")
  sourced <- init(p, folders = c(main = p$data_default, lookup = absent))
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
    paste0(
      "The folder lookup (",
      absent,
      ") did not exist when initialize_project() ran, so pbn cannot save ",
      "the scenario \"real\". pbn saved nothing."
    ),
    fixed = TRUE
  )
  expect_false(file.exists(file.path(p$home, "v01_results", "real")))
})

# Sets the simulations of null in the functions folder of local_project(),
# with an expect_null() that passes for any number of simulations.
set_null_sims <- function(p, sims) {
  cfg <- file.path(p$home, "v01_functions", "_cfg.R")
  lines <- sub("sims = c(1, 1, 3)", paste0("sims = c(1, 1, ", sims, ")"), readLines(cfg), fixed = TRUE)
  writeLines(lines, cfg)
  write_lines(
    p$home,
    "v01_functions/expect_null.R",
    "expect_null <- function(results) data.frame(assertion = 'a', expected = 1, observed = 1, pass = TRUE)"
  )
  return(invisible(NULL))
}

test_that("save_results empties sims_gitexclude/ of an earlier run with more simulations, and read_results then stops on the removed simulation", {
  p <- local_project()
  first <- run_project(p)
  quiet_save(first)
  folder <- file.path(p$home, "v01_results", "null", "sims_gitexclude")
  set_null_sims(p, 2)
  run <- run_project(p)
  out <- utils::capture.output(save_results(run$r, run$plan, run$scenarios))
  expect_identical(
    grep("sims_gitexclude", out, value = TRUE),
    paste0(
      "pbn: emptied ",
      folder,
      "/ before it wrote the scenario \"null\": it held 3 files of an ",
      "earlier run."
    )
  )
  expect_identical(
    sort(list.files(folder)),
    c("sim_0001.qs2", "sim_0002.qs2")
  )
  expect_identical(read_results("v01", "null", home = p$home)$manifest$sims, 2L)
  expect_identical(
    read_results("v01", "null", sim = 2, home = p$home)$manifest$sims,
    2L
  )
  expect_error(
    read_results("v01", "null", sim = 3, home = p$home),
    "There is no results file of the simulation 3",
    fixed = TRUE
  )
})

test_that("save_results removes sims_gitexclude/ when the scenario now has 1 simulation, and read_results(sim = 2) then stops", {
  p <- local_project()
  first <- run_project(p)
  quiet_save(first)
  folder <- file.path(p$home, "v01_results", "null", "sims_gitexclude")
  set_null_sims(p, 1)
  run <- run_project(p)
  out <- utils::capture.output(save_results(run$r, run$plan, run$scenarios))
  expect_identical(
    grep("sims_gitexclude", out, value = TRUE),
    paste0(
      "pbn: deleted ",
      folder,
      "/, because the scenario \"null\" now has 1 simulation: it held 3 ",
      "files of an earlier run."
    )
  )
  expect_false(dir.exists(folder))
  expect_identical(
    read_results("v01", "null", sim = 1, home = p$home),
    read_results("v01", "null", home = p$home)
  )
  expect_identical(read_results("v01", "null", home = p$home)$manifest$sims, 1L)
  expect_error(
    read_results("v01", "null", sim = 2, home = p$home),
    "There is no results file of the simulation 2",
    fixed = TRUE
  )
})

test_that("save_results deletes the folders sim_NNNN/ of the old layout and prints one line", {
  p <- local_project()
  run <- run_project(p)
  folder <- file.path(p$home, "v01_results", "null")
  for (k in c("sim_0001", "sim_0002", "sim_0005")) {
    dir.create(file.path(folder, k), recursive = TRUE)
    qs2::qs_save(list(a = 1), file.path(folder, k, "results.qs2"))
    writeLines("x", file.path(folder, k, "run.log"))
  }
  # A file with such a name is not a folder of the old layout.
  writeLines("x", file.path(folder, "sim_0009"))
  out <- utils::capture.output(save_results(run$r, run$plan, run$scenarios))
  expect_identical(
    grep("old layout", out, value = TRUE),
    paste0(
      "pbn: deleted the 3 folders sim_<kkkk>/ of the old layout in ",
      folder,
      ". The results of each simulation are now in sims_gitexclude/."
    )
  )
  expect_identical(
    results_files(p),
    sort(c(setdiff(expected_tree, "real/output.txt"), "null/sim_0009"))
  )
  # Without an old folder, no line.
  out <- utils::capture.output(save_results(run$r, run$plan, run$scenarios))
  expect_identical(grep("old layout", out, value = TRUE), character(0))
})

test_that("in a git repository, save_results stops until git ignores sims_gitexclude/", {
  p <- local_project()
  git_in(p$home, "init", "-q")
  write_lines(p$home, ".gitignore", "*.log")
  run <- run_project(p)
  expect_error(
    quiet_save(run),
    "git does not ignore v01_results/null/sims_gitexclude/sim_0001.qs2",
    fixed = TRUE
  )
  expect_identical(no_log(results_files(p)), character(0))
  # A .gitignore that ignores the file of simulation 1 only.
  write_lines(
    p$home,
    ".gitignore",
    "v01_results/null/sims_gitexclude/sim_0001.qs2"
  )
  expect_error(
    quiet_save(run),
    paste0(
      "git does not ignore v01_results/null/sims_gitexclude/sim_0002.qs2 ",
      "and v01_results/null/sims_gitexclude/sim_0003.qs2."
    ),
    fixed = TRUE
  )
  expect_identical(no_log(results_files(p)), character(0))
  # The files of the kinds of gitexclude of each simulation too.
  write_lines(
    p$home,
    ".gitignore",
    c("results_gitexclude.qs2", "sim_000[0-9].qs2")
  )
  expect_error(
    utils::capture.output(
      save_results(run$r, run$plan, run$scenarios, gitexclude = "models")
    ),
    paste0(
      "git does not ignore v01_results/null/sims_gitexclude/sim_0001_gitexclude.qs2, ",
      "v01_results/null/sims_gitexclude/sim_0002_gitexclude.qs2 and ",
      "v01_results/null/sims_gitexclude/sim_0003_gitexclude.qs2."
    ),
    fixed = TRUE
  )
  expect_identical(no_log(results_files(p)), character(0))
  write_lines(p$home, ".gitignore", "*gitexclude*")
  quiet_save(run)
  expect_identical(results_files(p), setdiff(expected_tree, "real/output.txt"))
  # git sees the stacked file and check.csv of null, and no file of a
  # simulation.
  seen <- git_in(p$home, "ls-files", "--others", "--exclude-standard")
  expect_identical(
    sort(grep("^v01_results/null/", seen, value = TRUE)),
    c("v01_results/null/check.csv", "v01_results/null/results.qs2")
  )
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
  expect_identical(no_log(results_files(p)), character(0))
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
  expect_identical(no_log(results_files(p)), character(0))
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
  expect_error(
    quiet_save(run),
    "1 of 4 assertions failed, in the scenarios null.",
    fixed = TRUE
  )
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
    "sims_gitexclude/sim_0002.qs2. Fix the analysis",
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
  out <- utils::capture.output(expect_error(
    save_results(run$r, run$plan, run$scenarios),
    "1 of 3 assertions failed, in the scenarios real.",
    fixed = TRUE
  ))
  expect_true(
    "  real: expect_real() returns at least one assertion (expected see the assertion, observed 0 rows)" %in%
      out
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
  expect_error(
    quiet_save(run),
    "1 of 2 assertions failed, in the scenarios known.",
    fixed = TRUE
  )
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

test_that("read_results with sim = 1 reads the results.qs2 of a scenario with 1 simulation", {
  p <- local_project()
  run <- run_project(p)
  quiet_save(run)
  for (s in c("real", "known")) {
    expect_identical(
      read_results("v01", s, sim = 1, home = p$home),
      read_results("v01", s, home = p$home),
      info = s
    )
  }
  expect_error(
    read_results("v01", "known", sim = 2, home = p$home),
    "There is no results file of the simulation 2",
    fixed = TRUE
  )
  # A stacked scenario reads the file of simulation 1, and stops without it.
  expect_identical(
    read_results("v01", "null", sim = 1, home = p$home)$manifest$sim,
    1L
  )
  unlink(file.path(p$home, "v01_results", "null", "sims_gitexclude"), recursive = TRUE)
  expect_error(
    read_results("v01", "null", sim = 1, home = p$home),
    "There is no results file of the simulation 1",
    fixed = TRUE
  )
})
