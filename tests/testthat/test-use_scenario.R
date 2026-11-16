# is_interactive() is mocked to TRUE where a test acts as a person who steps
# through a run script by hand. Rscript sessions are not interactive.

as_person <- function(env = parent.frame()) {
  testthat::local_mocked_bindings(
    is_interactive = function() return(TRUE),
    .env = env
  )
  return(invisible(NULL))
}

# initialize_project() for the synthetic project, with its printed line
# captured. Returns the list that initialize_project() returns.
start <- function(p) {
  out <- NULL
  utils::capture.output(
    out <- suppressMessages(initialize_project(
      "v01",
      p$home,
      p$data_default,
      env = new.env()
    ))
  )
  return(out)
}

# The four data.frames of a valid save_results() call on the data of `p`.
save_now <- function(p) {
  d <- utils::read.csv(file.path(p$data_raw, "d.csv"))
  return(save_results(
    exclusions = data.frame(
      step = "x not missing",
      n_before = nrow(d),
      n_excluded = 0,
      n_after = nrow(d)
    ),
    estimates = data.frame(term = "mean_x", estimate = mean(d$x)),
    tables = list(),
    d = d
  ))
}

# A script file in its own folder, run with Rscript.
run_lines <- function(lines) {
  dir <- tempfile("pbnscript")
  dir.create(dir)
  withr::defer(unlink(dir, recursive = TRUE), envir = parent.frame())
  file <- file.path(dir, "script.R")
  writeLines(lines, file)
  return(callr::rscript(
    file,
    wd = dir,
    show = FALSE,
    stderr = "2>&1",
    fail_on_status = FALSE
  ))
}

use_scenario_msg <- paste0(
  "pbn::use_scenario() is for stepping through a run script by hand in an ",
  "interactive R session. In the run script, the line MUST stay commented out."
)

test_that("the choice of use_scenario lasts for the session: initialize_project run twice stays fake, and real/ does not change", {
  local_clean_session()
  p <- local_project()
  as_person()
  expect_output(
    chosen <- use_scenario("real"),
    "pbn: from now on, pbn::initialize_project() uses the scenario \"real\", simulation 1.",
    fixed = TRUE
  )
  expect_identical(chosen, list(scenario = "real", sim = 1L))
  real <- start(p)
  save_now(real)
  utils::capture.output(expect_invisible(end_if_fake_scenario()))
  real_folder <- file.path(real$home, "v01_results", "real")
  real_before <- file_states(real_folder)
  expect_identical(real_before$file, "results.qs2")

  utils::capture.output(use_scenario("known"))
  first <- start(p)
  # A person runs the initialize_project() line a second time.
  second <- start(p)
  expect_identical(first$scenario, "known")
  expect_identical(second$scenario, "known")
  expect_identical(second$results, file.path(real$home, "v01_results", "known"))
  expect_identical(state$scenario, "known")
  file <- save_now(second)
  expect_identical(
    file,
    file.path(real$home, "v01_results", "known", "results.qs2")
  )
  expect_identical(qs2::qs_read(file)$manifest$scenario, "known")
  expect_error(end_if_fake_scenario(), class = "pbn_end_if_fake_scenario")
  expect_identical(file_states(real_folder), real_before)

  utils::capture.output(use_scenario("real", sim = 1))
  back <- start(p)
  expect_identical(back$scenario, "real")
  expect_identical(back$data_raw, p$data_default)
  expect_identical(file_states(real_folder), real_before)
})

test_that("use_scenario honours sim for a scenario with 3 simulations", {
  local_clean_session()
  p <- local_project()
  as_person()
  utils::capture.output(use_scenario("null", sim = 2))
  out <- start(p)
  expect_identical(out$scenario, "null")
  expect_identical(out$sim, 2L)
  expect_identical(out$sims, 3L)
  expect_identical(
    out$results,
    file.path(out$home, "v01_results", "null", "sim_0002")
  )
  expect_true(file.exists(file.path(out$results, "data", "d.csv")))
})

test_that("use_scenario(\"real\", sim = 1) gives the real scenario", {
  local_clean_session()
  p <- local_project()
  as_person()
  expect_output(
    use_scenario("real", sim = 1),
    "uses the scenario \"real\", simulation 1.",
    fixed = TRUE
  )
  out <- start(p)
  expect_identical(out$scenario, "real")
  expect_identical(out$sim, 1L)
  expect_identical(out$data_raw, p$data_default)
  expect_identical(out$results, file.path(out$home, "v01_results", "real"))
})

test_that("the command line replaces the session choice as a whole, and the choice stays", {
  local_clean_session()
  p <- local_project()
  as_person()
  local_mocked_bindings(
    command_args = function() return(c("scenario=null", "sim=3"))
  )
  utils::capture.output(use_scenario("known"))
  out <- start(p)
  expect_identical(out$scenario, "null")
  expect_identical(out$sim, 3L)

  # Without the command line, the session choice is used again.
  local_mocked_bindings(command_args = function() return(character(0)))
  expect_identical(start(p)$scenario, "known")

  # sim= alone also replaces the whole choice.
  local_mocked_bindings(command_args = function() return("sim=1"))
  utils::capture.output(use_scenario("null", sim = 2))
  out <- start(p)
  expect_identical(out$scenario, "real")
  expect_identical(out$sim, 1L)
})

test_that("use_scenario stops in a non-interactive session and chooses nothing", {
  local_clean_session()
  p <- local_project()
  local_mocked_bindings(is_interactive = function() return(FALSE))
  expect_error(use_scenario("known"), use_scenario_msg, fixed = TRUE)
  expect_identical(start(p)$scenario, "real")
})

test_that("use_scenario stops on a bad scenario name or simulation", {
  local_clean_session()
  as_person()
  expect_error(use_scenario("Known"), "lower-case letters")
  expect_error(use_scenario(c("a", "b")), "lower-case letters")
  for (bad in list(0, 1.5, NA, "1", c(1, 2))) {
    expect_error(
      use_scenario("known", sim = bad),
      "`sim` must be a whole number of 1 or more.",
      fixed = TRUE,
      info = deparse(bad)
    )
  }
  expect_null(choice$scenario)
})

test_that("end_if_fake_scenario returns invisibly for the real scenario", {
  local_clean_session()
  p <- local_project()
  # As a person: a wrong scenario then stops the test instead of quitting R.
  as_person()
  expect_identical(start(p)$scenario, "real")
  expect_invisible(end_if_fake_scenario())
  expect_null(end_if_fake_scenario())
})

test_that("end_if_fake_scenario in an interactive session stops a fake scenario with its own condition", {
  local_clean_session()
  p <- local_project()
  as_person()
  utils::capture.output(use_scenario("known"))
  start(p)
  err <- tryCatch(end_if_fake_scenario(), error = function(e) return(e))
  expect_s3_class(err, "pbn_end_if_fake_scenario")
  expect_s3_class(err, "error")
  expect_null(conditionCall(err))
  expect_identical(
    conditionMessage(err),
    paste0(
      "The scenario \"known\" is fake, so the run ends here. ",
      "The rest of the script is for the real data only."
    )
  )
  # The session goes on, and the state of the scenario is still there.
  expect_identical(state$scenario, "known")
})

test_that("end_if_fake_scenario before initialize_project stops with an error", {
  local_clean_session()
  expect_error(
    end_if_fake_scenario(),
    "Call pbn::initialize_project() before pbn::end_if_fake_scenario().",
    fixed = TRUE
  )
})

test_that("initialize_project prints which scenario the session is on, in an interactive session only", {
  local_clean_session()
  p <- local_project()
  as_person()
  run <- function() {
    return(utils::capture.output(suppressMessages(initialize_project(
      "v01",
      p$home,
      p$data_default,
      env = new.env()
    ))))
  }
  utils::capture.output(use_scenario("real"))
  expect_identical(run(), "pbn: scenario real, simulation 1 of 1")
  utils::capture.output(use_scenario("known"))
  expect_identical(run(), "pbn: scenario known, simulation 1 of 1 (fake)")
  utils::capture.output(use_scenario("null", sim = 2))
  expect_identical(run(), "pbn: scenario null, simulation 2 of 3 (fake)")

  local_mocked_bindings(is_interactive = function() return(FALSE))
  expect_identical(run(), character(0))
  local_mocked_bindings(command_args = function() return("scenario=known"))
  expect_identical(run(), character(0))
})

test_that("a failed initialize_project leaves the run state empty at each step, and the session choice stays", {
  local_clean_session()
  p <- local_project()
  as_person()
  utils::capture.output(use_scenario("known"))
  # A function that stops, for the step that writes the fake data.
  write_lines(
    p$home,
    "v01_functions/fake_broken.R",
    "fake_broken <- function(dir) stop(\"planted fake failure\")"
  )
  fails <- list(
    version = function() return(initialize_project("V01", p$home, "x")),
    home = function() {
      return(initialize_project("v01", file.path(p$root, "absent"), "x"))
    },
    command_line = function() {
      local_mocked_bindings(command_args = function() return("sim=0"))
      return(start(p))
    },
    data_raw = function() {
      local_mocked_bindings(
        command_args = function() return("scenario=real")
      )
      return(initialize_project("v01", p$home, file.path(p$root, "absent")))
    },
    unknown_scenario = function() {
      local_mocked_bindings(
        command_args = function() return("scenario=absent")
      )
      return(start(p))
    },
    sim_above_sims = function() {
      local_mocked_bindings(
        command_args = function() return(c("scenario=known", "sim=2"))
      )
      return(start(p))
    },
    missing_fake = function() {
      unlink(file.path(p$home, "v01_functions", "fake_known.R"))
      on.exit(write_lines(
        p$home,
        "v01_functions/fake_known.R",
        fake_known_lines()
      ))
      return(start(p))
    },
    fake_stops = function() {
      write_lines(
        p$home,
        "v01_functions/_cfg.R",
        sub("\"null\")", "\"broken\")", cfg_lines(), fixed = TRUE)
      )
      on.exit(write_lines(p$home, "v01_functions/_cfg.R", cfg_lines()))
      local_mocked_bindings(
        command_args = function() return("scenario=broken")
      )
      return(start(p))
    }
  )
  for (step in names(fails)) {
    expect_identical(start(p)$scenario, "known", info = step)
    expect_identical(state$scenario, "known", info = step)
    expect_error(suppressMessages(fails[[step]]()), info = step)
    expect_identical(ls(state, all.names = TRUE), character(0), info = step)
    expect_error(
      save_results(
        exclusions = data.frame(
          step = character(0),
          n_before = numeric(0),
          n_excluded = numeric(0),
          n_after = numeric(0)
        ),
        estimates = data.frame(term = "x", estimate = 1),
        tables = list()
      ),
      "Call pbn::initialize_project() before pbn::save_results().",
      fixed = TRUE,
      info = step
    )
    expect_error(
      end_if_fake_scenario(),
      "Call pbn::initialize_project() before pbn::end_if_fake_scenario().",
      fixed = TRUE,
      info = step
    )
    expect_identical(
      session_choice(),
      list(scenario = "known", sim = 1L),
      info = step
    )
  }
  # A call that succeeds after the failures uses the session choice.
  out <- start(p)
  expect_identical(out$scenario, "known")
  expect_identical(state$scenario, "known")
})

# The tests below start a new R session.

test_that("use_scenario under Rscript stops with the message", {
  skip_if_pbn_not_installed()
  res <- run_lines(c(
    "pbn::use_scenario(\"known\", sim = 1)",
    "cat(\"after the call\\n\")"
  ))
  expect_identical(res$status, 1L)
  expect_match(res$stdout, use_scenario_msg, fixed = TRUE)
  expect_no_match(res$stdout, "after the call", fixed = TRUE)
})

test_that("an active use_scenario line fails under Rscript and fails check_project; a commented one passes both", {
  skip_if_pbn_not_installed()
  p <- local_project()
  file <- file.path(p$home, "v01_run.R")
  commented <- readLines(file)
  active <- commented
  active[3] <- sub("^# ", "", active[3])
  expect_identical(active[3], "pbn::use_scenario(\"known\", sim = 1)")

  writeLines(active, file)
  res <- run_script(p$home)
  expect_identical(res$status, 1L)
  expect_match(res$stdout, use_scenario_msg, fixed = TRUE)
  expect_false(dir.exists(file.path(p$home, "v01_results")))
  expect_identical(
    failed_rules(project_findings(p$home)),
    "no_active_use_scenario"
  )
  check <- callr::rscript(
    file.path(p$home, "check_project.R"),
    wd = p$home,
    show = FALSE,
    fail_on_status = FALSE
  )
  expect_identical(check$status, 1L)
  expect_match(check$stdout, "FAIL  no_active_use_scenario", fixed = TRUE)

  writeLines(commented, file)
  res <- run_script(p$home)
  expect_identical(res$status, 0L, info = res$stdout)
  expect_identical(failed_rules(project_findings(p$home)), character(0))
  check <- callr::rscript(
    file.path(p$home, "check_project.R"),
    wd = p$home,
    show = FALSE,
    fail_on_status = FALSE
  )
  expect_identical(check$status, 0L)
  expect_match(check$stdout, "PASS  no_active_use_scenario", fixed = TRUE)
})

test_that("end_if_fake_scenario under Rscript ends a fake run with status 0 after save_results", {
  skip_if_pbn_not_installed()
  p <- local_project()
  results <- file.path(p$home, "v01_results")
  # output.txt is the file that the script writes after the call.
  for (args in list("scenario=known", c("scenario=null", "sim=2"))) {
    res <- run_script(p$home, args)
    info <- paste(args, collapse = " ")
    expect_identical(res$status, 0L, info = info)
    expect_match(res$stdout, "so the run ends here", fixed = TRUE, info = info)
  }
  expect_true(file.exists(file.path(results, "known", "results.qs2")))
  expect_true(file.exists(file.path(results, "null/sim_0002/results.qs2")))
  expect_identical(
    grep("output\\.txt$", list.files(results, recursive = TRUE), value = TRUE),
    character(0)
  )

  res <- run_script(p$home)
  expect_identical(res$status, 0L, info = res$stdout)
  expect_true(file.exists(file.path(results, "real", "results.qs2")))
  expect_true(file.exists(file.path(results, "real", "output.txt")))
  expect_no_match(res$stdout, "so the run ends here", fixed = TRUE)
  # Rscript is not interactive, so initialize_project() prints no line.
  expect_no_match(res$stdout, "pbn: scenario", fixed = TRUE)
})

test_that("end_if_fake_scenario under Rscript before initialize_project stops with status 1", {
  skip_if_pbn_not_installed()
  res <- run_lines("pbn::end_if_fake_scenario()")
  expect_identical(res$status, 1L)
  expect_match(
    res$stdout,
    "Call pbn::initialize_project() before pbn::end_if_fake_scenario().",
    fixed = TRUE
  )
})

test_that("end_if_fake_scenario in an interactive R process leaves the process alive", {
  skip_if_pbn_not_installed()
  p <- local_project()
  out <- callr::r(
    function(home, data_raw) {
      testthat::local_mocked_bindings(
        is_interactive = function() return(TRUE),
        .package = "pbn"
      )
      utils::capture.output(pbn::use_scenario("known"))
      suppressMessages(pbn::initialize_project(
        "v01",
        home,
        data_raw,
        env = new.env()
      ))
      cls <- tryCatch(
        pbn::end_if_fake_scenario(),
        error = function(e) return(class(e))
      )
      return(list(class = cls, alive = TRUE))
    },
    args = list(home = p$home, data_raw = p$data_default),
    show = FALSE
  )
  expect_identical(
    out,
    list(
      class = c("pbn_end_if_fake_scenario", "error", "condition"),
      alive = TRUE
    )
  )
})
