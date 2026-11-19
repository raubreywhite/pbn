# The display of run_plan() and pbn::progress().

# The scenarios real (1 simulation) and null (3), and one kind with 2 values
# of an extra dimension: 8 analyses that count, and 4 that count the rows.
# `fn` is the action function. It lives in the global environment, as in a
# project.
progress_plan <- function(fn = function(data, argset) return(argset$k)) {
  environment(fn) <- globalenv()
  data_fn <- function(scenario, sim) return(data.frame(x = 1:3))
  environment(data_fn) <- globalenv()
  p <- plnr::Plan$new()
  add_data(p, "d", fn = data_fn)
  sc <- scenarios(list(
    scenarios = data.frame(scenario = c("real", "null"), sims = c(1, 3))
  ))
  add_analyses(p, sc, "a", fn = fn, k = 1:2)
  return(p)
}

# Records each progression that reaches the display, and the counts of the
# display just before the counts of the end of the run replace them.
local_recorder <- function(env = parent.frame()) {
  seen <- new.env()
  seen$p <- list()
  seen$before_final <- NULL
  real_update <- display_update
  local_mocked_bindings(
    display_update = function(d, p) {
      seen$p <- c(seen$p, list(p))
      if (!is.null(p[["pbn_final"]])) {
        seen$before_final <- stats::setNames(d$done, d$scenario)
      }
      return(real_update(d, p))
    },
    .env = env
  )
  return(seen)
}

# The counts of the end of the run that reached the display.
final_of <- function(seen) {
  finals <- Filter(function(p) return(!is.null(p[["pbn_final"]])), seen$p)
  return(lapply(finals, function(p) return(p[["pbn_final"]])))
}

# The texts of pbn::progress() in the progressions `ps`.
detail_texts <- function(ps) {
  texts <- lapply(ps, function(p) return(p[["pbn_detail"]][["text"]]))
  return(unlist(texts))
}

# The display rules with a log that shows each update at once.
local_eager_log <- function(env = parent.frame()) {
  local_mocked_bindings(
    display_rules = function() {
      return(list(quiet = 0, beat_seconds = 0, beat_share = 0.1, redraw = 0))
    },
    .env = env
  )
  return(invisible(NULL))
}

test_that("the display gets each analysis once, with 1 and 2 workers, and not the analyses that count the rows", {
  skip_if_pbn_not_installed()
  local_clean_state()
  local_eager_log()
  for (workers in 1:2) {
    seen <- local_recorder()
    p <- progress_plan()
    out <- utils::capture.output(r <- run_plan(p, workers = workers))
    done <- unlist(lapply(seen$p, function(x) return(x[["pbn_done"]])))
    # Each count reached the display while the run ran, before the counts
    # of the end.
    expect_identical(
      vapply(c("real", "null"), function(s) sum(done[names(done) == s]), 1),
      c(real = 2, null = 6),
      info = paste("workers:", workers)
    )
    expect_identical(seen$before_final, c(real = 2, null = 6))
    expect_identical(
      final_of(seen),
      list(list(
        done = c(real = 2L, null = 6L),
        failed = c(real = 0L, null = 0L),
        not_run = c(real = 0L, null = 0L)
      )),
      info = paste("workers:", workers)
    )
    header <- grep("^pbn: [0-9]+ scenarios", out, value = TRUE)
    expect_length(header, 1)
    expect_match(
      header,
      paste0(
        "^pbn: 2 scenarios, ",
        workers,
        if (workers == 1) " worker" else " workers",
        ", started [0-9]{2}:[0-9]{2}$"
      ),
      info = paste("workers:", workers)
    )
    # Each line starts with the time since the start of the run.
    done_lines <- grep("done in", out, value = TRUE)
    expect_match(done_lines, "^pbn: \\[[0-9]+s\\] ")
    expect_identical(
      sub(" in [0-9]+s$", "", sub("^pbn: \\[[0-9]+s\\] ", "pbn: ", done_lines)),
      c("pbn: real 2/2 analyses, done", "pbn: null 6/6 analyses, done"),
      info = paste("workers:", workers)
    )
    expect_length(r, 12)
  }
})

test_that("in a log, a short run shows nothing, so the output of the tests stays as it was", {
  local_clean_state()
  out <- utils::capture.output(run_plan(progress_plan()))
  expect_identical(out, character(0))
})

test_that("pbn::progress() in an analysis reaches the line of its scenario, from this R session and from a worker, and the count of the analyses does not change", {
  skip_if_pbn_not_installed()
  local_clean_state()
  boot <- function(data, argset) {
    if (argset$scenario == "real" && argset$k == 1) {
      for (b in 1:4) {
        Sys.sleep(0.3)
        pbn::progress(b, 4, "boot")
      }
    }
    return(argset$k)
  }
  for (workers in 1:2) {
    seen <- local_recorder()
    run_plan(progress_plan(boot), workers = workers)
    details <- Filter(function(p) return(!is.null(p[["pbn_detail"]])), seen$p)
    texts <- detail_texts(details)
    # The first call shows, and a call 0.5 seconds after the last shown one.
    expect_gte(length(texts), 2)
    expect_true(
      all(grepl("^a \\(1\\) boot [1-4]/4 [0-9]+%$", texts)),
      info = paste("workers:", workers)
    )
    expect_identical(
      texts[1],
      "a (1) boot 1/4 25%",
      info = paste("workers:", workers)
    )
    expect_identical(
      unique(vapply(details, function(p) return(p$pbn_detail$scenario), "")),
      "real"
    )
    # A text of pbn::progress() is no count.
    expect_true(all(vapply(
      details,
      function(p) {
        return(is.null(p[["pbn_done"]]))
      },
      logical(1)
    )))
    # The end of the analysis ends its text.
    ended <- unlist(lapply(seen$p, function(p) return(p[["pbn_ended"]])))
    expect_identical(ended, c(real = details[[1]]$pbn_detail$key))
    expect_identical(seen$before_final, c(real = 2, null = 6))
  }
})

test_that("pbn::progress() does nothing in a forked child, and the parent loop between chunks reaches the display, with 1 and 2 workers", {
  skip_on_os("windows")
  skip_if_pbn_not_installed()
  local_clean_state()
  forked <- function(data, argset) {
    if (argset$scenario == "real" && argset$k == 1) {
      parallel::mclapply(
        1:2,
        function(i) {
          pbn::progress(i, 2, "in a fork")
          return(i)
        },
        mc.cores = 2
      )
      for (chunk in list(1:2, 3:4)) {
        parallel::mclapply(chunk, function(i) return(i), mc.cores = 2)
        pbn::progress(max(chunk), 4, "chunks")
        Sys.sleep(0.6)
      }
    }
    return(argset$k)
  }
  for (workers in 1:2) {
    seen <- local_recorder()
    run_plan(progress_plan(forked), workers = workers)
    texts <- detail_texts(seen$p)
    expect_false(
      any(grepl("in a fork", texts, fixed = TRUE)),
      info = paste("workers:", workers)
    )
    expect_identical(
      texts,
      c("a (1) chunks 2/4 50%", "a (1) chunks 4/4 100%"),
      info = paste("workers:", workers)
    )
  }
})

test_that("pbn::progress() checks its arguments at each call, also outside run_plan() and within 0.5 seconds of a call that it showed, and otherwise does nothing outside run_plan()", {
  expect_null(getOption("pbn.run"))
  expect_null(progress(1, 10))
  expect_error(
    progress("not a number", -1, 3),
    "needs `done`, one number of 0 or more"
  )
  expect_error(progress(NA, 10), "needs `done`, one number of 0 or more")
  run <- new.env()
  run$cur_argset <- NULL
  withr::local_options(pbn.run = run)
  expect_null(progress(1, 10))
  run$cur_argset <- list(
    scenario = "null",
    sim = 2L,
    analysis = "a",
    k = 1L,
    index_analysis = 7L
  )
  run$cur_start <- proc.time()[["elapsed"]] - 10
  run$cur_wrote <- FALSE
  run$cur_last <- -Inf
  run$here <- Sys.getpid()
  run$dims <- list(a = "k")
  run$sims <- c(null = 3L)
  sent <- list()
  run$progressor <- function(...) {
    sent[[length(sent) + 1]] <<- list(...)
    return(invisible(NULL))
  }
  expect_error(progress("1", 10), "needs `done`, one number of 0 or more")
  expect_error(progress(1, 10, label = 2), "must be one string")
  progress(5, 20, "boot")
  # A second call within 0.5 seconds is dropped, but its arguments are
  # checked.
  progress(6, 20, "boot")
  expect_length(sent, 1)
  expect_error(progress(NA, 20), "needs `done`, one number of 0 or more")
  expect_length(sent, 1)
  detail <- sent[[1]]$pbn_detail
  expect_identical(sent[[1]]$amount, 0)
  expect_identical(detail$text, "a (1, sim 2) boot 5/20 25%")
  expect_identical(detail$key, "7")
  expect_identical(detail$scenario, "null")
  # 10 seconds for 5 of 20 steps: 30 seconds left.
  expect_equal(detail$left, 30, tolerance = 0.01)
  # Another R session, as in a forked child, shows nothing.
  run$cur_wrote <- FALSE
  run$here <- -1L
  progress(7, 20)
  expect_length(sent, 1)
})

test_that("progress_left gives no time left before 3 steps and 10% of the steps, and none after the last step", {
  expect_identical(progress_left(10, 2, 10), NA_real_)
  expect_identical(progress_left(10, 3, 100), NA_real_)
  expect_identical(progress_left(10, 3, 30), 90)
  expect_identical(progress_left(10, 10, 10), NA_real_)
})

test_that("a log shows nothing for 60 seconds, then the header and one line for each started scenario, then a line at most every 60 seconds and 10%, and one line at the end of each scenario", {
  clock <- new.env()
  clock$now <- 0
  local_mocked_bindings(display_now = function() return(clock$now))
  d <- new_display(
    data.frame(scenario = c("real", "null"), analyses = c(1, 40)),
    2,
    as.POSIXct("2026-11-23 14:02:00"),
    "log"
  )
  show <- function(p, at) {
    clock$now <- at
    return(utils::capture.output(display_update(d, p)))
  }
  expect_identical(
    show(list(pbn_done = c(null = 4), pbn_t0 = 0), 10),
    character(0)
  )
  expect_identical(
    show(list(pbn_done = c(real = 1), pbn_t0 = 0), 50),
    character(0)
  )
  expect_identical(
    show(list(pbn_done = c(null = 4)), 61),
    c(
      "pbn: 2 scenarios, 2 workers, started 14:02",
      "pbn: [50s] real 1/1 analyses, done in 50s",
      "pbn: [1m 01s] null 8/40 analyses, 1m 01s, about 5m left"
    )
  )
  # 10% more, but within 60 seconds: no line.
  expect_identical(show(list(pbn_done = c(null = 8)), 100), character(0))
  # 60 seconds later, with 10% more: one line.
  expect_identical(
    show(list(pbn_done = c(null = 1)), 121),
    "pbn: [2m 01s] null 17/40 analyses, 2m 01s, about 3m left"
  )
  # 60 seconds later, but less than 10% more: no line.
  expect_identical(show(list(pbn_done = c(null = 3)), 200), character(0))
  # A text of pbn::progress() that moved 10% counts too.
  detail <- list(
    scenario = "null",
    key = "9",
    text = "a (1) boot 30/100 30%",
    share = 0.3,
    t0 = 190,
    left = NA_real_
  )
  expect_identical(
    show(list(pbn_detail = detail), 260),
    "pbn: [4m 20s] null 20/40 analyses, 4m 20s, a (1) boot 30/100 30%"
  )
  expect_identical(
    show(list(pbn_done = c(null = 20), pbn_ended = c(null = "9")), 300),
    "pbn: [5m 00s] null 40/40 analyses, done in 5m 00s"
  )
  # A scenario gets its last line once.
  expect_identical(show(list(pbn_done = c(null = 0)), 400), character(0))
})

test_that("a line of a log carries the time of its event: a scenario that finished in the quiet seconds gets the time of its end, and the lines come in the order of these times", {
  clock <- new.env()
  clock$now <- 0
  local_mocked_bindings(display_now = function() return(clock$now))
  d <- new_display(
    data.frame(scenario = c("null", "known", "real"), analyses = c(40, 2, 1)),
    2,
    as.POSIXct("2026-11-23 14:02:00"),
    "log"
  )
  show <- function(p, at) {
    clock$now <- at
    return(utils::capture.output(display_update(d, p)))
  }
  expect_identical(
    show(list(pbn_done = c(null = 4), pbn_t0 = 0), 2),
    character(0)
  )
  expect_identical(
    show(list(pbn_done = c(known = 2), pbn_t0 = 1), 9),
    character(0)
  )
  expect_identical(
    show(list(pbn_done = c(real = 1), pbn_t0 = 0), 3),
    character(0)
  )
  expect_identical(
    show(list(pbn_done = c(null = 4)), 75),
    c(
      "pbn: 3 scenarios, 2 workers, started 14:02",
      "pbn: [3s] real 1/1 analyses, done in 3s",
      "pbn: [9s] known 2/2 analyses, done in 8s",
      "pbn: [1m 15s] null 8/40 analyses, 1m 15s, about 5m left"
    )
  )
})

test_that("in a terminal, the display draws its lines again in place, and hides them for other output", {
  clock <- new.env()
  clock$now <- 0
  local_mocked_bindings(display_now = function() return(clock$now))
  d <- new_display(
    data.frame(scenario = c("real", "null"), analyses = c(1, 40)),
    2,
    as.POSIXct("2026-11-23 14:02:00"),
    "ansi"
  )
  d$stream <- "stdout"
  first <- utils::capture.output(display_show(d, 0, force = TRUE))
  expect_identical(
    first,
    c(
      "\033[2Kpbn: 2 scenarios, 2 workers, started 14:02, 0s",
      "\033[2K  real   0/1  analyses           waiting",
      "\033[2K  null   0/40 analyses           waiting"
    )
  )
  clock$now <- 372
  second <- utils::capture.output(display_update(
    d,
    list(pbn_done = c(null = 12), pbn_t0 = 0)
  ))
  # The cursor goes up the 3 lines, and each line is cleared and drawn.
  expect_identical(
    second,
    c(
      "\033[3A\r\033[2Kpbn: 2 scenarios, 2 workers, started 14:02, 6m 12s",
      "\033[2K  real   0/1  analyses           waiting",
      "\033[2K  null  12/40 analyses   6m 12s  about 15m left"
    )
  )
  # Within 0.2 seconds no new drawing, unless a scenario finishes.
  clock$now <- 372.1
  expect_identical(
    utils::capture.output(display_update(d, list(pbn_done = c(null = 1)))),
    character(0)
  )
  expect_identical(
    utils::capture.output(display_hide(d)),
    "\033[3A\r\033[J"
  )
  expect_identical(d$drawn, 0)
})

test_that("the note of a scenario gives no time left for a scenario with one analysis, and the text of pbn::progress() with its own time left", {
  clock <- new.env()
  clock$now <- 100
  local_mocked_bindings(display_now = function() return(clock$now))
  d <- new_display(
    data.frame(scenario = c("real", "null"), analyses = c(1, 40)),
    1,
    Sys.time(),
    "log"
  )
  d$start[1] <- 0
  expect_identical(scenario_note(d, 1, 100), "")
  d$details[[1]] <- list(
    "1" = list(
      key = "1",
      text = "sem (ppv6) 1240/2000 62%",
      at = 90,
      left = 70,
      share = 0.62
    )
  )
  expect_identical(
    scenario_note(d, 1, 100),
    "sem (ppv6) 1240/2000 62%, about 1m left"
  )
  d$done[2] <- 3
  d$start[2] <- 40
  expect_identical(scenario_note(d, 2, 100), "")
  d$done[2] <- 4
  expect_identical(scenario_note(d, 2, 100), "about 9m left")
  expect_identical(format_duration(372), "6m 12s")
  expect_identical(format_duration(3725), "1h 02m")
  expect_identical(format_left(25), "30s")
  expect_identical(format_left(3725), "1h 03m")
})

test_that("run_plan sets back the future plan, the option of doFuture and the foreach backend, also after a failed run, and leaves the progressr handlers of the session alone", {
  skip_if_pbn_not_installed()
  local_clean_state()
  seen <- new.env()
  seen$n <- 0
  # A handler that a package sets for itself, as cs9 does in its .onLoad.
  other <- progressr::make_progression_handler(
    "other",
    list(
      initiate = function(...) seen$n <- seen$n + 1,
      update = function(...) seen$n <- seen$n + 1
    ),
    enable = TRUE
  )
  old <- progressr::handlers(other)
  withr::defer(progressr::handlers(old))
  withr::local_options(progressr.enable = TRUE)
  before <- future::plan()
  rng <- getOption("doFuture.rng.onMisuse")
  r <- run_plan(progress_plan(), workers = 2)
  expect_identical(future::plan(), before)
  expect_identical(getOption("doFuture.rng.onMisuse"), rng)
  expect_identical(foreach::getDoParName(), "doSEQ")
  bad <- function(data, argset) stop("planted")
  expect_error(
    utils::capture.output(run_plan(progress_plan(bad), workers = 2)),
    "analyses failed"
  )
  expect_identical(future::plan(), before)
  expect_identical(getOption("doFuture.rng.onMisuse"), rng)
  expect_identical(foreach::getDoParName(), "doSEQ")
  expect_identical(progressr::handlers(), list(other))
  expect_identical(seen$n, 0)
  expect_null(getOption("pbn.run"))
})

test_that("in a session with global progressr handlers, run_plan shows only its own display and leaves the global handlers on", {
  skip_if_pbn_not_installed()
  root <- withr::local_tempdir()
  script <- file.path(root, "global.R")
  writeLines(
    c(
      "progressr::handlers(global = TRUE)",
      "progressr::handlers(progressr::handler_txtprogressbar())",
      "CFG <- list(scenarios = data.frame(scenario = c('real', 'null'), sims = c(1, 2)))",
      "p <- plnr::Plan$new()",
      "pbn::add_data(p, 'd', fn = function(scenario, sim) return(data.frame(x = 1)))",
      "pbn::add_analyses(p, pbn::scenarios(CFG), 'a', fn = function(data, argset) return(1))",
      "r <- pbn::run_plan(p, workers = 2)",
      "cat('global:', progressr::handlers(global = NA), '\\n')",
      "cat('results:', length(r), '\\n')"
    ),
    script
  )
  res <- run_script(root, "global.R")
  expect_identical(res$status, 0L, info = res$stdout)
  expect_identical(
    strsplit(res$stdout, "\n", fixed = TRUE)[[1]],
    c("global: TRUE ", "results: 6 ")
  )
})

test_that("the display replaces the progress of plnr: a verbose plan sets no progressr handler, and gets its own progressor back", {
  skip_if_pbn_not_installed()
  local_clean_state()
  before <- progressr::handlers()
  withr::defer(progressr::handlers(before))
  p <- progress_plan()
  # A verbose plan without a progressor sets the progressr handlers of the
  # session in the foreach path of plnr.
  p$set_verbose(TRUE)
  run_plan(p, workers = 2)
  expect_identical(progressr::handlers(), before)
  expect_null(plan_progressor(p))
  mine <- function(...) return(invisible(NULL))
  p$set_progressor(mine)
  run_plan(p)
  expect_identical(plan_progressor(p), mine)
})

test_that("in a console without ANSI codes, the display draws one line again in place, and the lines of each scenario at the end", {
  clock <- new.env()
  clock$now <- 0
  local_mocked_bindings(display_now = function() return(clock$now))
  withr::local_options(width = 60)
  d <- new_display(
    data.frame(scenario = c("real", "null"), analyses = c(1, 40)),
    1,
    as.POSIXct("2026-11-23 14:02:00"),
    "line"
  )
  d$stream <- "stdout"
  clock$now <- 75
  shown <- paste(
    utils::capture.output(display_update(
      d,
      list(pbn_done = c(null = 4), pbn_t0 = 0)
    )),
    collapse = "\n"
  )
  # A carriage return, then the line padded to the width less 1.
  expect_identical(
    shown,
    paste0("\r", formatC("pbn: 1m 15s | real 0/1 | null 4/40", width = -59))
  )
  clock$now <- 80
  end <- utils::capture.output(display_finish(d))
  expect_identical(
    end,
    c(
      paste0(
        "\r",
        strrep(" ", 59),
        "\rpbn: 2 scenarios, 1 worker, started 14:02, 1m 20s"
      ),
      "  real   0/1  analyses           waiting",
      "  null   4/40 analyses   1m 20s  about 12m left"
    )
  )
})

test_that("a wrong argument of pbn::progress() fails its analysis, also right after a call that it showed", {
  local_clean_state()
  fast <- function(data, argset) {
    pbn::progress(1, 10)
    pbn::progress(NA, 10)
    return(1)
  }
  out <- utils::capture.output(
    err <- tryCatch(run_plan(progress_plan(fast)), error = function(e) e)
  )
  expect_match(
    conditionMessage(err),
    "^1 of 12 analyses failed, and the run stopped early: 7 analyses did not run\\. "
  )
  expect_match(
    paste(out, collapse = "\n"),
    "pbn::progress() needs `done`, one number of 0 or more",
    fixed = TRUE
  )
})

test_that("the display counts failed analyses: a line shows them, a finished scenario with failures says so with its time, and the time left counts only the analyses without a failure", {
  clock <- new.env()
  clock$now <- 0
  local_mocked_bindings(display_now = function() return(clock$now))
  local_eager_log()
  d <- new_display(
    data.frame(scenario = c("real", "null"), analyses = c(11, 40)),
    1,
    as.POSIXct("2026-11-23 14:02:00"),
    "log"
  )
  show <- function(p, at) {
    clock$now <- at
    return(utils::capture.output(display_update(d, p)))
  }
  # Each analysis of real fails after its data failed.
  out <- show(
    list(pbn_done = c(real = 11), pbn_failed = c(real = 11), pbn_t0 = 0),
    2
  )
  expect_identical(
    out[length(out)],
    "pbn: [2s] real 11/11 analyses, 11 failed, done in 2s"
  )
  # 8 of null ran in 100 seconds, and 4 of them failed. The time left comes
  # from the 4 without a failure: 100 * 32 / 4 seconds, 14 minutes. With
  # the failed ones as done, it would be 100 * 32 / 8 seconds, 7 minutes.
  out <- show(
    list(pbn_done = c(null = 8), pbn_failed = c(null = 4), pbn_t0 = 0),
    100
  )
  expect_identical(
    out,
    "pbn: [1m 40s] null 8/40 analyses, 4 failed, 1m 40s, about 14m left"
  )
  # In a terminal, the count column holds the failures, padded to the
  # widest count.
  expect_identical(
    display_lines(d, 100)[2:3],
    c(
      "  real  11/11 analyses, 11 failed       2s  done",
      "  null   8/40 analyses, 4 failed    1m 40s  about 14m left"
    )
  )
})

test_that("the time of a scenario whose data failed is not blank: its batch starts at its data, or now", {
  sent <- new.env()
  sent$p <- list()
  index <- data.frame(
    scenario = "real",
    sim = 1L,
    hidden = FALSE,
    analysis = "a"
  )
  run <- run_context(
    list(dims = list(), data = list()),
    index,
    1,
    function(...) {
      sent$p <- c(sent$p, list(list(...)))
      return(invisible(NULL))
    },
    tempfile()
  )
  # The data of the analysis failed, so no analysis began: no t0 of the
  # data, and no start of the analysis.
  run$t0 <- NULL
  run_done(run, list(scenario = "real", index_analysis = 1L), 5, failed = TRUE)
  expect_length(sent$p, 1)
  expect_false(is.na(sent$p[[1]]$pbn_t0))
  expect_identical(sent$p[[1]]$pbn_failed, c(real = 1))
  expect_identical(sent$p[[1]]$pbn_done, c(real = 1))
})

test_that("after the stop of the run, the display says so at once in a log, and each scenario ends with the analyses that did not run", {
  clock <- new.env()
  clock$now <- 0
  local_mocked_bindings(display_now = function() return(clock$now))
  d <- new_display(
    data.frame(scenario = c("real", "null"), analyses = c(1, 40)),
    2,
    as.POSIXct("2026-11-23 14:02:00"),
    "log"
  )
  clock$now <- 3
  # In the first 60 seconds of a log, only the line of the stop.
  expect_identical(
    utils::capture.output(display_update(d, list(pbn_stop = "real"))),
    "pbn: [3s] an analysis of the scenario `real` failed, so the run stops early: no new analysis starts."
  )
  expect_identical(
    utils::capture.output(display_update(d, list(pbn_stop = "null"))),
    character(0)
  )
  expect_identical(scenario_note(d, 2, 3), "stopping")
  display_final(
    d,
    list(
      done = c(real = 1L, null = 0L),
      failed = c(real = 1L, null = 0L),
      not_run = c(real = 0L, null = 40L)
    ),
    4
  )
  expect_identical(
    log_line(d, 1, 4),
    "pbn: [4s] real 1/1 analyses, 1 failed, done"
  )
  expect_identical(
    log_line(d, 2, 4),
    "pbn: [4s] null 0/40 analyses, stopped early: 40 did not run"
  )
  expect_match(
    display_lines(d, 4)[1],
    ", 4s, stopped early: an analysis failed$"
  )
})
